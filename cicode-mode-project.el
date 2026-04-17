;;; cicode-mode-project.el --- Project indexing for cicode-mode

;; Copyright (C) 2025 Sebastian Gazey

;; Scans Cicode project files for FUNCTION definitions and their doxygen
;; doc comments.  Provides completion, xref (go-to-def / go-to-refs),
;; imenu, and eldoc for user-defined functions -- using the same doc
;; display infrastructure as the built-in functions.
;;
;; Results are cached to `.cicode-cache.json` with per-file content
;; hashing so only changed files are re-scanned.
;;
;; Usage:
;;   (require 'cicode-mode-project)      ; auto-hooks into cicode-mode
;;   M-x cicode-project-scan             ; index / re-index the project
;;
;; Place a `.cicode-project` marker file at your project root, or set
;; `cicode-project-root` directly.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'xref)

;; Forward declarations
(defvar cicode-hash-table-completion)
(declare-function cicode-mode-load-functions "cicode-mode")

;;;; Customisation ============================================================

(defgroup cicode-project nil
  "Project-level indexing for Cicode."
  :group 'programming
  :prefix "cicode-project-")

(defcustom cicode-project-root nil
  "Root directory of the Cicode project.
When nil, searches upward for a `.cicode-project' marker file,
then falls back to `project-current', then `default-directory'."
  :type '(choice (const :tag "Auto-detect" nil) directory))

(defcustom cicode-project-cache-file ".cicode-cache.json"
  "Cache filename, relative to project root."
  :type 'string)

(defcustom cicode-project-scan-on-open nil
  "When non-nil, scan the whole project when the first .ci buffer is opened."
  :type 'boolean)

;;;; Internal state ===========================================================

(defvar cicode-project--file-hashes (make-hash-table :test 'equal)
  "File-path -> MD5 of file contents at last scan.")

(defvar cicode-project--file-functions (make-hash-table :test 'equal)
  "File-path -> (list of function-name strings) defined in that file.")

(defvar cicode-project--all-names nil
  "All function names added to the completion table by project scanning.")

(defvar cicode-project--references (make-hash-table :test 'equal)
  "Downcased function-name -> list of plists (:file :line :col :text).")

(defvar cicode-project--file-references (make-hash-table :test 'equal)
  "File-path -> list of reference plists for cache serialisation.")

(defvar cicode-project--names-hash nil
  "MD5 of sorted project function names.  Used to detect when the set
of functions changes, which invalidates the reference index.")

;;;; Project root =============================================================

(defun cicode-project--root ()
  "Return the project root directory."
  (or cicode-project-root
      (locate-dominating-file default-directory ".cicode-project")
      (and (fboundp 'project-current)
           (when-let ((proj (project-current)))
             (project-root proj)))
      default-directory))

(defun cicode-project--cache-path ()
  "Absolute path to the cache file."
  (expand-file-name cicode-project-cache-file (cicode-project--root)))

;;;; Utilities ================================================================

(defun cicode-project--find-ci-files (root)
  "Return all .ci files under ROOT, skipping unreadable directories."
  (nreverse (cicode-project--find-ci-files-1 (expand-file-name root) nil)))

(defun cicode-project--find-ci-files-1 (dir acc)
  "Collect .ci files under DIR into ACC, skipping unreadable directories."
  (condition-case nil
      (let ((entries (directory-files dir t nil t)))
        (dolist (entry entries)
          (let ((name (file-name-nondirectory entry)))
            (unless (member name '("." ".."))
              (cond
               ((and (not (file-directory-p entry))
                     (string-match-p "\\.ci\\'" name))
                (push entry acc))
               ((file-directory-p entry)
                (setq acc (cicode-project--find-ci-files-1 entry acc)))))))
        acc)
    (error acc)))

(defun cicode-project--hash-file (file)
  "Return MD5 hash of FILE contents, or nil if unreadable."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (md5 (buffer-string)))
    (error nil)))

;;;; Doxygen comment parser ===================================================

(defun cicode-project--parse-doxygen-above ()
  "Parse the /** ... */ doxygen block above point.
Returns plist (:brief :params :return :example :name) or nil.
Multi-line content (lines without a new @tag) is appended to the
most recently opened tag with a newline separator."
  (save-excursion
    (forward-line -1)
    ;; Skip blank lines between comment and FUNCTION
    (while (and (not (bobp)) (looking-at-p "^[ \t]*$"))
      (forward-line -1))
    ;; Must be on closing */
    (when (looking-at-p "^[ \t]*\\*/")
      (let (brief params ret example doc-name
                  (current-tag nil)
                  (current-param-name nil))
        ;; Walk up to opening /**
        (while (and (not (bobp)) (not (looking-at-p "^[ \t]*/\\*\\*")))
          (forward-line -1))
        (when (looking-at-p "^[ \t]*/\\*\\*")
          (forward-line 1)
          (while (not (looking-at-p "^[ \t]*\\*/"))
            (let* ((raw (string-trim (thing-at-point 'line t)))
                   ;; Strip leading " * "
                   (line (if (string-match "^\\*[ \t]?" raw)
                             (substring raw (match-end 0))
                           raw)))
              (cond
               ;; @brief
               ((string-match "^@brief[ \t]+\\(.*\\)" line)
                (setq brief (string-trim (match-string 1 line))
                      current-tag 'brief))
               ;; @param
               ((string-match "^@param[ \t]+\\(\\S-+\\)[ \t]+\\(.*\\)" line)
                (let ((pname (match-string 1 line))
                      (pdesc (string-trim (match-string 2 line))))
                  (push (cons pname pdesc) params)
                  (setq current-tag 'param
                        current-param-name pname)))
               ;; @return / @returns
               ((string-match "^@returns?[ \t]+\\(.*\\)" line)
                (setq ret (string-trim (match-string 1 line))
                      current-tag 'return))
               ;; @example
               ((string-match "^@example[ \t]*\\(.*\\)" line)
                (setq example (string-trim (match-string 1 line))
                      current-tag 'example))
               ;; @name
               ((string-match "^@name[ \t]+\\(.*\\)" line)
                (setq doc-name (string-trim (match-string 1 line))
                      current-tag nil))
               ;; Any other @tag resets continuation
               ((string-match "^@" line)
                (setq current-tag nil))
               ;; Continuation line -- append to current tag
               ((and current-tag
                     (not (string-empty-p (string-trim line))))
                (let ((text (string-trim line)))
                  (pcase current-tag
                    ('brief
                     (setq brief (concat brief "\n" text)))
                    ('param
                     (when-let ((cell (assoc current-param-name params
                                             #'string-equal)))
                       (setcdr cell (concat (cdr cell) "\n" text))))
                    ('return
                     (setq ret (concat ret "\n" text)))
                    ('example
                     (setq example
                           (concat (or example "") "\n" text))))))))
            (forward-line 1))
          (list :brief   (or brief "")
                :params  (nreverse params)
                :return  (or ret "")
                :example (or example "")
                :name    doc-name))))))

;;;; Trailing doc comment parser ==============================================

(defun cicode-project--parse-trailing-doc (line)
  "Extract the /**< ... */ trailing doc comment from LINE.
Returns the doc string or nil."
  (when (string-match "/\\*\\*<\\s-*\\(.*?\\)\\s-*\\*/" line)
    (match-string 1 line)))

;;;; Variable declaration parser =============================================

(defvar cicode-project--var-re
  (concat "^[ \t]*"
          "\\(GLOBAL\\|MODULE\\)[ \t]+"
          "\\(INT\\|STRING\\|FLOAT\\|REAL\\|BOOL\\|LONG"
          "\\|OBJECT\\|TIMESTAMP\\|QUALITY\\)[ \t]+"
          "\\(\\w+\\)")
  "Regexp matching a GLOBAL or MODULE variable declaration.")

(defun cicode-project--parse-variable-line (line)
  "Parse a variable declaration LINE.
Return plist (:name :scope :type :init) or nil.
:init is the initializer expression if present."
  (let ((case-fold-search t))
    (when (string-match
           (concat
            "^[ \t]*"
            "\\(GLOBAL\\|MODULE\\)[ \t]+"
            "\\(INT\\|STRING\\|FLOAT\\|REAL\\|BOOL\\|LONG"
            "\\|OBJECT\\|TIMESTAMP\\|QUALITY\\)[ \t]+"
            "\\(\\w+\\)"
            "\\(?:[ \t]*=[ \t]*\\([^;]*[^ \t;]\\)\\)?")
           line)
      (list :name  (match-string 3 line)
            :scope (string-trim (match-string 1 line))
            :type  (string-trim (match-string 2 line))
            :init  (when (match-string 4 line)
                     (string-trim (match-string 4 line)))))))

(defun cicode-project--make-var-entry (var doc file line-num)
  "Build a hash-table entry for variable VAR.
DOC: trailing doc string or nil.
FILE: source path.  LINE-NUM: 1-based line number."
  (let* ((name  (plist-get var :name))
         (vtype (plist-get var :type))
         (scope (plist-get var :scope))
         (init  (plist-get var :init))
         (ht (make-hash-table :test 'equal :size 12)))
    (puthash "name"       name ht)
    (puthash "doc"        (or doc "") ht)
    (puthash "returnType" vtype ht)
    (puthash "returnDesc" vtype ht)
    (puthash "syntax"     (format "%s %s %s" scope vtype name) ht)
    (puthash "example"    "" ht)
    (puthash "file"       file ht)
    (puthash "line"       line-num ht)
    (puthash "scope"      scope ht)
    (puthash "kind"       "variable" ht)
    (when init
      (puthash "init" init ht))
    (puthash "params"     '() ht)
    ht))

;;;; FUNCTION line parser =====================================================

(defvar cicode-project--func-re
  (concat "^[ \t]*"
          "\\(?:\\(?:GLOBAL\\|PRIVATE\\|PUBLIC\\|MODULE\\)[ \t]+\\)?"
          "\\(?:\\(?:INT\\|STRING\\|FLOAT\\|REAL\\|BOOL\\|VOID\\|LONG"
          "\\|OBJECT\\|TIMESTAMP\\|QUALITY\\)[ \t]+\\)?"
          "FUNCTION[ \t]+\\(\\w+\\)")
  "Regexp matching the start of a Cicode FUNCTION definition.")

(defun cicode-project--split-params (raw)
  "Split RAW parameter string on commas, respecting quoted strings.
Returns a list of individual parameter strings."
  (let ((result '()) (current "") (in-string nil) (i 0) (len (length raw)))
    (while (< i len)
      (let ((ch (aref raw i)))
        (cond
         ;; Toggle string state (handle ^" escape)
         ((and (= ch ?\") (not (and (> i 0) (= (aref raw (1- i)) ?^))))
          (setq in-string (not in-string))
          (setq current (concat current (string ch))))
         ;; Comma outside a string = split point
         ((and (= ch ?,) (not in-string))
          (push current result)
          (setq current ""))
         (t (setq current (concat current (string ch))))))
      (setq i (1+ i)))
    (push current result)
    (nreverse result)))

(defun cicode-project--parse-function-line (line)
  "Parse a FUNCTION LINE.
Return plist (:name :scope :rtype :syntax :params) or nil.
Each param is (:name NAME :type TYPE :default DEFAULT-OR-NIL)."
  (let ((case-fold-search t))
    (when (string-match
           (concat
            "^[ \t]*"
            "\\(\\(?:GLOBAL\\|PRIVATE\\|PUBLIC\\|MODULE\\)[ \t]+\\)?"
            "\\(\\(?:INT\\|STRING\\|FLOAT\\|REAL\\|BOOL\\|VOID\\|LONG"
            "\\|OBJECT\\|TIMESTAMP\\|QUALITY\\)[ \t]+\\)?"
            "FUNCTION[ \t]+\\(\\w+\\)[ \t]*(\\(.*\\))")
           line)
      (let* ((scope (if (match-string 1 line)
                        (string-trim (match-string 1 line))
                      "PUBLIC"))
             (rtype (if (match-string 2 line)
                        (string-trim (match-string 2 line))
                      "VOID"))
             (name  (match-string 3 line))
             (raw   (match-string 4 line))
             ;; Strip trailing )
             (raw   (if (string-suffix-p ")" raw)
                        (substring raw 0 -1)
                      raw))
             (params
              (when (and raw (not (string-blank-p raw)))
                (delq nil
                      (mapcar
                       (lambda (p)
                         (let ((case-fold-search t)
                               (trimmed (string-trim p)))
                           (when (string-match
                                  "^\\(\\w+\\)[ \t]+\\(\\w+\\)\\(?:[ \t]*=[ \t]*\\(.*\\)\\)?$"
                                  trimmed)
                             (list :name    (match-string 2 trimmed)
                                   :type    (match-string 1 trimmed)
                                   :default (match-string 3 trimmed)))))
                       (cicode-project--split-params raw))))))
        (list :name name
              :scope scope
              :rtype rtype
              :syntax (string-trim line)
              :params params)))))

;;;; Build completion entry ===================================================

(defun cicode-project--make-entry (func doc file line-num)
  "Build a hash-table entry matching the builtin format.
FUNC: plist from `cicode-project--parse-function-line'.
DOC:  plist from `cicode-project--parse-doxygen-above' (or nil).
FILE: source path.  LINE-NUM: 1-based line number."
  (let* ((func-name (plist-get func :name))
         (rtype     (plist-get func :rtype))
         (brief     (or (plist-get doc :brief) ""))
         (ret-d     (or (plist-get doc :return) ""))
         (ret-str   (if (string-empty-p ret-d) rtype
                      (format "%s - %s" rtype ret-d)))
         (example   (or (plist-get doc :example) ""))
         (doc-name  (plist-get doc :name))
         (doc-params (or (plist-get doc :params) '()))
         (ht (make-hash-table :test 'equal :size 12)))
    (puthash "name"       (or doc-name func-name) ht)
    (puthash "doc"        brief ht)
    (puthash "returns" ret-d ht)
    (puthash "returnType" rtype ht)
    (puthash "returnDesc" ret-str ht)
    (puthash "syntax"     (plist-get func :syntax) ht)
    (puthash "example"    example ht)
    (puthash "file"       file ht)
    (puthash "line"       line-num ht)
    (puthash "scope"      (plist-get func :scope) ht)
    (puthash "params"
             (mapcar
              (lambda (fp)
                (let* ((pname   (plist-get fp :name))
                       (ptype   (plist-get fp :type))
                       (default (plist-get fp :default))
                       (ddesc   (cdr (assoc pname doc-params #'string-equal)))
                       (desc    (or ddesc (format "[%s]" ptype)))
                       (pht (make-hash-table :test 'equal :size 4)))
                  (puthash "paramname" pname pht)
                  (puthash "paramdescription" desc pht)
                  (when default
                    (puthash "paramdefault" default pht))
                  pht))
              (plist-get func :params))
             ht)
    ht))

;;;; File scanner =============================================================

(defun cicode-project--scan-file (file)
  "Scan FILE for FUNCTION definitions and variable declarations.
Return list of hash-table entries."
  (let (entries)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((case-fold-search t))
        ;; Scan for functions
        (while (re-search-forward cicode-project--func-re nil t)
          (beginning-of-line)
          (let* ((ln   (line-number-at-pos))
                 (text (thing-at-point 'line t))
                 (func (cicode-project--parse-function-line text))
                 (doc  (cicode-project--parse-doxygen-above)))
            (when func
              (push (cicode-project--make-entry func doc file ln) entries)))
          (forward-line 1))
        ;; Scan for global/module variables
        (goto-char (point-min))
        (while (re-search-forward cicode-project--var-re nil t)
          (beginning-of-line)
          (let* ((ln   (line-number-at-pos))
                 (text (thing-at-point 'line t))
                 (var  (cicode-project--parse-variable-line text))
                 (doc  (cicode-project--parse-trailing-doc text)))
            (when var
              (push (cicode-project--make-var-entry var doc file ln) entries)))
          (forward-line 1))))
    (nreverse entries)))

;;;; Reference scanning =======================================================

(defun cicode-project--compute-names-hash ()
  "MD5 of sorted project function names."
  (md5 (mapconcat #'identity
                  (sort (copy-sequence cicode-project--all-names) #'string<)
                  "\n")))

(defun cicode-project--canonical-name-table ()
  "Build downcased-name -> canonical-name lookup from project names."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (n cicode-project--all-names)
      (puthash (downcase n) n tbl))
    tbl))

(defun cicode-project--scan-file-refs (file names-re canonical)
  "Scan FILE for references matching NAMES-RE.
CANONICAL maps downcased match text to the canonical function name.
Returns list of (CANONICAL-NAME LINE COL TEXT)."
  (let (refs)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward names-re nil t)
          (let* ((raw   (match-string-no-properties 0))
                 (canon (gethash (downcase raw) canonical)))
            (when canon
              (push (list canon
                          (line-number-at-pos (match-beginning 0))
                          (- (match-beginning 0) (line-beginning-position))
                          (string-trim (or (thing-at-point 'line t) "")))
                    refs))))))
    (nreverse refs)))

(defun cicode-project--scan-refs-rg (root names canonical)
  "Use ripgrep to find all references to NAMES under ROOT.
CANONICAL maps downcased name -> canonical name.
Returns a hash-table: file -> list of (CANONICAL-NAME LINE COL TEXT)."
  (let ((patterns-file (make-temp-file "cicode-refs-"))
        (result (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file patterns-file
            (dolist (n names) (insert n "\n")))
          (with-temp-buffer
            (let ((exit-code
                   (call-process "rg" nil t nil
                                 "--json" "-i" "-w" "-F"
                                 "-f" patterns-file
                                 "-g" "*.ci"
                                 root)))
              (when (memq exit-code '(0 1))
                (goto-char (point-min))
                (while (not (eobp))
                  (let ((json (condition-case nil
                                  (json-parse-string
                                   (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))
                                (error nil))))
                    (when (and json (equal (gethash "type" json) "match"))
                      (let* ((data     (gethash "data" json))
                             (file     (gethash "text" (gethash "path" data)))
                             (line-num (gethash "line_number" data))
                             (lines-obj (gethash "lines" data))
                             (line-txt (string-trim
                                        (or (and lines-obj
                                                 (gethash "text" lines-obj))
                                            "")))
                             (subs     (gethash "submatches" data)))
                        (dotimes (i (length subs))
                          (let* ((sm    (aref subs i))
                                 (raw   (gethash "text"
                                                 (gethash "match" sm)))
                                 (col   (gethash "start" sm))
                                 (canon (gethash (downcase raw) canonical)))
                            (when canon
                              (push (list canon line-num col line-txt)
                                    (gethash file result))))))))
                  (forward-line 1))))))
      (delete-file patterns-file))
    result))

(defun cicode-project--build-refs-from-tuples (file-tuples)
  "Populate reference tables from FILE-TUPLES (file -> list of tuples)."
  (clrhash cicode-project--references)
  (clrhash cicode-project--file-references)
  (maphash
   (lambda (file tuples)
     (puthash file tuples cicode-project--file-references)
     (dolist (tup tuples)
       (push (list :file file
                   :line (nth 1 tup)
                   :col  (nth 2 tup)
                   :text (nth 3 tup))
             (gethash (downcase (nth 0 tup))
                      cicode-project--references))))
   file-tuples)
  (setq cicode-project--names-hash (cicode-project--compute-names-hash)))

(defun cicode-project--build-references (root files &optional cache)
  "Build the reference index for all FILES under ROOT.
Uses CACHE data when file hashes and function names haven't changed."
  (let* ((canonical      (cicode-project--canonical-name-table))
         (names-hash     (cicode-project--compute-names-hash))
         (cached-nhash   (when cache (gethash "names_hash" cache)))
         (names-ok       (and cached-nhash (equal names-hash cached-nhash)))
         (cf-table       (when cache (gethash "files" cache)))
         (file-tuples    (make-hash-table :test 'equal))
         (changed-files  '()))
    ;; Classify files: use cache or needs re-scan
    (dolist (file files)
      (let* ((cf           (when cf-table (gethash file cf-table)))
             (cached-hash  (when cf (gethash "hash" cf)))
             (current-hash (gethash file cicode-project--file-hashes)))
        (if (and names-ok cf (equal cached-hash current-hash)
                 (gethash "references" cf))
            ;; Load from cache
            (let ((raw-refs (gethash "references" cf)))
              (puthash file
                       (mapcar (lambda (r)
                                 (list (gethash "name" r)
                                       (gethash "line" r)
                                       (gethash "col" r)
                                       (gethash "text" r)))
                               (append raw-refs nil))
                       file-tuples))
          (push file changed-files))))
    ;; Scan changed files
    (when (and changed-files cicode-project--all-names)
      (if (executable-find "rg")
          ;; rg scans everything; keep only changed-file results
          (let ((rg-res (cicode-project--scan-refs-rg
                         root cicode-project--all-names canonical)))
            (dolist (file changed-files)
              (puthash file (gethash file rg-res '()) file-tuples)))
        ;; Elisp fallback
        (let ((names-re (regexp-opt cicode-project--all-names 'words)))
          (dolist (file changed-files)
            (condition-case nil
                (puthash file
                         (cicode-project--scan-file-refs file names-re canonical)
                         file-tuples)
              (error nil))))))
    ;; Build lookup tables
    (cicode-project--build-refs-from-tuples file-tuples)))

(defun cicode-project--rescan-file-refs (file)
  "Re-scan a single FILE for references and update the index."
  (when cicode-project--all-names
    ;; Remove old refs originating from this file
    (dolist (tup (gethash file cicode-project--file-references))
      (let* ((key (downcase (nth 0 tup)))
             (old (gethash key cicode-project--references)))
        (puthash key
                 (cl-remove-if (lambda (r) (equal (plist-get r :file) file)) old)
                 cicode-project--references)))
    ;; Scan fresh
    (let* ((canonical (cicode-project--canonical-name-table))
           (names-re  (regexp-opt cicode-project--all-names 'words))
           (tuples    (cicode-project--scan-file-refs file names-re canonical)))
      (puthash file tuples cicode-project--file-references)
      (dolist (tup tuples)
        (push (list :file file
                    :line (nth 1 tup)
                    :col  (nth 2 tup)
                    :text (nth 3 tup))
              (gethash (downcase (nth 0 tup))
                       cicode-project--references))))))

;;;; Case-insensitive lookup =================================================

(defun cicode-project--lookup (identifier)
  "Look up IDENTIFIER in the completion table, case-insensitively.
Returns the entry hash-table or nil."
  (or (gethash identifier cicode-hash-table-completion)
      (let (found)
        (maphash (lambda (k v)
                   (when (and (not found) (string-equal-ignore-case k identifier))
                     (setq found v)))
                 cicode-hash-table-completion)
        found)))

;;;; Completion table helpers =================================================

(defun cicode-project--remove-file (file)
  "Remove all functions belonging to FILE from the completion table."
  (dolist (name (gethash file cicode-project--file-functions))
    (remhash name cicode-hash-table-completion)
    (setq cicode-project--all-names
          (delete name cicode-project--all-names)))
  (remhash file cicode-project--file-functions))

(defun cicode-project--inject-file (file entries)
  "Add ENTRIES for FILE into the completion table."
  (let (names)
    (dolist (entry entries)
      (let ((name (gethash "name" entry)))
        (puthash name entry cicode-hash-table-completion)
        (cl-pushnew name cicode-project--all-names :test #'string=)
        (push name names)))
    (puthash file names cicode-project--file-functions)))

;;;; Cache I/O ================================================================

(defun cicode-project--serialize-entry (entry)
  "Convert an in-memory hash-table ENTRY to one safe for `json-serialize'."
  (let ((out (make-hash-table :test 'equal :size 10)))
    (dolist (key '("name" "doc" "returns" "returnType" "returnDesc" "syntax" "example"
                   "line" "scope" "kind" "init"))
      (puthash key (gethash key entry "") out))
    ;; params: list of ht -> vector of ht (json arrays)
    (puthash "params"
             (vconcat
              (mapcar
               (lambda (p)
                 (let ((pht (make-hash-table :test 'equal :size 4)))
                   (puthash "paramname"        (gethash "paramname" p)        pht)
                   (puthash "paramdescription" (gethash "paramdescription" p) pht)
                   (when (gethash "paramdefault" p)
                     (puthash "paramdefault" (gethash "paramdefault" p) pht))
                   pht))
               (gethash "params" entry)))
             out)
    out))

(defun cicode-project-save-cache ()
  "Write the current project index to the cache file."
  (interactive)
  (let ((data    (make-hash-table :test 'equal))
        (files-ht (make-hash-table :test 'equal)))
    (maphash
     (lambda (file names)
       (let ((fht (make-hash-table :test 'equal :size 4)))
         (puthash "hash" (gethash file cicode-project--file-hashes "") fht)
         (puthash "functions"
                  (vconcat
                   (delq nil
                         (mapcar
                          (lambda (name)
                            (when-let ((e (gethash name cicode-hash-table-completion)))
                              (cicode-project--serialize-entry e)))
                          names)))
                  fht)
         ;; Serialise per-file references
         (let ((refs (gethash file cicode-project--file-references)))
           (puthash "references"
                    (vconcat
                     (mapcar (lambda (tup)
                               (let ((rht (make-hash-table :test 'equal :size 4)))
                                 (puthash "name" (nth 0 tup) rht)
                                 (puthash "line" (nth 1 tup) rht)
                                 (puthash "col"  (nth 2 tup) rht)
                                 (puthash "text" (nth 3 tup) rht)
                                 rht))
                             (or refs '())))
                    fht))
         (puthash file fht files-ht)))
     cicode-project--file-functions)
    (puthash "version" 2 data)
    (puthash "names_hash" (or cicode-project--names-hash "") data)
    (puthash "files" files-ht data)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file (cicode-project--cache-path)
        (insert (json-serialize data))))))

(defun cicode-project--load-cache ()
  "Read the cache file.  Return parsed hash-table or nil."
  (let ((path (cicode-project--cache-path)))
    (when (file-exists-p path)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents path)
            (json-parse-string (buffer-string)))
        (error
         (message "Cicode: cache file corrupt, will re-scan")
         nil)))))

(defun cicode-project--fill-cached-entry (entry file)
  "Make a cached ENTRY (from json-parse) usable, injecting FILE path.
Converts param vectors to lists so `mapcar' is consistent."
  (puthash "file" file entry)
  (let ((params (gethash "params" entry)))
    (when (vectorp params)
      (puthash "params" (append params nil) entry)))
  entry)

;;;; Main scan ================================================================

;;;###autoload
(defun cicode-project-scan (&optional force)
  "Index all .ci files in the project.
Loads unchanged files from the cache; re-scans changed ones.
With prefix arg FORCE, ignore the cache entirely."
  (interactive "P")
  (cicode-mode-load-functions)
  (let* ((root     (cicode-project--root))
         (files    (cicode-project--find-ci-files root))
         (cache    (unless force (cicode-project--load-cache)))
         (cf-table (when cache (gethash "files" cache)))
         (n-scan 0) (n-cache 0) (n-funcs 0))
    ;; Wipe previous project entries
    (dolist (name cicode-project--all-names)
      (remhash name cicode-hash-table-completion))
    (setq cicode-project--all-names nil)
    (clrhash cicode-project--file-functions)
    (clrhash cicode-project--file-hashes)
    ;; Process each file
    (dolist (file files)
      (let ((hash (cicode-project--hash-file file)))
        (when hash ; skip unreadable files
          (let* ((cf         (when cf-table (gethash file cf-table)))
                 (cached-hash (when cf (gethash "hash" cf))))
            (puthash file hash cicode-project--file-hashes)
            (if (and cf (equal hash cached-hash))
                ;; --- from cache ---
                (let ((funcs (gethash "functions" cf))
                      names)
                  (dotimes (i (length funcs))
                    (let* ((entry (cicode-project--fill-cached-entry
                                   (aref funcs i) file))
                           (name  (gethash "name" entry)))
                      (puthash name entry cicode-hash-table-completion)
                      (cl-pushnew name cicode-project--all-names :test #'string=)
                      (push name names)
                      (cl-incf n-funcs)))
                  (puthash file names cicode-project--file-functions)
                  (cl-incf n-cache))
              ;; --- fresh scan ---
              (condition-case nil
                  (let ((entries (cicode-project--scan-file file)))
                    (cicode-project--inject-file file entries)
                    (cl-incf n-funcs (length entries))
                    (cl-incf n-scan))
                (error nil)))))))
    ;; Build reference index (uses rg if available, cache-aware)
    (message "Cicode: building reference index...")
    (cicode-project--build-references root files cache)
    ;; Persist
    (cicode-project-save-cache)
    (message "Cicode: %d files scanned, %d from cache, %d project functions, %d ref entries"
             n-scan n-cache n-funcs
             (let ((n 0))
               (maphash (lambda (_k v) (cl-incf n (length v)))
                        cicode-project--references)
               n))))

;;;###autoload
(defun cicode-project-rescan-buffer ()
  "Re-scan the current buffer and update the index incrementally."
  (interactive)
  (when (and buffer-file-name
             (string-suffix-p ".ci" buffer-file-name))
    (cicode-mode-load-functions)
    (let* ((file buffer-file-name)
           (hash (cicode-project--hash-file file)))
      ;; Remove stale entries for this file
      (cicode-project--remove-file file)
      ;; Scan & inject
      (puthash file hash cicode-project--file-hashes)
      (let ((entries (cicode-project--scan-file file)))
        (cicode-project--inject-file file entries)
        (cicode-project--rescan-file-refs file)
        (cicode-project-save-cache)
        (message "Cicode: re-scanned %s (%d functions)"
                 (file-name-nondirectory file) (length entries))))))

;;;; Xref backend =============================================================

(defun cicode-project--xref-backend ()
  "Return the xref backend symbol for Cicode buffers."
  'cicode-project)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'cicode-project)))
  "Return the Cicode symbol at point."
  (thing-at-point 'symbol t))

(cl-defmethod xref-backend-definitions ((_backend (eql 'cicode-project)) identifier)
  "Find the definition of IDENTIFIER in the project."
  (cicode-mode-load-functions)
  (when-let* ((entry (cicode-project--lookup identifier))
              (file  (gethash "file" entry))
              (line  (gethash "line" entry)))
    (list (xref-make
           (format "%s  [%s]" identifier (gethash "scope" entry "PUBLIC"))
           (xref-make-file-location file line 0)))))

(cl-defmethod xref-backend-references ((_backend (eql 'cicode-project)) identifier)
  "Find all references to IDENTIFIER.
Uses the pre-built index for project functions; falls back to
a live search for builtins or unknown names."
  (let ((refs (gethash (downcase identifier) cicode-project--references)))
    (if refs
        (mapcar (lambda (r)
                  (xref-make
                   (plist-get r :text)
                   (xref-make-file-location
                    (plist-get r :file)
                    (plist-get r :line)
                    (plist-get r :col))))
                refs)
      ;; Fallback: live search across project files
      (let* ((root  (cicode-project--root))
             (files (cicode-project--find-ci-files root))
             (re    (concat "\\_<" (regexp-quote identifier) "\\_>"))
             (case-fold-search t)
             results)
        (dolist (file files)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward re nil t)
              (let ((ln  (line-number-at-pos))
                    (col (- (match-beginning 0) (line-beginning-position)))
                    (txt (string-trim (or (thing-at-point 'line t) ""))))
                (push (xref-make txt (xref-make-file-location file ln col))
                      results)))))
        (nreverse results)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql 'cicode-project)))
  "Return completion table of all known Cicode function names."
  (cicode-mode-load-functions)
  (let (names)
    (maphash (lambda (k _v) (push k names)) cicode-hash-table-completion)
    names))

;;;; Imenu ====================================================================

(defvar cicode-project--imenu-expression
  `(("Function" ,"\\<FUNCTION\\s-+\\(\\w+\\)" 1)
    ("Variable" ,"^[ \t]*\\(?:GLOBAL\\|MODULE\\)[ \t]+\\(?:INT\\|STRING\\|FLOAT\\|REAL\\|BOOL\\|LONG\\|OBJECT\\|TIMESTAMP\\|QUALITY\\)[ \t]+\\(\\w+\\)" 1))
  "Imenu generic expression for Cicode definitions.")

;;;; Cache-only loading (no scan) =============================================

(defun cicode-project-load-cache ()
  "Load the cache file into the completion table without scanning.
Called automatically on first open to provide immediate completions."
  (interactive)
  (let ((cache (cicode-project--load-cache)))
    (when (and cache (gethash "files" cache))
      (cicode-mode-load-functions)
      ;; Wipe previous project entries
      (dolist (name cicode-project--all-names)
        (remhash name cicode-hash-table-completion))
      (setq cicode-project--all-names nil)
      (clrhash cicode-project--file-functions)
      (clrhash cicode-project--file-hashes)
      ;; fill from cache
      (let ((cf-table (gethash "files" cache))
            (n-funcs 0))
        (maphash
         (lambda (file cf)
           (let ((hash  (gethash "hash" cf))
                 (funcs (gethash "functions" cf))
                 names)
             (puthash file hash cicode-project--file-hashes)
             (dotimes (i (length funcs))
               (let* ((entry (cicode-project--fill-cached-entry
                              (aref funcs i) file))
                      (name  (gethash "name" entry)))
                 (puthash name entry cicode-hash-table-completion)
                 (cl-pushnew name cicode-project--all-names :test #'string=)
                 (push name names)
                 (cl-incf n-funcs)))
             (puthash file names cicode-project--file-functions)))
         cf-table)
        ;; Restore references
        (let ((names-hash (gethash "names_hash" cache)))
          (when names-hash
            (setq cicode-project--names-hash names-hash))
          (clrhash cicode-project--references)
          (clrhash cicode-project--file-references)
          (maphash
           (lambda (file cf)
             (when-let ((raw-refs (gethash "references" cf)))
               (let ((tuples (mapcar (lambda (r)
                                       (list (gethash "name" r)
                                             (gethash "line" r)
                                             (gethash "col" r)
                                             (gethash "text" r)))
                                     (append raw-refs nil))))
                 (puthash file tuples cicode-project--file-references)
                 (dolist (tup tuples)
                   (push (list :file file
                               :line (nth 1 tup)
                               :col  (nth 2 tup)
                               :text (nth 3 tup))
                         (gethash (downcase (nth 0 tup))
                                  cicode-project--references))))))
           cf-table))
        (message "Cicode: loaded %d functions from cache" n-funcs)))))

;;;; Mode hook setup ==========================================================

(defun cicode-project--setup ()
  "Hook run in `cicode-mode' buffers to enable project features."
  ;; Imenu
  (setq-local imenu-generic-expression cicode-project--imenu-expression)
  (setq-local imenu-case-fold-search t)
  ;; Xref -- must be at front so it runs before etags
  (add-hook 'xref-backend-functions #'cicode-project--xref-backend -90 t)
  ;; Re-scan current file on save
  (add-hook 'after-save-hook #'cicode-project-rescan-buffer nil t)
  ;; Load cache on first open if available
  (when (and (null cicode-project--all-names)
             (file-exists-p (cicode-project--cache-path)))
    (cicode-project-load-cache))
  ;; Optional: scan whole project on first open
  (when (and cicode-project-scan-on-open
             (null cicode-project--all-names))
    (cicode-project-scan)))

;;;###autoload
(add-hook 'cicode-mode-hook #'cicode-project--setup)

(provide 'cicode-mode-project)
;;; cicode-mode-project.el ends here
