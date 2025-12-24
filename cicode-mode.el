;;; cicode-mode.el --- Major mode for cicode  -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Sebastian Gazey
;; Author: Sebastian Gazey
;; URL: https://github.com/Sebagabones/cicode-mode
;; Created: 2025
;; Version: 0.2
;; Package-Requires: ((emacs "24.3"))
;; Keywords: languages cicode citect plant-scada aveva

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A simple major mode for editing Cicode files (from Citect/Plant Scada)
;; It is very janky and cursed, I don't understand more than 25% of what is going on with it at any given time

;; KNOWN BUGS / LIMITATIONS
;; ~SELECT CASE~ indentation is very broken
;; I have not tried to implement support for any of the ~:#~ syntax

;; Feel free to open an issue (or even better make a contribution)
;; This is my first emacs package/time writing elisp, so be warned ;)
;; TODO: Don’t use ~setq~, fix global variables...
;; TODO: Add ablity to parse code, and add in :company-location feature, which is used by company to jump to the location of current candidate
;; Long way away TODO: Tree-sitter

;;; Code:
;;;###autoload
(define-derived-mode cicode-mode prog-mode "Cicode"
  "Major mode for editing Cicode files."

  ;; Comment syntax
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (modify-syntax-entry ?/ ". 124b" cicode-mode-syntax-table)
  (modify-syntax-entry ?! "< b" cicode-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" cicode-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" cicode-mode-syntax-table)
  (modify-syntax-entry ?\\ "." cicode-mode-syntax-table)

  ;; For strings and escapes
  (modify-syntax-entry ?\" "\"" cicode-mode-syntax-table)
  (modify-syntax-entry ?^ "\\" cicode-mode-syntax-table)
  ;; Indentation
  (setq-local indent-line-function 'cicode-indent-line)


  ;; Custom face for type declarations with italics
  (defface cicode-type-face
    '((t (:inherit font-lock-number-face :slant italic)))
    "Face for Cicode type declarations with italic style."
    :group 'font-lock-faces)
  (defvar cicode-type-face 'cicode-type-face
    "Face for Cicode type declarations with italic style.")
  (add-hook 'completion-at-point-functions #'cicode-mode-capf nil t)
  (add-hook 'cicode-mode-hook #'cicode-mode-setup-eldoc)
  (setq-local font-lock-defaults
              '((;; Types (with italic style)
                 ("\\<\\(FLOAT\\|INT\\|OBJECT\\|REAL\\|STRING\\|LONG\\|BOOL\\|TIMESTAMP\\|QUALITY\\)\\>" . cicode-type-face)

                 ;; Comments
                 ("//.*$" . font-lock-comment-face)
                 ("!.*$" . font-lock-comment-delimiter-face) ;here because didnt want to put it as comment start, but needs to be able to have numbers inside it lol
                 ;; NOTE: ! style comments support fontification with the simple-comment-markup - I have no clue why the other style of comments don’t - but meh, I guess use "!" when you want to make something stand out
                 ;; Scope
                 ("\\<\\(GLOBAL\\|LOCAL\\|MODULE\\|PRIVATE\\|PUBLIC\\)\\>" . font-lock-preprocessor-face)

                 ;; Numbers
                 ("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
                 ("\\<[0-9]+\\(\\.[0-9]+\\)?\\s-*[eE]\\s-*[+-]?\\s-*[0-9]+\\>" . font-lock-constant-face)

                 ;; Keywords
                 ("\\<\\(AND\\|C\\(?:\\(?:AS\\|ICOD\\)E\\)\\|DO\\|E\\(?:LSE\\|ND\\)\\|F\\(?:OR\\|UNCTION\\)\\|GLOBAL\\|IF\\|MOD\\(?:ULE\\)?\\|NOT\\|OR\\|P\\(?:RIVATE\\|UBLIC\\)\\|RETURN\\|SELECT\\|T\\(?:HEN\\|O\\)\\|WHILE\\)\\>"
                  . font-lock-keyword-face)

                 ;; Function names
                 ("\\<FUNCTION\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-function-name-face)
                 ;; Operators and delimiters
                 ("\\(\\+=\\|-=\\|\\*=\\|/=\\|%=\\|&=\\|\\^=\\||=\\|<<\\|>>\\|==\\|!=\\|<=\\|>=\\|<\\|>\\|=\\|\\+\\|-\\|\\*\\|/\\|%\\|&\\|\\^\\||\\)" . font-lock-preprocessor-face)

                 ;; Built-in Functions
                 ("\\_<\\(A\\(?:bs\\|cc\\(?:Control\\|umBrowse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\)\\|l\\(?:arm\\(?:Ac\\(?:k\\(?:Rec\\|Tag\\|_CIREF\\)\\|tive_CIREF\\)\\|C\\(?:atGetFormat\\|lear\\(?:Rec\\|Tag\\)?\\|o\\(?:mment\\(?:RecID\\)?\\|unt\\(?:\\(?:Equipmen\\|Lis\\)t\\)?\\)\\)\\|D\\(?:elete\\|isable\\(?:Rec\\|Tag\\)?\\|sp\\(?:Cluster\\(?:Add\\|\\(?:InUs\\|Remov\\)e\\)\\|Last\\|Next\\|Prev\\)?\\)\\|E\\(?:nable\\(?:Rec\\|Tag\\)?\\|ventQue_CIREF\\)\\|Fi\\(?:lter\\(?:Close\\|Edit\\(?:Append\\(?:Equipment\\)?\\|C\\(?:lose\\|ommit\\)\\|First\\|HasField\\|Last\\|Next\\|Open\\|Prev\\|Set\\)\\|Form\\|Open\\)\\|rst\\(?:\\(?:Cat\\|Pri\\|Tag\\)Rec\\)\\)\\|Get\\(?:D\\(?:elay\\(?:Rec\\)?\\|sp\\)\\|Fi\\(?:eldRec\\|lterName\\)\\|Info\\|OrderbyKey\\|Threshold\\(?:Rec\\)?\\)\\|H\\(?:elp\\|ighestPriority\\)\\|List\\(?:Create\\|D\\(?:\\(?:estro\\|ispla\\)y\\)\\|Fill\\)\\|N\\(?:ext\\(?:\\(?:Cat\\|Pri\\|Tag\\)Rec\\)\\|otifyVarChange\\)\\|Query\\(?:\\(?:Firs\\|Nex\\)tRec\\)\\|ResetQuery\\|S\\(?:et\\(?:Delay\\(?:Rec\\)?\\|Info\\|Threshold\\(?:Rec\\)?\\)\\|plit\\|um\\(?:Append\\|Commit\\|Delete\\|Fi\\(?:nd\\|rst\\)\\|Get\\|Last\\|Next\\|Prev\\|S\\(?:\\(?:e\\|pli\\)t\\)\\|Type\\)\\)\\|TagFromEquipment\\)\\|m\\(?:Browse\\(?:Ack\\|Close\\|Disable\\|Enable\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|Summary\\(?:Ack\\|C\\(?:l\\(?:ear\\|ose\\)\\|ommit\\)\\|D\\(?:elete\\(?:All\\)?\\|isable\\)\\|Enable\\|First\\|GetField\\|Last\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\|SetFieldValue\\)\\|Tags\\(?:Ack\\|Cl\\(?:ear\\|ose\\)\\|Disable\\|Enable\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\)\\)\\|nByName\\|r\\(?:c\\(?:Cos\\|\\(?:Si\\|Ta\\)n\\)\\|eaCheck\\|ray\\(?:C\\(?:opy\\|reate\\(?:ByAn\\)?\\)\\|Destroy\\(?:ByAn\\)?\\|Exists\\(?:ByAn\\)?\\|FillFromAlarmDataByAn\\|Get\\(?:ArrayByAn\\|In\\(?:fo\\|t\\(?:ByAn\\)?\\)\\|MapName\\(?:ByAn\\)?\\|String\\(?:ByAn\\)?\\)\\|IsDirty\\|S\\(?:et\\(?:I\\(?:nt\\(?:ByAn\\)?\\|sDirty\\)\\|String\\(?:ByAn\\)?\\)\\|wap\\)\\)\\)\\|ss\\(?:Chain\\(?:P\\(?:age\\|opUp\\)\\|Win\\(?:Free\\)?\\)?\\|Equip\\(?:\\(?:Parameter\\|Reference\\)s\\)\\|Get\\(?:Property\\|Scale\\)\\|Info\\(?:Ex\\)?\\|Metadata\\(?:P\\(?:age\\|opUp\\)\\|Win\\)?\\|P\\(?:age\\|opUp\\)\\|ScaleStr\\|T\\(?:ag\\|itle\\)\\|VarTags\\|Win\\(?:Replace\\)?\\|ert\\)?\\)\\|Beep\\|C\\(?:allEvent\\|ha\\(?:inEvent\\|rToStr\\)\\|itectInfo\\|l\\(?:ip\\(?:Copy\\|Paste\\|ReadLn\\|SetMode\\|WriteLn\\)\\|uster\\(?:Activate\\|Deactivate\\|First\\|GetName\\|IsActive\\|Next\\|S\\(?:e\\(?:rverTypes\\|tName\\)\\|tatus\\|wapActive\\)\\)\\)\\|o\\(?:de\\(?:\\(?:SetMod\\|Trac\\)e\\)\\|m\\(?:Close\\|Open\\|Re\\(?:ad\\|set\\)\\|Write\\)\\|s\\)\\|reate\\(?:\\(?:Control\\)?Object\\)\\)\\|D\\(?:DE\\(?:Exec\\|Post\\|Read\\|Write\\|h\\(?:Execute\\|GetLastError\\|Initiate\\|Poke\\|Re\\(?:adLn\\|quest\\)\\|SetMode\\|Terminate\\|WriteLn\\)\\)\\|LL\\(?:C\\(?:all\\(?:Ex\\)?\\|lose\\)\\|Open\\)\\|ate\\(?:Add\\|Day\\|Info\\|Month\\|Sub\\|WeekDay\\|Year\\|_CIREF\\)\\|e\\(?:bug\\(?:Break\\|Msg\\(?:Set\\)?\\)\\|gToRad\\|layShutdown\\|v\\(?:Append\\|C\\(?:lose\\|ontrol\\|urr\\)\\|D\\(?:\\(?:elet\\|isabl\\)e\\)\\|EOF\\|F\\(?:i\\(?:nd\\|rst\\)\\|lush\\)\\|GetField\\|History\\|Info\\|Modify\\|Next\\|Open\\|Pr\\(?:ev\\|int\\)\\|Re\\(?:ad\\(?:Ln\\)?\\|cNo\\)\\|S\\(?:e\\(?:ek\\|tField\\)\\|ize\\)\\|Write\\(?:Ln\\)?\\|Zap\\)\\)\\|isplayRuntimeManager\\|llClass\\(?:C\\(?:allMethod\\|reate\\)\\|Dispose\\|GetProperty\\|IsValid\\|SetProperty\\)\\|riverInfo\\|sp\\(?:A\\(?:n\\(?:CreateControlObject\\|Free\\|Get\\(?:Area\\|Metadata\\(?:At\\)?\\|P\\(?:os\\|rivilege\\)\\)\\|In\\(?:Rgn\\|fo\\)\\|Move\\(?:Rel\\)?\\|New\\(?:Rel\\)?\\|Set\\(?:Metadata\\(?:At\\)?\\|Name\\)\\)\\|rrayByAn\\)\\|B\\(?:ar\\|mp\\|utton\\(?:Fn\\)?\\)\\|C\\(?:hart\\|learClip\\|ol\\)\\|D\\(?:el\\(?:ayRender\\(?:Begin\\|End\\)\\)?\\|irty\\)\\|Error\\|F\\(?:ile\\(?:Get\\(?:Info\\|Name\\)\\|S\\(?:croll\\|etName\\)\\)?\\|ont\\(?:Hnd\\)?\\|ullScreen\\)\\|G\\(?:et\\(?:An\\(?:Bottom\\|Cur\\|Extent\\|F\\(?:irst\\|rom\\(?:Name\\(?:Relative\\)?\\|Point\\)\\)\\|Height\\|Left\\|Next\\|R\\(?:\\(?:awExten\\|igh\\)t\\)\\|Top\\|Width\\)\\|Env\\|M\\(?:etadataFromName\\(?:Relative\\)?\\|ouse\\(?:Over\\)?\\)\\|N\\(?:\\(?:ameFrom\\|earest\\)An\\)\\|ParentAn\\|Slider\\|Tip\\)\\|rayButton\\)\\|I\\(?:nfo\\(?:Destroy\\|Field\\|New\\|Valid\\)?\\|sButtonGray\\)\\|Kernel\\|M\\(?:CI\\|arker\\(?:Move\\|New\\)\\)\\|P\\(?:laySound\\|opup\\(?:\\(?:Config\\)?Menu\\)\\)\\|R\\(?:ichText\\(?:E\\(?:dit\\|nable\\)\\|GetInfo\\|Load\\|P\\(?:gScroll\\|rint\\)\\|S\\(?:ave\\|croll\\)\\)?\\|ub\\(?:End\\|Move\\|S\\(?:etC\\(?:lip\\|olor\\)\\|tart\\)\\)\\)\\|S\\(?:et\\(?:C\\(?:lip\\|urColor\\)\\|MetadataFromName\\(?:Relative\\)?\\|PopupMenuFont\\|Slider\\|T\\(?:ip\\|ooltipFont\\)\\)\\|t\\(?:atus\\|r\\)\\|ym\\(?:A\\(?:nm\\(?:Ex\\)?\\|tSize\\)\\)?\\)\\|T\\(?:ext\\|ipMode\\|rend\\(?:Info\\)?\\)\\|WebContent\\(?:[GS]etURL\\)\\)\\|umpKernel\\)\\|E\\(?:n\\(?:gToGeneric\\|terCriticalSection\\)\\|quip\\(?:Browse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|CheckUpdate\\|GetP\\(?:aram\\|roperty\\)\\|RefBrowse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|S\\(?:etProperty\\|tateBrowse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\)\\)\\|rr\\(?:Com\\|Drv\\|GetHw\\|Help\\|Info\\|Log\\|Msg\\|Set\\(?:Hw\\|Level\\)?\\|Trap\\)\\|x\\(?:ec\\(?:uteDTSPkg\\)?\\|p\\)\\)\\|F\\(?:act\\|ile\\(?:C\\(?:lose\\|opy\\)\\|Delete\\|E\\(?:OF\\|xist\\)\\|Find\\(?:Close\\)?\\|GetTime\\|MakePath\\|Open\\|Print\\|R\\(?:e\\(?:ad\\(?:Block\\|Ln\\)?\\|name\\)\\|ichTextPrint\\)\\|S\\(?:e\\(?:ek\\|tTime\\)\\|ize\\|plitPath\\)\\|Write\\(?:Block\\|Ln\\)?\\)\\|mt\\(?:Close\\|FieldHnd\\|GetField\\(?:Count\\|Hnd\\|Name\\|Width\\)?\\|Open\\|SetField\\(?:Hnd\\)?\\|ToStr\\)\\|orm\\(?:A\\(?:ctive\\|ddList\\)\\|Button\\|C\\(?:heckBox\\|omboBox\\|urr\\)\\|Destroy\\|Edit\\|Field\\|G\\(?:et\\(?:CurrInst\\|Data\\|\\(?:Ins\\|Tex\\)t\\)\\|oto\\|roupBox\\)\\|Input\\|List\\(?:AddText\\|Box\\|\\(?:Delete\\|Select\\)Text\\)\\|N\\(?:ew\\|umPad\\)\\|OpenFile\\|P\\(?:assword\\|osition\\|rompt\\)\\|R\\(?:adioButton\\|ead\\)\\|S\\(?:aveAsFile\\|e\\(?:curePassword\\|lectPrinter\\|t\\(?:Data\\|\\(?:Ins\\|Tex\\)t\\)\\)\\)\\|WndHnd\\)\\|u\\(?:llName\\|zzy\\(?:Close\\|Get\\(?:\\(?:Code\\|Shell\\)Value\\)\\|Open\\|\\(?:Set\\(?:\\(?:Code\\|Shell\\)Valu\\)\\|Trac\\)e\\)\\)\\)\\|G\\(?:et\\(?:Area\\|BlueValue\\|E\\(?:nv\\|vent\\)\\|GreenValue\\|L\\(?:anguage\\|ogging\\)\\|Priv\\|\\(?:RedValu\\|WinTitl\\)e\\)\\|rp\\(?:Close\\|Delete\\|First\\|In\\(?:sert\\)?\\|Math\\|N\\(?:ame\\|ext\\)\\|Open\\|ToStr\\)\\)\\|H\\(?:alt\\|exToStr\\|igh\\(?:Byte\\|Word\\)\\|wAlarmQue\\)\\|I\\(?:ODevice\\(?:Control\\|Info\\|Stats\\)\\|n\\(?:foForm\\(?:An\\)?\\|put\\|tTo\\(?:Real\\|Str\\)\\)\\|sError\\)\\|Ke\\(?:r\\(?:Cmd\\|nel\\(?:QueueLength\\|TableI\\(?:nfo\\|temCount\\)\\)\\)\\|y\\(?:AllowCursor\\|Bs\\|Down\\|Get\\(?:Cursor\\)?\\|Left\\|Move\\|P\\(?:eek\\|ut\\(?:Str\\)?\\)\\|R\\(?:eplay\\(?:All\\)?\\|ight\\)\\|Set\\(?:Cursor\\|Seq\\)\\|Up\\)\\)\\|L\\(?:anguageFileTranslate\\|eaveCriticalSection\\|ibAlarmFilterForm\\|n\\|o\\(?:g\\(?:in\\(?:Form\\)?\\|out\\(?:Idle\\)?\\)?\\|w\\(?:Byte\\|Word\\)\\)\\)\\|M\\(?:a\\(?:il\\(?:Error\\|Logo\\(?:ff\\|n\\)\\|\\(?:Rea\\|Sen\\)d\\)\\|keColour\\|p\\(?:Cl\\(?:ear\\|ose\\)\\|Exists\\|Key\\(?:Count\\|Delete\\|Exists\\|\\(?:Firs\\|Nex\\)t\\)\\|Open\\|Value\\(?:Get\\|Set\\(?:Quality\\)?\\)\\)\\|x\\)\\|e\\(?:nu\\(?:Get\\(?:Child\\|FirstChild\\|GenericNode\\|N\\(?:extChild\\|odeByPath\\)\\|P\\(?:a\\(?:geNode\\|rent\\)\\|revChild\\)\\|WindowNode\\)\\|Node\\(?:AddChild\\|Get\\(?:Curr\\|Depth\\|Expanded\\|Property\\|TargetPage\\)\\|HasCommand\\|Is\\(?:Disabled\\|Hidden\\)\\|R\\(?:emove\\|unCommand\\)\\|Set\\(?:DisabledWhen\\|Expanded\\|HiddenWhen\\|Property\\)\\)\\|Reload\\)\\|ssage\\)\\|in\\|sg\\(?:Brdcst\\|Close\\|GetCurr\\|Open\\|R\\(?:PC\\|ead\\)\\|\\(?:Sta\\|Wri\\)te\\)\\|ulti\\(?:MonitorStart\\|Signature\\(?:Form\\|TagWrite\\)\\)\\)\\|Name\\|O\\(?:LEDateToTime\\|bject\\(?:Associate\\(?:Events\\|PropertyWithTag\\)\\|ByName\\|HasInterface\\|IsValid\\|ToStr\\)\\|nEvent\\)\\|P\\(?:a\\(?:ckedRGB\\(?:ToCitectColour\\)?\\|ge\\(?:Alarm_CIREF\\|Back\\|Dis\\(?:abled\\|play\\)\\|Exists\\|F\\(?:ile\\(?:Info\\(?:Ex\\)?\\)?\\|orward\\)\\|G\\(?:et\\(?:Int\\|Str\\)\\|oto\\)\\|H\\(?:ardware\\|istory\\(?:DspMenu\\|Empty\\)\\|ome\\)\\|Info\\|KeyboardCommandsPut\\|L\\(?:ast\\|ist\\(?:C\\(?:\\(?:ou\\|urre\\)nt\\)\\|D\\(?:elete\\|isplay\\)\\|Info\\)\\)\\|Menu\\|Next\\|P\\(?:eek\\(?:\\(?:Curren\\|Las\\)t\\)\\|op\\(?:Last\\|Up\\)\\|r\\(?:ev\\|ocessAnalyst\\(?:Pens\\)?\\)\\|ushLast\\)\\|R\\(?:ecall\\|ichTextFile\\)\\|S\\(?:OE\\|e\\(?:lect\\|t\\(?:Int\\|Str\\)\\)\\|ummary\\)\\|T\\(?:ask\\|r\\(?:ansformCoords\\|end\\(?:Ex\\)?\\)\\)\\)\\|rameter\\(?:\\(?:Ge\\|Pu\\)t\\)\\|thToStr\\)\\|i\\|lot\\(?:Close\\|Draw\\|G\\(?:etMarker\\|rid\\)\\|Info\\|Line\\|Marker\\|Open\\|S\\(?:\\(?:cale\\|et\\)Marker\\)\\|Text\\|XYLine\\)\\|ow\\|r\\(?:int\\(?:Font\\|Ln\\)?\\|o\\(?:cess\\(?:Analyst\\(?:LoadFile\\|PopUp\\|Se\\(?:lect\\|tPen\\)\\|Win\\)\\|Is\\(?:Client\\|Server\\)\\|Restart\\)\\|ductInfo\\|ject\\(?:Info\\|Restart\\(?:[GS]et\\)\\)\\|mpt\\)\\)\\|ulse\\)\\|Qu\\(?:ality\\(?:Create\\|GetPart\\|Is\\(?:Bad\\|ControlInhibit\\|Good\\|Override\\|Uncertain\\)\\|SetPart\\|ToStr\\)\\|e\\(?:Close\\|Length\\|Open\\|Peek\\|Read\\|Write\\)\\)\\|R\\(?:a\\(?:dToDeg\\|nd\\)\\|e\\(?:Read\\|alToStr\\|p\\(?:GetC\\(?:luster\\|ontrol\\)\\|SetControl\\|ort\\)\\|setScreenProfile\\)\\|ound\\)\\|S\\(?:OE\\(?:Archive\\|Dismount\\|EventAdd\\|Mount\\)\\|PC\\(?:Alarms\\|ClientInfo\\|Get\\(?:\\(?:Histogram\\|Subgroup\\)Table\\)\\|\\(?:P\\(?:lo\\|rocessXRS\\(?:[GS]e\\)\\)\\|S\\(?:etLimi\\|\\(?:pecLimit[GS]\\|ubgroupSize[GS]\\)e\\)\\)t\\)\\|QL\\(?:Append\\|BeginTran\\|C\\(?:a\\(?:\\(?:l\\|nce\\)l\\)\\|lose\\|o\\(?:\\(?:mmi\\|nnec\\)t\\)\\|reate\\)\\|Dis\\(?:connect\\|pose\\)\\|E\\(?:nd\\|rrMsg\\|xec\\)\\|FieldInfo\\|Get\\(?:Field\\|Recordset\\|Scalar\\)\\|I\\(?:nfo\\|sNullField\\)\\|N\\(?:ext\\|oFields\\|um\\(?:Change\\|Fields\\)\\)\\|Open\\|P\\(?:arams\\(?:ClearAll\\|SetAs\\(?:Int\\|Real\\|String\\)\\)\\|rev\\)\\|Query\\(?:\\(?:Creat\\|Dispos\\)e\\)\\|Ro\\(?:llBack\\|wCount\\)\\|Set\\|TraceO\\(?:ff\\|n\\)\\)\\|ch\\(?:d\\(?:C\\(?:lose\\|onfig\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\)\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\|Special\\(?:Add\\|Close\\|Delete\\|First\\|GetField\\|Item\\(?:Add\\(?:Range\\)?\\|Close\\|Delete\\(?:Range\\)?\\|First\\|GetField\\|Modify\\(?:Range\\)?\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|Modify\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\)\\|eduleItem\\(?:Add\\|Delete\\|Modify\\|SetRepeat\\)\\)\\|e\\(?:m\\(?:Close\\|Open\\|Signal\\|Wait\\)\\|ndKeys\\|r\\(?:ialKey\\|v\\(?:er\\(?:Browse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|DumpKernel\\|GetProperty\\|I\\(?:nfo\\(?:Ex\\)?\\|sOnline\\)\\|R\\(?:PC\\|e\\(?:load\\|start\\)\\)\\)\\|iceGetList\\)\\)\\|t\\(?:Area\\|Event\\|Logging\\)\\)\\|hutdown\\(?:Form\\|Mode\\)?\\|i\\(?:g?n\\)\\|leep\\(?:MS\\)?\\|qrt\\|tr\\(?:C\\(?:alcWidth\\|lean\\)\\|EndsWith\\|F\\(?:ill\\|ormat\\)\\|GetChar\\|L\\(?:e\\(?:ft\\|ngth\\)\\|istContainsItem\\|ower\\)\\|Mid\\|Pad\\|R\\(?:eplace\\|ight\\)\\|S\\(?:e\\(?:arch\\|tChar\\)\\|plit\\)\\|T\\(?:o\\(?:Bool\\|Char\\|Date\\|Fmt\\|Grp\\|Hex\\|Int\\|L\\(?:ines\\|ocalText\\)\\|Period\\|Real\\|Time\\(?:stamp\\)?\\|Value\\)\\|r\\(?:im\\|uncFont\\(?:Hnd\\|Tooltip\\)?\\)\\)\\|Upper\\|Word\\)\\|ubscription\\(?:AddCallback\\|Get\\(?:Attribute\\|Info\\|Quality\\|T\\(?:ag\\|imestamp\\)\\|Value\\)\\|RemoveCallback\\)\\|witchConfig\\|ysTime\\(?:Delta\\)?\\)\\|T\\(?:a\\(?:ble\\(?:Lookup\\|Math\\|Shift\\)\\|g\\(?:Browse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|Debug\\(?:Form\\)?\\|Event\\(?:Format\\|Queue\\)\\|Get\\(?:Property\\|\\(?:Scal\\|Valu\\)e\\)\\|Info\\(?:Ex\\)?\\|R\\(?:DBReload\\|amp\\|e\\(?:ad\\(?:Ex\\)?\\|solve\\)\\)\\|S\\(?:caleStr\\|etOverride\\(?:Bad\\|Good\\|Quality\\|Uncertain\\)\\|ubscribe\\)\\|Un\\(?:\\(?:resolv\\|subscrib\\)e\\)\\|Write\\(?:EventQue\\|\\(?:Int\\|Real\\)Array\\)?\\)\\|n\\|sk\\(?:C\\(?:all\\|luster\\)\\|GetSignal\\|Hnd\\|Kill\\|New\\(?:Ex\\)?\\|Resume\\|S\\(?:etSignal\\|uspend\\)\\)\\)\\|est\\(?:\\(?:Random\\|S\\(?:aw\\|in\\|quare\\)\\|Triang\\)Wave\\)\\|ime\\(?:Current\\|Hour\\|In\\(?:fo\\|tToTimestamp\\)\\|Mi\\(?:dNight\\|n\\)\\|Se[ct]\\|To\\(?:OLEDate\\|Str\\)\\|UTCOffset\\|_CIREF\\|stamp\\(?:Add\\|C\\(?:reate\\|urrent\\)\\|Difference\\|Format\\|GetPart\\|Sub\\|To\\(?:Str\\|TimeInt\\)\\)\\)\\|oggle\\|r\\(?:aceMsg\\|end\\(?:DspCursor\\(?:Comment\\|Scale\\|T\\(?:ag\\|ime\\)\\|Value\\)\\|GetAn\\|PopUp\\|Run\\|Set\\(?:Date\\|S\\(?:cale\\|pan\\)\\|Time\\(?:base\\)?\\)\\|Win\\|Zoom\\)\\|n\\(?:AddHistory\\|Browse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|C\\(?:lientInfo\\|omparePlot\\)\\|Del\\(?:History\\|ete\\)\\|E\\(?:cho\\|vent\\(?:GetTable\\(?:MS\\)?\\|SetTable\\(?:MS\\)?\\)\\|xport\\(?:C\\(?:SV\\|lip\\)\\|D\\(?:BF\\|DE\\)\\)\\)\\|Flush\\|Get\\(?:Buf\\(?:Event\\|\\(?:Tim\\|Valu\\)e\\)\\|C\\(?:luster\\|ursor\\(?:Event\\|MSTime\\|Pos\\|Time\\|Value\\(?:Str\\)?\\)\\)\\|D\\(?:\\(?:efScal\\|isplayMod\\)e\\)\\|Event\\|Format\\|GatedValue\\|InvalidValue\\|M\\(?:\\(?:STim\\|od\\)e\\)\\|Pe\\(?:n\\(?:Comment\\|Focus\\|No\\)?\\|riod\\)\\|S\\(?:cale\\(?:Str\\)?\\|pan\\)\\|T\\(?:\\(?:abl\\|im\\)e\\)\\|Units\\)\\|I\\(?:nfo\\|sValidValue\\)\\|New\\|P\\(?:\\(?:lo\\|rin\\)t\\)\\|S\\(?:amplesConfigured\\|croll\\|e\\(?:lect\\|t\\(?:Cursor\\(?:Pos\\)?\\|DisplayMode\\|Event\\|Pe\\(?:n\\(?:Focus\\)?\\|riod\\)\\|S\\(?:cale\\|pan\\)\\|T\\(?:\\(?:abl\\|im\\)e\\)\\)\\)\\)\\)\\)\\)\\|User\\(?:Create\\(?:ByRole\\|Form\\)?\\|Delete\\|EditForm\\|Info\\|Login\\|Password\\(?:ExpiryDays\\|Form\\)?\\|SetStr\\|UpdateRecord\\|Verify\\)\\|V\\(?:ariable\\(?:Quality\\|Timestamp\\)\\|er\\(?:ifyPrivilege\\(?:Form\\|TagWrite\\)\\|sion\\)\\)\\|W\\(?:hoAmI\\|in\\(?:Copy\\|F\\(?:ile\\|ree\\(?:Ex\\)?\\)\\|G\\(?:et\\(?:Clicked\\|F\\(?:irstChild\\|ocus\\)\\|N\\(?:ame\\|extChild\\)\\|Parent\\|WndHnd\\)\\|oto\\)\\|Mo\\(?:[dv]e\\)\\|N\\(?:e\\(?:w\\(?:\\(?:Pin\\)?At\\)?\\|xt\\)\\|umber\\)\\|P\\(?:os\\|r\\(?:ev\\|int\\(?:File\\)?\\)\\)\\|S\\(?:e\\(?:lect\\|tName\\)\\|\\(?:iz\\|tyl\\)e\\)\\|Title\\)\\|nd\\(?:Find\\|GetFileProfile\\|Help\\|Info\\|MonitorInfo\\(?:Ex\\)?\\|PutFileProfile\\|Show\\|Viewer\\)\\)\\|XML\\(?:C\\(?:\\(?:los\\|reat\\)e\\)\\|Get\\(?:Attribute\\(?:Count\\|\\(?:Nam\\|Valu\\)e\\)?\\|Child\\(?:Count\\)?\\|\\(?:Paren\\|Roo\\)t\\)\\|Node\\(?:AddChild\\|Find\\|\\(?:Get\\(?:Nam\\|Valu\\)\\|Remov\\|SetValu\\)e\\)\\|Open\\|S\\(?:\\(?:av\\|etAttribut\\)e\\)\\)\\|_Object\\(?:CallMethod\\|[GS]etProperty\\)\\|projectset\\)\\_>"  . font-lock-builtin-face)

                 ;; Function calls
                 ("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\([^)]*\\))"
                  (1 font-lock-function-name-face))

                 ;; String literals
                 ("\"[^\"]*\"" . font-lock-string-face)

                 )


                nil ; KEYWORDS-ONLY
                t   ; CASE-FOLD
                nil ; SYNTAX-ALIST
                nil ; SYNTAX-BEGIN
                )))

(defun cicode-find-matching-opener ()
  "Return the indentation of the matching block opener for the current line."
  (save-excursion
    (let ((stack '()) ; block stack
          (found-indent nil)
          (line (thing-at-point 'line t))
          (case-line-p nil))
      ;; Detect if we are on a CASE line
      (when (string-match-p "^[ \t]*CASE\\>" line)
        (setq case-line-p t))
      (forward-line 0)
      ;; Scan backward
      (while (and (not found-indent) (not (bobp)))
        (forward-line -1)
        (let ((l (thing-at-point 'line t)))
          (cond
           ;; Push END / END SELECT onto stack
           ((string-match-p "^[ \t]*END\\( SELECT\\)?\\>" l)
            (push l stack))
           ;; Handle SELECT
           ((string-match-p "^[ \t]*SELECT\\>" l)
            (if (and stack (string-match-p "END SELECT" (car stack)))
                (pop stack)
              (when (or (null stack) case-line-p)
                (setq found-indent (current-indentation))
                (setq case-line-p nil))))
           ;; Handle CASE line inside SELECT
           ((string-match-p "^[ \t]*CASE\\>" l)
            (when case-line-p
              ;; CASE is one level inside SELECT
              (setq found-indent (+ (current-indentation) tab-width))
              (setq case-line-p nil)))
           ;; Handle IF/FOR/WHILE/FUNCTION
           ((string-match-p "^[ \t]*\\(IF\\|FOR\\|WHILE\\|FUNCTION\\)\\>" l)
            (if (and stack (string-match-p "END" (car stack)))
                (pop stack)
              (when (null stack)
                (setq found-indent (current-indentation)))))
           ;; Ignore other lines
           )))
      ;; Fallback
      (or found-indent 0))))


;;;###autoload
(defun cicode-indent-line ()
  "Indent current line as Cicode code."
  (interactive)
  (let ((cur-indent 0))
    (beginning-of-line)
    (cond
     ;; END, END SELECT aligns with opener
     ((looking-at "^[ \t]*END\\( SELECT\\)?\\>")
      (setq cur-indent (or (cicode-find-matching-opener) 0)))
     ;; Previous line defines a function
     ((looking-at "^[ \t]*\\(\\w+[ \t]+\\)*FUNCTION\\>")
      (setq cur-indent 0))


     ;; CASE lines indented inside SELECT
     ((looking-at "^[ \t]*CASE\\>")
      (save-excursion
        (while (and (not (bobp))
                    (not (looking-at "^[ \t]*SELECT\\>")))
          (forward-line -1))
        (setq cur-indent (if (bobp)
                             tab-width
                           (+ (current-indentation) tab-width)))))

     ;; ELSE aligns with matching IF
     ((looking-at "^[ \t]*ELSE\\>")
      (save-excursion
        (while (and (not (bobp))
                    (not (looking-at "^[ \t]*IF\\>")))
          (forward-line -1))
        (setq cur-indent (if (bobp)
                             0
                           (current-indentation)))))

     ;; Normal line: check if previous line opens a block
     (t
      (save-excursion
        ;; Move back to the previous non-empty line
        (forward-line -1)
        (while (and (not (bobp))
                    (looking-at "^[ \t]*$"))  ;; skip empty/whitespace-only lines
          (forward-line -1))
        ;; Now check this non-empty line
        (cond
         ;; Previous line opens a block (IF ... THEN, FOR ... DO, WHILE ... DO, SELECT)
         ((looking-at "^[ \t]*\\(IF\\|FOR\\|WHILE\\|SELECT\\)\\>.*\\(THEN\\|DO\\)?$")
          (setq cur-indent (+ (current-indentation) tab-width)))

         ;; Previous line defines a function
         ((looking-at "^[ \t]*\\(\\w+[ \t]+\\)*FUNCTION\\>")
          (setq cur-indent (+ (current-indentation) tab-width)))

         ;; Previous line is CASE or ELSE
         ((looking-at "^[ \t]*\\(CASE\\|ELSE\\)\\>")
          (setq cur-indent (+ (current-indentation) tab-width)))

         ;; Otherwise continue previous indentation
         (t
          (setq cur-indent (current-indentation)))))))

    ;; Apply indentation
    (indent-line-to cur-indent)))


;;;###autoload
(defun cicode-newline-and-indent ()
  "Insert a newline and indent according to Cicode rules."
  (interactive)
  ;; Insert newline
  (newline)
  ;; Indent the new line
  (cicode-indent-line))

;;;###autoload
(defun cicode-mode-setup-keys ()
  "Set up the key map for cicode-mode."
  (define-key cicode-mode-map (kbd "RET") 'cicode-newline-and-indent))

(add-hook 'cicode-mode-hook 'cicode-mode-setup-keys)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ci\\'" . cicode-mode))


(require 'json)
(require 'ht)

(defvar cicode-mode-json-functions-in nil
  "Loaded hash table of builtin functions from JSON.")

(defvar cicode-hash-table-completion nil
  "Completion hash table derived from JSON.")

(cl-defstruct cicode-parameterstruct ParameterName ParameterDescription)

(require 'builtins)
;; Load JSON once
(let* ((json-object-type 'hash-table)
       (json-array-type 'list)
       (json-key-type 'string))
  (setq cicode-mode-json-functions-in
        ;; (json-read-file (expand-file-name builtins.json cicode-mode-base))))
        (json-parse-string cicode-mode-json-builtins-str)))


(defvar cicode-mode-max-docstring-width 80
  "The maximum width of a single line of docstring inside of completion popup, defaults to ‘80’.
Set this to the width of the info popup you use (i.e. if you use corfu, set this to be ‘corfu-popupinfo-max-width’)")


(defvar cicode-mode-max-docstring-length (* 3 (- cicode-mode-max-docstring-width 4))
  "The maximum length of docstring inside of completion")

(defun cicode-mode-trim-docstring (docstring)
  "Trim the DOCSTRING to a set length."
  (let ((max-length cicode-mode-max-docstring-length))
    (if (> (length docstring) max-length)
        (concat (substring docstring 0 (- max-length 3)) "...")
      docstring)))

(defun cicode-mode-make-function-completion-table (json-table)
  (let ((tab (make-hash-table :test 'equal)))
    (maphash
     (lambda (key val)
       (puthash (gethash "name" val) val tab))
     json-table)
    tab))

(defun cicode-mode-annotation (cand)
  "Return formatted signature for candidate CAND."
  (let* ((data (gethash cand cicode-hash-table-completion))
         (params (gethash "params" data))
         (paramNames (mapcar (lambda (p) (gethash "paramname" p)) params))
         (return (gethash "returnType" data)))
    (when data
      (format "(%s) → %s"
              (mapconcat #'identity paramNames ", ")
              return))))

(defun cicode-mode-find-split-pos (strIn pos)
  "Find the postion/index to split the string on.
STRIN: string to search for
POS: position to search backwards for whitespace from"
  (let ((currentpos (min pos (1- (length strIn)))))
    (while (and (>= currentpos 0)
                (not (member (aref strIn currentpos) '(?\s ?\t ?\n ?\r))))
      (setq currentpos (1- currentpos)))
    currentpos))

(defun cicode-mode-load-functions ()
  "Load completion data for cicode."
  (unless cicode-hash-table-completion
    (setq cicode-hash-table-completion
          (cicode-mode-make-function-completion-table cicode-mode-json-functions-in)))
  cicode-hash-table-completion)

(defun cicode-mode-custom-company-doc-buffer (&optional string)
  "Modified from company repo. STRING: Formatted string."
  (with-current-buffer (get-buffer-create "*company-documentation*")
    (erase-buffer)
    (fundamental-mode)
    (when string
      (save-excursion
        (insert string)
        (visual-line-mode)))
    (current-buffer)))

;; Older home grown system - was smaller and less in your face, but cicode inbuilts are annoying so went with new implemenetation for more details
;; (defun cicode-mode-format-cicode-parameterstructs-with-indent (structs)
;;   "Format a list of cicode-parameterstruct STRUCTS as aligned descriptions and line wrapping."
;;   (let* ((max-name-len (apply #'max (mapcar (lambda (s)
;;                                               (length (cicode-parameterstruct-ParameterName s)))
;;                                             structs)))
;;          (result ""))
;;     (dolist (s structs)
;;       (let* ((name (cicode-parameterstruct-ParameterName s))
;;              ;; (desc (cicode-mode-trim-parameter-docstring max-name-len (cicode-parameterstruct-ParameterDescription s)))
;;              (desc  (cicode-parameterstruct-ParameterDescription s))
;;              ;; compute indent after the colon, based on longest name
;;              (indent-col (+ max-name-len 3))
;;              (indent (make-string indent-col ?\s))
;;              (indentFirstLine (make-string (+ 2 (- max-name-len (length name))) ?\s))
;;              ;; replace newlines in description with proper hanging indent
;;              (formatted-desc (replace-regexp-in-string "\n" (concat "\n" indent) desc)))
;;         (setq result
;;               (concat result  " " (propertize name 'face 'font-lock-keyword-face)  ":" indentFirstLine (propertize formatted-desc 'face 'font-lock-doc-face ) "\n"))))
;;     result))

(defun cicode-mode-format-cicode-parameterstructs-with-indent (structs)
  "Format STRUCTS as aligned descriptions, wrapping lines and indenting properly.
Respects existing newlines without reprinting the parameter name."
  (let* ((max-name-len (apply #'max
                              (mapcar (lambda (s)
                                        (length (cicode-parameterstruct-ParameterName s)))
                                      structs)))
         (max-width cicode-mode-max-docstring-width)
         (result ""))
    (dolist (s structs)
      (let* ((name (cicode-parameterstruct-ParameterName s))
             (desc (or (cicode-parameterstruct-ParameterDescription s) ""))
             (first-line-indent (+ 2 (- max-name-len (length name))))
             (hanging-indent (+ max-name-len 3))
             (lines (split-string desc "\n"))
             (first-paragraph-line t))
        (dolist (line lines)
          (let* ((words (split-string line " " t))
                 ;; First line uses name + colon, others use hanging indent
                 (current-line (if first-paragraph-line
                                   (concat (propertize name 'face 'font-lock-keyword-face)
                                           ":"
                                           (make-string first-line-indent ?\s))
                                 (make-string hanging-indent ?\s)))
                 (line-len (length current-line)))
            (setq first-paragraph-line nil)
            ;; Process words with wrapping
            (dolist (w words)
              (if (> (+ line-len 1 (length w)) max-width)
                  (progn
                    (setq result (concat result current-line "\n"))
                    (setq current-line (concat (make-string hanging-indent ?\s) w))
                    (setq line-len (+ hanging-indent (length w))))
                (setq current-line (if (= line-len (if first-paragraph-line
                                                       (+ (length name) 1 first-line-indent)
                                                     hanging-indent))
                                       (concat current-line w)
                                     (concat current-line " " w)))
                (setq line-len (+ line-len 1 (length w)))))
            ;; Append last line of this paragraph
            (setq result (concat result current-line "\n"))))
        ;; Add a separator line after this parameter
        (setq result (concat result
                             (propertize (make-string max-width ?─)
                                         'face '(:foreground "gray50"))
                             "\n"))))
    result))


(defun cicode-doc-buffer (cand)
  "Return a buffer containing the docstring for CAND."
  (let* ((data (gethash cand cicode-hash-table-completion))
         (functionName (propertize (gethash "name" data)
                                   'face 'font-lock-keyword-face
                                   'face 'bold-italic))
         (returns (concat (propertize "Returns: " 'face 'font-lock-doc-markup-face)
                          (propertize (cicode-mode-trim-docstring (gethash "returnType" data))
                                      'face 'font-lock-doc-face)))
         (docstring (propertize (cicode-mode-trim-docstring (gethash "doc" data))
                                'face 'font-lock-doc-face))
         (syntax (concat (propertize "Syntax: " 'face 'font-lock-doc-markup-face)
                         (cicode-fontify-string (gethash "syntax" data))))
         (params (gethash "params" data))
         (paramNames (mapcar (lambda (p) (gethash "paramname" p)) params))
         (paramDesc (mapcar (lambda (p) (gethash "paramdescription" p)) params))
         (paramsListStruct (cl-mapcar
                            (lambda (name desc)
                              (make-cicode-parameterstruct
                               :ParameterName name
                               :ParameterDescription desc))
                            paramNames paramDesc))
         (paramsString (if paramsListStruct  (concat (propertize "Parameters:\n" 'face 'font-lock-doc-markup-face)
                                                     (cicode-mode-format-cicode-parameterstructs-with-indent
                                                      paramsListStruct)) "")))
    (cicode-mode-custom-company-doc-buffer
     (format "%s\n%s\n%s\n%s\n%s\n"
             functionName docstring returns syntax paramsString))))

(defun cicode-mode-capf ()
  "Capf for Cicpde."
  (cicode-mode-load-functions)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let* ((start (car bounds))
             (end   (cdr bounds))
             (collection cicode-hash-table-completion))
        (list start end collection
              :exclusive 'no
              :annotation-function #'cicode-mode-annotation
              :company-doc-buffer #'cicode-doc-buffer
              :completion-ignore-case nil)))))



(defun cicode-mode-setup-eldoc ()
  (setq-local eldoc-documentation-functions
              #'cicode-mode-eldoc-function)
  (eldoc-mode 1))

(defun cicode-mode-function-name-at-point ()
  ;; (or
  ;; Prefer symbol at point
  (let ((sym (thing-at-point 'symbol t)))
    (when (and sym (cicode-mode-get-function-info sym))
      sym)))
;; Otherwise walk backward - not sure i actually want this tbh
;; (cicode-find-enclosing-function)))



(defun cicode-find-enclosing-function ()
  "Return the first word immediately outside the nearest parentheses."
  (save-excursion
    ;; Move backward to nearest '('
    (when (search-backward "(" nil t)
      ;; Skip spaces before '('
      (skip-chars-backward " \t")
      ;; Now point is just after function name
      (let ((end (point)))
        ;; Move backward over word characters
        (while (and (> (point) (line-beginning-position))
                    (let ((c (char-before)))
                      (and c (or (and (>= c ?a) (<= c ?z))
                                 (and (>= c ?A) (<= c ?Z))
                                 (and (>= c ?0) (<= c ?9))
                                 (= c ?_)))))
          (backward-char))
        ;; Return the substring from start to end
        (buffer-substring-no-properties (point) end)))))


(defun cicode-mode-format-eldoc-full (name)
  "Basically just the same as completion, but with example"
  (let* ((data (gethash name cicode-hash-table-completion))
         (functionName (propertize (gethash "name" data)
                                   'face 'font-lock-keyword-face
                                   'face 'bold-italic))
         (returns (concat (propertize "Returns: " 'face 'font-lock-doc-markup-face)
                          (propertize  (gethash "returnType" data)
                                       'face 'font-lock-doc-face)))
         (docstring (propertize  (gethash "doc" data)
                                 'face 'font-lock-doc-face))
         (syntax (concat (propertize "Syntax: " 'face 'font-lock-doc-markup-face)
                         (cicode-fontify-string (gethash "syntax" data))))
         (params (gethash "params" data))
         (paramNames (mapcar (lambda (p) (gethash "paramname" p)) params))
         (paramDesc (mapcar (lambda (p) (gethash "paramdescription" p)) params))
         (paramsListStruct (cl-mapcar
                            (lambda (name desc)
                              (make-cicode-parameterstruct
                               :ParameterName name
                               :ParameterDescription desc))
                            paramNames paramDesc))
         (paramsString (if paramsListStruct  (concat (propertize "Parameters:\n" 'face 'font-lock-doc-markup-face)
                                                     (cicode-mode-format-cicode-parameterstructs-with-indent
                                                      paramsListStruct)) ""))
         (exampleString (concat (propertize "Example:\n" 'face 'font-lock-doc-markup-face)(cicode-fontify-string (gethash "example" data)))))
    (format "%s\n%s\n%s\n%s\n%s\n"
            syntax docstring returns paramsString exampleString)))

(defun cicode-mode-get-function-info (name)
  (gethash name cicode-hash-table-completion))


(defun cicode-mode-eldoc-function (&rest _ignored)
  (let* ((fname (cicode-mode-function-name-at-point))
         (info  (and fname
                     (gethash fname cicode-hash-table-completion))))
    (when info
      (cicode-mode-format-eldoc-full fname))))


(defun cicode-fontify-string (string)
  "Return STRING fontified using `cicode-mode`."
  (with-temp-buffer
    (cicode-mode)
    (insert string)
    (font-lock-ensure)
    (buffer-string)))

;; Used to create regex from hashtable
;; (defun cicode-mode-get-builtin-functions ()
;;   (regexp-opt (ht-map (lambda (key value) (gethash "name" value)) cicode-hash-table-completion) 'symbols))


(provide 'cicode-mode)
;;; cicode-mode.el ends here
