;;; cicode-mode.el --- major mode for cicode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Sebastian Gazey
;; Author: Sebastian Gazey
;; URL: https://github.com/Sebagabones/cicode-mode
;; Created: 2025
;; Version: 0.1
;; Keywords: cicode citect plant-scada aveva

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; A simple major mode for editing Cicode files (from Citect/Plant Scada)
;; It is very janky and cursed, I don't understand more than 25% of what is going on with it

;; KNOWN BUGS / LIMITATIONS
;; ~SELECT CASE~ indentation is very broken
;; I have not tried to implement support for any of the ~:#~ syntax

;; Feel free to open an issue (or even better make a contribution)
;; I am not good at elisp, and doubt I will be able to improve this package much ;)


;;; Code:
;;;###autoload
(define-derived-mode cicode-mode prog-mode "Cicode"
  "Major mode for editing Cicode files."

  ;; Comment syntax
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  ;; Syntax table setup
  (modify-syntax-entry ?/ ". 124b" cicode-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" cicode-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" cicode-mode-syntax-table)
  (modify-syntax-entry ?\\ "." cicode-mode-syntax-table)
  ;; Indentation
  (setq-local indent-line-function 'cicode-indent-line)
  ;; Custom face for type declarations with italics
  (defface cicode-type-face
    '((t (:inherit font-lock-number-face :slant italic)))
    "Face for Cicode type declarations with italic style."
    :group 'font-lock-faces)
  (defvar cicode-type-face 'cicode-type-face
    "Face for Cicode type declarations with italic style.")

  ;; Enhanced syntax highlighting for Cicode with italic types
  (setq-local font-lock-defaults
              '((;; Types (with italic style)
                 ("\\<\\(FLOAT\\|INT\\|OBJECT\\|REAL\\|STRING\\)\\>" . cicode-type-face)

                 ;; Numbers
                 ("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-preprocessor-face)
                 ("\\<[0-9]+\\(\\.[0-9]+\\)?\\s-*[eE]\\s-*[+-]?\\s-*[0-9]+\\>" . font-lock-preprocessor-face)


                 ;; Function calls
                 ("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\([^)]*\\))"
                  (1 font-lock-function-name-face))

                 ;; Keywords
                 ("\\<\\(AND\\|C\\(?:\\(?:AS\\|ICOD\\)E\\)\\|DO\\|E\\(?:LSE\\|ND\\)\\|F\\(?:OR\\|UNCTION\\)\\|GLOBAL\\|IF\\|MOD\\(?:ULE\\)?\\|NOT\\|OR\\|P\\(?:RIVATE\\|UBLIC\\)\\|RETURN\\|SELECT\\|T\\(?:HEN\\|O\\)\\|WHILE\\)\\>"
                  . font-lock-keyword-face)

                 ;; Function names
                 ("\\<FUNCTION\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-function-name-face)

                 ;; Built-in Functions
                 ("\\<\\(A\\(?:bs\\|cc\\(?:Control\\|umBrowse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\)\\|l\\(?:arm\\(?:Ac\\(?:k\\(?:Rec\\|Tag\\)?\\|tive\\)\\|C\\(?:atGetFormat\\|lear\\(?:Rec\\|Tag\\)?\\|o\\(?:mment\\(?:RecID\\)?\\|unt\\(?:\\(?:Equipmen\\|Lis\\)t\\)?\\)\\)\\|D\\(?:elete\\|isable\\(?:Rec\\|Tag\\)?\\|sp\\(?:Cluster\\(?:Add\\|\\(?:InUs\\|Remov\\)e\\)\\|Last\\|Next\\|Prev\\)?\\)\\|Enable\\(?:Rec\\|Tag\\)?\\|Fi\\(?:lter\\(?:Close\\|Edit\\(?:Append\\(?:Equipment\\)?\\|C\\(?:lose\\|ommit\\)\\|First\\|HasField\\|Last\\|Next\\|Open\\|Prev\\|Set\\)\\|Open\\)\\|rst\\(?:\\(?:Cat\\|Pri\\|Tag\\)Rec\\)\\)\\|Get\\(?:D\\(?:elay\\(?:Rec\\)?\\|sp\\)\\|Fi\\(?:eldRec\\|lterName\\)\\|Info\\|OrderbyKey\\|Threshold\\(?:Rec\\)?\\)\\|H\\(?:elp\\|ighestPriority\\)\\|List\\(?:Create\\|D\\(?:\\(?:estro\\|ispla\\)y\\)\\|Fill\\)\\|N\\(?:ext\\(?:\\(?:Cat\\|Pri\\|Tag\\)Rec\\)\\|otifyVarChange\\)\\|Query\\(?:\\(?:Firs\\|Nex\\)tRec\\)\\|S\\(?:aveType\\|et\\(?:Delay\\(?:Rec\\)?\\|Info\\|Priority\\(?:Rec\\)?\\|Query\\|Threshold\\(?:Rec\\)?\\)\\|plit\\|um\\(?:Append\\|Commit\\|Delete\\|Fi\\(?:nd\\(?:Exact\\)?\\|rst\\)\\|Get\\|Last\\|Next\\|Prev\\|S\\(?:\\(?:e\\|pli\\)t\\)\\|Type\\)\\)\\|TagFromEquipment\\)\\|m\\(?:Browse\\(?:Ack\\|Cl\\(?:ear\\|ose\\)\\|Disable\\|Enable\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|Summary\\(?:Ack\\|C\\(?:l\\(?:ear\\|ose\\)\\|ommit\\)\\|D\\(?:elete\\(?:All\\)?\\|isable\\)\\|Enable\\|First\\|GetField\\|Last\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\|SetFieldValue\\)\\|Tags\\(?:Ack\\|Cl\\(?:ear\\|ose\\)\\|Disable\\|Enable\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\)\\)\\|nByName\\|r\\(?:c\\(?:Cos\\|\\(?:Si\\|Ta\\)n\\)\\|eaCheck\\|ray\\(?:C\\(?:opy\\|reate\\(?:ByAn\\)?\\)\\|Destroy\\(?:ByAn\\)?\\|Exists\\(?:ByAn\\)?\\|FillFromAlarmDataByAn\\|Get\\(?:ArrayByAn\\|In\\(?:fo\\|t\\(?:ByAn\\)?\\)\\|MapName\\(?:ByAn\\)?\\|String\\(?:ByAn\\)?\\)\\|IsDirty\\|S\\(?:et\\(?:I\\(?:nt\\(?:ByAn\\)?\\|sDirty\\)\\|String\\(?:ByAn\\)?\\)\\|wap\\)\\)\\)\\|ss\\(?:Chain\\|EquipParameters\\|Get\\(?:Property\\|Scale\\)\\|Info\\(?:Ex\\)?\\|Metadata\\|ScaleStr\\|WinReplace\\|ert\\)?\\)\\|Beep\\|C\\(?:APIPost\\|allEvent\\|ha\\(?:inEvent\\|rToStr\\)\\|itect\\(?:ColourToPackedRGB\\|Info\\)\\|l\\(?:ip\\(?:Copy\\|Paste\\|ReadLn\\|SetMode\\|WriteLn\\)\\|uster\\(?:Activate\\|Deactivate\\|First\\|GetName\\|IsActive\\|Next\\|S\\(?:e\\(?:rverTypes\\|tName\\)\\|tatus\\|wapActive\\)\\)\\)\\|o\\(?:de\\(?:\\(?:SetMod\\|Trac\\)e\\)\\|m\\(?:Close\\|Open\\|Re\\(?:ad\\|set\\)\\|Write\\)\\|s\\)\\|reate\\(?:\\(?:Control\\)?Object\\)\\)\\|D\\(?:DE\\(?:Exec\\|Post\\|Read\\|Write\\|h\\(?:Execute\\|GetLastError\\|Initiate\\|Poke\\|Re\\(?:adLn\\|quest\\)\\|SetMode\\|Terminate\\|WriteLn\\)\\)\\|LL\\(?:C\\(?:all\\(?:Ex\\|OEM\\)?\\|lose\\)\\|Open\\)\\|ate\\(?:Add\\|Day\\(?:Month\\)?\\|Info\\|Month\\|Sub\\|WeekDay\\)\\|e\\(?:bug\\(?:Break\\)?\\|gToRad\\|v\\(?:Append\\|C\\(?:lose\\|ontrol\\|urr\\)\\|D\\(?:\\(?:elet\\|isabl\\)e\\)\\|EOF\\|F\\(?:i\\(?:nd\\|rst\\)\\|lush\\)\\|GetField\\|History\\|Info\\|Modify\\|Next\\|Open\\(?:Grp\\)?\\|Pr\\(?:ev\\|int\\)\\|Re\\(?:ad\\(?:Ln\\)?\\|cNo\\)\\|S\\(?:e\\(?:ek\\|tField\\)\\|ize\\)\\|Write\\(?:Ln\\)?\\|Zap\\)\\)\\|isplayRuntimeManager\\|llClass\\(?:C\\(?:allMethod\\|reate\\)\\|Dispose\\|GetProperty\\|IsValid\\|SetProperty\\)\\|riverInfo\\|sp\\(?:A\\(?:n\\(?:CreateControlObject\\|Free\\|Get\\(?:Area\\|Metadata\\(?:At\\)?\\|P\\(?:os\\|rivilege\\)\\)\\|In\\(?:Rgn\\|fo\\)\\|Move\\(?:Rel\\)?\\|New\\(?:Rel\\)?\\|Set\\(?:Metadata\\(?:At\\)?\\|Name\\)\\|Write\\)\\|rrayByAn\\)\\|B\\(?:ar\\(?:Load\\)?\\|mp\\|utton\\(?:Fn\\)?\\)\\|C\\(?:hart\\|learClip\\|ol\\)\\|D\\(?:el\\(?:ayRender\\(?:Begin\\|End\\)\\)?\\|irty\\)\\|Error\\|F\\(?:ile\\(?:Get\\(?:Info\\|Name\\)\\|S\\(?:croll\\|etName\\)\\)?\\|lushObj\\|ont\\(?:Hnd\\)?\\|ullScreen\\)\\|G\\(?:et\\(?:An\\(?:Cur\\|Extent\\|F\\(?:irst\\|rom\\(?:Name\\(?:Relative\\)?\\|Point\\)\\)\\|Height\\|Next\\|RawExtent\\|Width\\)\\|Env\\|M\\(?:etadataFromName\\(?:Relative\\)?\\|ouse\\(?:Over\\)?\\)\\|N\\(?:\\(?:ameFrom\\|earest\\)An\\)\\|ParentAn\\|Slider\\|Tip\\)\\|rayButton\\)\\|I\\(?:nfo\\(?:Destroy\\|Field\\|New\\|Valid\\)?\\|s\\(?:ButtonGray\\|Visible\\)\\)\\|Kernel\\|M\\(?:CI\\|arker\\(?:Move\\|New\\)\\)\\|P\\(?:age\\|laySound\\|opupMenu\\)\\|R\\(?:ichText\\(?:E\\(?:dit\\|nable\\)\\|GetInfo\\|Load\\|P\\(?:gScroll\\|rint\\)\\|S\\(?:ave\\|croll\\)\\)?\\|ub\\(?:End\\|Move\\|S\\(?:etClip\\|tart\\)\\)\\)\\|S\\(?:et\\(?:C\\(?:lip\\|urColor\\)\\|MetadataFromName\\(?:Relative\\)?\\|PopupMenuFont\\|Slider\\|T\\(?:ip\\|ooltipFont\\)\\)\\|how\\|t\\(?:atus\\|r\\)\\|ym\\(?:A\\(?:nm\\(?:Ex\\)?\\|tSize\\)\\|Load\\)?\\)\\|T\\(?:ext\\|ipMode\\|r\\(?:end\\(?:Info\\)?\\|nLoad\\)\\)\\|Verbose\\)\\|umpKernel\\)\\|E\\(?:n\\(?:gToGeneric\\|terCriticalSection\\)\\|quip\\(?:Browse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|CheckUpdate\\|GetP\\(?:arameter\\|roperty\\)\\|RefBrowse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|S\\(?:etProperty\\|tateBrowse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\)\\)\\|rr\\(?:Com\\|Drv\\|GetHw\\|Help\\|Info\\|Log\\|Msg\\|Set\\(?:Hw\\|Level\\)?\\|Trap\\)\\|x\\(?:ec\\(?:uteDTSPkg\\)?\\|p\\)\\)\\|F\\(?:act\\|ile\\(?:C\\(?:lose\\|opy\\)\\|Delete\\|E\\(?:OF\\|xist\\)\\|Find\\(?:Close\\)?\\|GetTime\\|MakePath\\|Open\\|R\\(?:e\\(?:ad\\(?:Block\\|Ln\\)?\\|name\\)\\|ichTextPrint\\)\\|S\\(?:e\\(?:ek\\|tTime\\)\\|ize\\|plitPath\\)\\|Write\\(?:Block\\|Ln\\)?\\)\\|lashColourState\\|mt\\(?:Close\\|FieldHnd\\|GetField\\(?:Count\\|Hnd\\|Name\\|Width\\)?\\|Open\\|SetField\\(?:Hnd\\)?\\|ToStr\\)\\|orm\\(?:A\\(?:ctive\\|ddList\\)\\|Button\\|C\\(?:heckBox\\|omboBox\\|urr\\)\\|Destroy\\|Edit\\|Field\\|G\\(?:et\\(?:CurrInst\\|Data\\|\\(?:Ins\\|Tex\\)t\\)\\|oto\\|roupBox\\)\\|Input\\|List\\(?:AddText\\|Box\\|\\(?:Delete\\|Select\\)Text\\)\\|New\\|OpenFile\\|P\\(?:assword\\|osition\\|rompt\\)\\|R\\(?:adioButton\\|ead\\)\\|S\\(?:aveAsFile\\|e\\(?:curePassword\\|lectPrinter\\|t\\(?:Data\\|\\(?:Ins\\|Tex\\)t\\)\\)\\)\\|WndHnd\\)\\|tp\\(?:Close\\|File\\(?:Copy\\|Find\\(?:Close\\)?\\)\\|Open\\)\\|u\\(?:llName\\|zzy\\(?:Close\\|Get\\(?:\\(?:Code\\|Shell\\)Value\\)\\|Open\\|\\(?:Set\\(?:\\(?:Code\\|Shell\\)Valu\\)\\|Trac\\)e\\)\\)\\)\\|G\\(?:et\\(?:Area\\|BlueValue\\|E\\(?:nv\\|vent\\)\\|G\\(?:lb\\(?:Bool\\|Flt\\|Int\\|Str\\)\\|reenValue\\)\\|L\\(?:anguage\\|ogging\\)\\|Priv\\|RedValue\\|Var\\(?:Def\\|Str\\(?:Def\\)?\\)?\\|WinTitle\\)\\|r\\(?:aph\\(?:Box\\|Close\\|G\\(?:etInfo\\|rid\\)\\|Line\\|Marker\\|Open\\|ScaleMarker\\|Text\\)\\|p\\(?:Close\\|Delete\\|First\\|In\\(?:sert\\)?\\|Math\\|N\\(?:ame\\|ext\\)\\|Open\\|ToStr\\)\\)\\)\\|H\\(?:alt\\|exToStr\\|igh\\(?:Byte\\|Word\\)\\|tmlHelp\\)\\|I\\(?:FDEF\\(?:\\(?:A\\(?:DV\\|NA\\)\\|DIG\\)ALM\\)?\\|ODevice\\(?:Control\\|Info\\)\\|n\\(?:AnimationCycle\\|CommunicationsCycle\\|put\\|tTo\\(?:Real\\|Str\\)\\)\\|sError\\)\\|Ke\\(?:r\\(?:Cmd\\|nel\\(?:QueueLength\\|TableI\\(?:nfo\\|temCount\\)\\)\\)\\|y\\(?:AllowCursor\\|BS\\|Down\\|Get\\(?:Cursor\\)?\\|Left\\|Move\\|OEM\\|P\\(?:eek\\|ut\\(?:Str\\)?\\)\\|R\\(?:eplay\\(?:All\\)?\\|ight\\)\\|Set\\(?:Cursor\\|Seq\\|Type\\)\\|Up\\)\\)\\|L\\(?:anguageFileTranslate\\|eaveCriticalSection\\|ine\\(?:Answer\\|Close\\|Drop\\|Info\\|MakeCall\\|Open\\)\\|n\\|o\\(?:g\\(?:in\\|out\\)?\\|w\\(?:Byte\\|Word\\)\\)\\)\\|M\\(?:a\\(?:il\\(?:Error\\|Logo\\(?:ff\\|n\\)\\|\\(?:Rea\\|Sen\\)d\\)\\|p\\(?:Cl\\(?:ear\\|ose\\)\\|Exists\\|Key\\(?:Count\\|Delete\\|Exists\\|\\(?:Firs\\|Nex\\)t\\)\\|Open\\|Value\\(?:Get\\|Set\\(?:Quality\\)?\\)\\)\\|x\\)\\|e\\(?:nu\\(?:Get\\(?:Child\\|FirstChild\\|GenericNode\\|N\\(?:extChild\\|odeByPath\\)\\|P\\(?:a\\(?:geNode\\|rent\\)\\|revChild\\)\\|WindowNode\\)\\|Node\\(?:AddChild\\|Get\\(?:Curr\\|Depth\\|Expanded\\|Property\\|TargetPage\\)\\|HasCommand\\|Is\\(?:Disabled\\|Hidden\\)\\|R\\(?:emove\\|unCommand\\)\\|Set\\(?:DisabledWhen\\|Expanded\\|HiddenWhen\\|Property\\)\\)\\|Reload\\)\\|ssage\\)\\|in\\|sg\\(?:Brdcst\\|Close\\|GetCurr\\|Open\\|R\\(?:PC\\|ead\\)\\|\\(?:Sta\\|Wri\\)te\\)\\|ultiMonitorStart\\)\\|Name\\|O\\(?:LEDateToTime\\|bject\\(?:Associate\\(?:Events\\|PropertyWithTag\\)\\|ByName\\(?:Ex\\)?\\|HasInterface\\|IsValid\\|Nothing\\|ResetState\\|SaveState\\|ToStr\\)\\|nEvent\\)\\|P\\(?:a\\(?:ckedRGB\\(?:ToCitectColour\\)?\\|ge\\(?:Back\\|Exists\\|F\\(?:ileInfo\\(?:Ex\\)?\\|orward\\)\\|Get\\(?:Int\\|Str\\)\\|Home\\|Info\\|L\\(?:ast\\|ist\\(?:C\\(?:\\(?:ou\\|urre\\)nt\\)\\|D\\(?:elete\\|isplay\\)\\|Info\\)\\)\\|Next\\|P\\(?:eek\\(?:\\(?:Curren\\|Las\\)t\\)\\|opLast\\|rev\\|ushLast\\)\\|Recall\\|Set\\(?:Int\\|Str\\)\\|T\\(?:ask\\|ransformCoords\\)\\)\\|rameter\\(?:\\(?:Ge\\|Pu\\)t\\)\\|ssword\\(?:\\(?:De\\|En\\)crypt\\)\\|thToStr\\)\\|i\\|lot\\(?:Close\\|Draw\\|G\\(?:etMarker\\|rid\\)\\|Info\\|Line\\|Marker\\|Open\\|S\\(?:\\(?:cale\\|et\\)Marker\\)\\|Text\\|XYLine\\)\\|o\\(?:int\\(?:Data\\|Free\\|New\\|Read\\|Status\\|Write\\(?:Array\\(?:Long\\|Real\\)\\)?\\)\\|w\\)\\|r\\(?:int\\(?:Font\\|Ln\\)?\\|o\\(?:cessRestart\\|ductInfo\\|ject\\(?:Info\\|\\(?:Restart[GS]\\|S\\)et\\)\\|mpt\\)\\)\\|u\\(?:lse\\|rgeMemory\\)\\)\\|Qu\\(?:ality\\(?:Create\\|GetPart\\|Is\\(?:Bad\\|ControlInhibit\\|Good\\|Override\\|Uncertain\\)\\|SetPart\\|ToStr\\)\\|e\\(?:Close\\|Length\\|Open\\|Peek\\|Read\\|Write\\)\\)\\|R\\(?:a\\(?:dToDeg\\|nd\\)\\|db\\(?:Close\\|EOF\\|Fi\\(?:nd\\|rstRec\\)\\|Get\\(?:Path\\)?\\|N\\(?:\\(?:ext\\|o\\)Rec\\)\\|Open\\(?:Page\\|Sub\\)?\\|P\\(?:\\(?:os\\|rev\\)Rec\\)\\|Set\\(?:PathRdbLastRec\\)?\\)\\|e\\(?:Read\\|alToStr\\|p\\(?:GetC\\(?:luster\\|ontrol\\)\\|SetControl\\|ort\\)\\|setScreenProfile\\)\\|ound\\)\\|S\\(?:OE\\(?:Archive\\|Dismount\\|EventAdd\\|Mount\\)\\|PC\\(?:Alarms\\|Client\\(?:Info\\|TableGet\\)\\|Get\\(?:\\(?:Histogram\\|Subgroup\\)Table\\)\\|\\(?:ProcessXRS\\(?:[GS]e\\)\\|S\\(?:etLimi\\|\\(?:pecLimit[GS]\\|ubgroupSize[GS]\\)e\\)\\)t\\)\\|QL\\(?:Append\\|BeginTran\\|C\\(?:a\\(?:\\(?:l\\|nce\\)l\\)\\|lose\\|o\\(?:\\(?:mmi\\|nnec\\)t\\)\\|reate\\)\\|Dis\\(?:connect\\|pose\\)\\|E\\(?:nd\\|rrMsg\\|xec\\)\\|FieldInfo\\|Get\\(?:Field\\|Recordset\\|Scalar\\)\\|I\\(?:nfo\\|sNullField\\)\\|N\\(?:ext\\|oFields\\|um\\(?:Change\\|Fields\\)\\)\\|Open\\|P\\(?:arams\\(?:ClearAll\\|SetAs\\(?:Int\\|Real\\|String\\)\\)\\|rev\\)\\|Query\\(?:\\(?:Creat\\|Dispos\\)e\\)\\|Ro\\(?:llBack\\|wCount\\)\\|Set\\|TraceO\\(?:ff\\|n\\)\\)\\|ch\\(?:d\\(?:C\\(?:lose\\|onfig\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\)\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\|Special\\(?:Add\\|Close\\|Delete\\|First\\|GetField\\|Item\\(?:Add\\(?:Range\\)?\\|Close\\|Delete\\(?:Range\\)?\\|First\\|GetField\\|Modify\\(?:Range\\)?\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|Modify\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\)\\|eduleItem\\(?:Add\\|Delete\\|Modify\\|SetRepeat\\)\\)\\|e\\(?:m\\(?:Close\\|Open\\|Signal\\|Wait\\)\\|ndKeys\\|rv\\(?:er\\(?:Browse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|Control\\|DumpKernel\\|GetProperty\\|I\\(?:nfo\\(?:Ex\\)?\\|sOnline\\)\\|R\\(?:PC\\|e\\(?:load\\|start\\)\\)\\)\\|iceGetList\\)\\|t\\(?:Area\\|Event\\|Glb\\(?:Bool\\|Flt\\|Int\\|Str\\)\\|L\\(?:anguage\\|ogging\\)\\)\\)\\|hutdown\\(?:Mode\\)?\\|i\\(?:g?n\\)\\|leep\\(?:MS\\)?\\|qrt\\|tr\\(?:C\\(?:alcWidth\\|lean\\)\\|F\\(?:ill\\|ormat\\)\\|GetChar\\|L\\(?:e\\(?:ft\\|ngth\\)\\|ower\\)\\|Mid\\|Pad\\|R\\(?:eplace\\|ight\\)\\|Se\\(?:arch\\|tChar\\)\\|T\\(?:o\\(?:Char\\|Date\\|Fmt\\|Grp\\|Hex\\|Int\\|L\\(?:ines\\|ocalText\\)\\|Period\\|Real\\|Time\\(?:stamp\\)?\\|Value\\)\\|r\\(?:im\\|uncFont\\(?:Hnd\\)?\\)\\)\\|Upper\\|Word\\)\\|ubscription\\(?:AddCallback\\|Get\\(?:Attribute\\|Info\\|Quality\\|T\\(?:ag\\|imestamp\\)\\|Value\\)\\|RemoveCallback\\)\\|witchConfig\\|ysTime\\(?:Delta\\)?\\)\\|T\\(?:a\\(?:ble\\(?:LookUp\\|Math\\|Shift\\)\\|g\\(?:Browse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|Debug\\(?:Form\\)?\\|Get\\(?:Property\\|\\(?:Scal\\|Valu\\)e\\)\\|Info\\(?:Ex\\)?\\|R\\(?:DBReload\\|e\\(?:ad\\(?:Ex\\)?\\|solve\\)\\)\\|S\\(?:caleStr\\|etOverride\\(?:Bad\\|Good\\|Quality\\|Uncertain\\)\\|ubscribe\\)\\|Un\\(?:\\(?:resolv\\|subscrib\\)e\\)\\|Write\\(?:\\(?:Int\\|Real\\)Array\\)?\\)\\|n\\|sk\\(?:C\\(?:all\\|luster\\)\\|GetSignal\\|Hnd\\|Kill\\|New\\(?:Ex\\)?\\|Resume\\|S\\(?:etSignal\\|uspend\\)\\)\\)\\|est\\(?:Dbl2Flt\\|RandomWave\\|S\\(?:awWave\\|inWave\\|quareWave\\|tringNULL\\)\\|TriangWave\\|Variant\\(?:INT\\|NULL\\|QUALITY\\|REAL\\|STRING\\(?:NULL\\)?\\|TIMESTAMP\\)\\)\\|ime\\(?:Current\\|Hour\\|In\\(?:fo\\|tToTimestamp\\)\\|Mi\\(?:dNight\\|n\\)\\|Se\\(?:cond\\|[ct]\\)\\|To\\(?:OLEDate\\|Str\\(?:Fmt\\)?\\)\\|UTCOffset\\|YearDay\\|stamp\\(?:Add\\|C\\(?:reate\\|urrent\\)\\|Difference\\|Format\\|GetPart\\|Sub\\|To\\(?:Str\\|TimeInt\\)\\)\\)\\|oggle\\|r\\(?:aceMsg\\|n\\(?:AddHistory\\|Browse\\(?:Close\\|First\\|GetField\\|N\\(?:ext\\|umRecords\\)\\|Open\\|Prev\\)\\|ClientInfo\\|Del\\(?:History\\|ete\\)\\|E\\(?:cho\\|vent\\(?:GetTable\\(?:MS\\)?\\|SetTable\\(?:MS\\)?\\)\\)\\|Flush\\|Get\\(?:Buf\\(?:Event\\|\\(?:MSTim\\|Tim\\|Valu\\)e\\)\\|C\\(?:luster\\|ursor\\(?:Event\\|MSTime\\|Pos\\|Time\\|Value\\(?:Str\\)?\\)\\)\\|D\\(?:\\(?:efScal\\|isplayMod\\)e\\)\\|Event\\|Format\\|GatedValue\\|InvalidValue\\|M\\(?:\\(?:STim\\|od\\)e\\)\\|Pe\\(?:n\\(?:Comment\\|Focus\\|No\\)?\\|riod\\)\\|S\\(?:cale\\(?:Str\\)?\\|pan\\)\\|T\\(?:\\(?:abl\\|im\\)e\\)\\|Units\\)\\|I\\(?:nfo\\|sValidValue\\)\\|New\\|S\\(?:croll\\|e\\(?:lect\\|t\\(?:Cursor\\(?:Pos\\)?\\|DisplayMode\\|Event\\|Pe\\(?:n\\(?:Focus\\)?\\|riod\\)\\|S\\(?:cale\\|pan\\)\\|T\\(?:\\(?:abl\\|im\\)e\\)\\)\\)\\)\\)\\)\\)\\|U\\(?:nit\\(?:Control\\|Info\\|Stats\\)\\|ser\\(?:Create\\|Delete\\|Info\\|Login\\|Password\\(?:ExpiryDays\\)?\\|SetStr\\|UpdateRecord\\|Verify\\)\\)\\|V\\(?:ar\\(?:To\\(?:ArrayIndex\\|Str\\)\\|iable\\(?:Quality\\|T\\(?:imestamp\\|oStr\\)\\)\\)\\|\\(?:bCall\\(?:Ope\\|Retur\\)\\|ersio\\)n\\)\\|W\\(?:hoAmI\\|in\\(?:Copy\\|F\\(?:ile\\|ree\\(?:Ex\\)?\\)\\|G\\(?:et\\(?:Clicked\\|F\\(?:irstChild\\|ocus\\)\\|N\\(?:ame\\|extChild\\)\\|Parent\\|WndHnd\\)\\|oto\\)\\|Mo\\(?:[dv]e\\)\\|N\\(?:e\\(?:w\\(?:\\(?:Pin\\)?At\\)?\\|xt\\)\\|umber\\)\\|P\\(?:os\\|r\\(?:ev\\|int\\(?:File\\)?\\)\\)\\|S\\(?:e\\(?:lect\\|tName\\)\\|\\(?:iz\\|tyl\\)e\\)\\)\\|nd\\(?:Find\\|Get\\(?:\\(?:File\\)?Profile\\)\\|Help\\|Info\\|MonitorInfo\\(?:Ex\\)?\\|Put\\(?:\\(?:File\\)?Profile\\)\\|Show\\|Viewer\\)\\)\\|XML\\(?:C\\(?:\\(?:los\\|reat\\)e\\)\\|Get\\(?:Attribute\\(?:Count\\|\\(?:Nam\\|Valu\\)e\\)?\\|Child\\(?:Count\\)?\\|\\(?:Paren\\|Roo\\)t\\)\\|Node\\(?:AddChild\\|Find\\|\\(?:Get\\(?:Nam\\|Valu\\)\\|Remov\\|SetValu\\)e\\)\\|Open\\|S\\(?:\\(?:av\\|etAttribut\\)e\\)\\)\\|_\\(?:AlarmQuery\\(?:\\(?:Firs\\|Nex\\)tRec\\)\\|CreateControlObject\\|Dsp\\(?:Button\\(?:Fn\\)?\\|Cursor_Mouse\\(?:Down\\|Up\\)\\|Exec\\|FixedWidthText\\|ObjectGetCursor\\(?:Down\\|Up\\)?\\)\\|LibControl_\\(?:Get\\(?:ANName\\|Int\\(?:_ByAN\\)?\\|Str\\(?:_ByAN\\)?\\)\\|S\\(?:etCallbackArg\\|tr\\(?:Array_\\(?:Get\\(?:Int\\|Len\\)\\|Set\\(?:Int\\|Len\\)\\)\\|Replace\\)\\)\\)\\|Object\\(?:CallMethod\\|GetProperty\\|Se\\(?:rverInvoke\\(?:Ex\\)?\\|tProperty\\)\\)\\|Page\\(?:Display\\|Goto\\)\\|RunTests?\\|TimeSub\\|Vb\\(?:\\(?:C\\(?:allRu\\|icodeCallRetur\\)\\|ExpressionOpe\\)n\\)\\|W\\(?:\\(?:av\\|inTitl\\)e\\)\\|_\\(?:ObjectMasterAN\\|Tab\\(?:Alarm_GetIDByAN\\|menu_GetMenuConfigDisabled\\)\\)\\)\\)\\>" . font-lock-builtin-face)

                 ;; Operators and delimiters
                 ("\\(\\+=\\|-=\\|\\*=\\|/=\\|%=\\|&=\\|\\^=\\||=\\|<<\\|>>\\|==\\|!=\\|<=\\|>=\\|<\\|>\\|=\\|\\+\\|-\\|\\*\\|/\\|%\\|&\\|\\^\\||\\)" . font-lock-preprocessor-face)


                 ;; String literals
                 ("\"[^\"]*\"" . font-lock-string-face)

                 ;; Comments
                 ("//.*$" . font-lock-comment-face))

                nil ; KEYWORDS-ONLY
                t   ; CASE-FOLD
                nil ; SYNTAX-ALIST
                nil ; SYNTAX-BEGIN
                )))

;; Function to find matching opening block
(defun cicode-find-matching-opener ()
  "Find the matching opening block statement."
  (let ((block-level 0) (found-match nil) (opener-indent nil))
    (while (and (not found-match) (not (bobp)))
      (forward-line -1)
      (cond
       ((looking-at "^[ \t]*\\(END\\)\\>")
        (setq block-level (1+ block-level)))
       ((looking-at "^[ \t]*\\(IF\\|FOR\\|WHILE\\|FUNCTION\\|SELECT\\|CASE\\)\\>")
        (if (= block-level 0)
            (progn
              (setq opener-indent (current-indentation))
              (setq found-match t))
          (setq block-level (1- block-level))))))
    opener-indent))

;; Add hook for auto-indent after newline
;;;###autoload
(defun cicode-newline-and-indent ()
  "Insert a newline and indent properly."
  (interactive)
  (let ((was-after-end nil) (end-indent nil))
    ;; Check if we're right after an END
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^[ \t]*\\(END\\)\\>[ \t]*$")
        (setq was-after-end t)
        (setq end-indent (current-indentation))))
    ;; Insert new line
    (newline)

    ;; Handle special case after END
    (if was-after-end
        (let ((parent-indent (save-excursion
                               (forward-line -1)
                               (cicode-find-matching-opener))))
          (if parent-indent
              (indent-line-to parent-indent)
            (indent-line-to 0)))
      ;; Normal indentation
      (cicode-indent-line))))

;; Indentation function
;;;###autoload
(defun cicode-indent-line ()
  "Indent current line as Cicode code."
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; Check for beginning of buffer
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (cond
       ;; Handle END - should align with the matching IF/FOR/WHILE/FUNCTION
       ((looking-at "^[ \t]*\\(END\\)\\>")
        (let ((opener-indent (save-excursion (cicode-find-matching-opener))))
          (if opener-indent
              (setq cur-indent opener-indent)
            (setq cur-indent 0))))

       ;; Handle ELSE - should align with the matching IF
       ((looking-at "^[ \t]*\\(ELSE\\)\\>")
        (save-excursion
          (let ((block-level 0) (found-match nil))
            (while (and (not found-match) (not (bobp)))
              (forward-line -1)
              (cond
               ((looking-at "^[ \t]*\\(END\\)\\>")
                (setq block-level (1+ block-level)))
               ((looking-at "^[ \t]*\\(IF\\)\\>")
                (if (= block-level 0)
                    (progn
                      (setq cur-indent (current-indentation))
                      (setq found-match t))
                  (setq block-level (1- block-level))))))))
        (if (not cur-indent) (setq cur-indent 0)))

       ;; Check if line is right after an END
       ((save-excursion
          (forward-line -1)
          (beginning-of-line)
          (looking-at "^[ \t]*\\(END\\)\\>[ \t]*$"))
        (save-excursion
          (forward-line -1)
          (let ((opener-indent (cicode-find-matching-opener)))
            (when opener-indent
              (setq cur-indent opener-indent)
              (setq not-indented nil)))))

       ;; Other cases - check for block openers to follow
       (t
        (save-excursion
          (forward-line -1)
          (cond
           ;; If previous line is a block opener (FUNCTION or control structure with THEN/DO)
           ((or (looking-at "^[ \t]*\\(FUNCTION\\)>.*")
                (looking-at "^[ \t]*\\(IF\\|FOR\\|WHILE\\|SELECT\\)\\>.*\\(THEN\\|DO\\|CASE\\)\\>"))
            (setq cur-indent (+ (current-indentation) tab-width))
            (setq not-indented nil))

           ;; If previous line is ELSE alone or with THEN
           ((looking-at "^[ \t]*\\(ELSE\\|CASE\\)\\>")
            (setq cur-indent (+ (current-indentation) tab-width))
            (setq not-indented nil))

           ;; If none of the above specific cases matched
           (t
            (save-excursion
              (while not-indented
                (forward-line -1)
                (cond
                 ((bobp)
                  ;; Beginning of buffer
                  (setq cur-indent 0)
                  (setq not-indented nil))

                 ;; Look for containing block
                 ((looking-at "^[ \t]*\\(IF\\|FOR\\|WHILE\\|FUNCTION\\|ELSE\\|SELECT\\|CASE\\)\\>")
                  (setq cur-indent (+ (current-indentation) tab-width))
                  (setq not-indented nil))

                 ;; Look for END which would close a level
                 ((looking-at "^[ \t]*\\(END\\)\\>")
                  ;; Find the opener for this END and use its indentation
                  (let ((opener-indent (cicode-find-matching-opener)))
                    (if opener-indent
                        (setq cur-indent opener-indent)
                      (setq cur-indent (current-indentation))))
                  (setq not-indented nil))

                 ;; Skip empty lines
                 ((looking-at "^[ \t]*$")
                  nil)

                 ;; Default: use indentation of previous non-empty line
                 (t
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))))))))))

      ;; Apply the indentation
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

;;;###autoload
(defun cicode-mode-setup-keys ()
  "Set up the key map for cicode-mode."
  (define-key cicode-mode-map (kbd "RET") 'cicode-newline-and-indent))

(add-hook 'cicode-mode-hook 'cicode-mode-setup-keys)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ci\\'" . cicode-mode))

(provide 'cicode-mode)
;;; cicode-mode.el ends here
