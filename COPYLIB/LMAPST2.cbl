       01  LFORMI.
           02  FILLER PIC X(12).
           02  SSNL    COMP  PIC  S9(4).
           02  SSNF    PICTURE X.
           02  FILLER REDEFINES SSNF.
             03 SSNA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SSNI  PIC 9(06).
           02  SVNTHL    COMP  PIC  S9(4).
           02  SVNTHF    PICTURE X.
           02  FILLER REDEFINES SVNTHF.
             03 SVNTHA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SVNTHI  PIC X(1).
           02  RESTL    COMP  PIC  S9(4).
           02  RESTF    PICTURE X.
           02  FILLER REDEFINES RESTF.
             03 RESTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  RESTI  PIC X(4).
           02  FNAMEL    COMP  PIC  S9(4).
           02  FNAMEF    PICTURE X.
           02  FILLER REDEFINES FNAMEF.
             03 FNAMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  FNAMEI  PIC X(15).
           02  LNAMEL    COMP  PIC  S9(4).
           02  LNAMEF    PICTURE X.
           02  FILLER REDEFINES LNAMEF.
             03 LNAMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LNAMEI  PIC X(15).
           02  ADDRESSL    COMP  PIC  S9(4).
           02  ADDRESSF    PICTURE X.
           02  FILLER REDEFINES ADDRESSF.
             03 ADDRESSA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ADDRESSI  PIC X(20).
           02  POSTCDL    COMP  PIC  S9(4).
           02  POSTCDF    PICTURE X.
           02  FILLER REDEFINES POSTCDF.
             03 POSTCDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  POSTCDI  PIC X(5).
           02  CITYL    COMP  PIC  S9(4).
           02  CITYF    PICTURE X.
           02  FILLER REDEFINES CITYF.
             03 CITYA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CITYI  PIC X(20).
           02  EMPSTSL    COMP  PIC  S9(4).
           02  EMPSTSF    PICTURE X.
           02  FILLER REDEFINES EMPSTSF.
             03 EMPSTSA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  EMPSTSI  PIC X(5).
           02  MOGIL    COMP  PIC  S9(4).
           02  MOGIF    PICTURE X.
           02  FILLER REDEFINES MOGIF.
             03 MOGIA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MOGII  PIC X(8).
           02  CLOANSL    COMP  PIC  S9(4).
           02  CLOANSF    PICTURE X.
           02  FILLER REDEFINES CLOANSF.
             03 CLOANSA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CLOANSI  PIC X(8).
           02  CASSTSL    COMP  PIC  S9(4).
           02  CASSTSF    PICTURE X.
           02  FILLER REDEFINES CASSTSF.
             03 CASSTSA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CASSTSI  PIC X(8).
           02  LAMNTL    COMP  PIC  S9(4).
           02  LAMNTF    PICTURE X.
           02  FILLER REDEFINES LAMNTF.
             03 LAMNTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LAMNTI  PIC X(8).
           02  LPRDL    COMP  PIC  S9(4).
           02  LPRDF    PICTURE X.
           02  FILLER REDEFINES LPRDF.
             03 LPRDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LPRDI  PIC X(2).
           02  USAGEL    COMP  PIC  S9(4).
           02  USAGEF    PICTURE X.
           02  FILLER REDEFINES USAGEF.
             03 USAGEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  USAGEI  PIC X(40).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGI  PIC X(40).
       01  LFORMO REDEFINES LFORMI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  SSNC    PICTURE X.
           02  SSNH    PICTURE X.
           02  SSNO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  SVNTHC    PICTURE X.
           02  SVNTHH    PICTURE X.
           02  SVNTHO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  RESTC    PICTURE X.
           02  RESTH    PICTURE X.
           02  RESTO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  FNAMEC    PICTURE X.
           02  FNAMEH    PICTURE X.
           02  FNAMEO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  LNAMEC    PICTURE X.
           02  LNAMEH    PICTURE X.
           02  LNAMEO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  ADDRESSC    PICTURE X.
           02  ADDRESSH    PICTURE X.
           02  ADDRESSO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  POSTCDC    PICTURE X.
           02  POSTCDH    PICTURE X.
           02  POSTCDO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  CITYC    PICTURE X.
           02  CITYH    PICTURE X.
           02  CITYO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  EMPSTSC    PICTURE X.
           02  EMPSTSH    PICTURE X.
           02  EMPSTSO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  MOGIC    PICTURE X.
           02  MOGIH    PICTURE X.
           02  MOGIO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  CLOANSC    PICTURE X.
           02  CLOANSH    PICTURE X.
           02  CLOANSO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  CASSTSC    PICTURE X.
           02  CASSTSH    PICTURE X.
           02  CASSTSO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  LAMNTC    PICTURE X.
           02  LAMNTH    PICTURE X.
           02  LAMNTO PIC ZZZZZZZ9.
           02  FILLER PICTURE X(3).
           02  LPRDC    PICTURE X.
           02  LPRDH    PICTURE X.
           02  LPRDO PIC Z9.
           02  FILLER PICTURE X(3).
           02  USAGEC    PICTURE X.
           02  USAGEH    PICTURE X.
           02  USAGEO  PIC X(40).
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(40).
