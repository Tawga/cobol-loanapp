       01  LSEARCHI.
           02  FILLER PIC X(12).
           02  SSNL    COMP  PIC  S9(4).
           02  SSNF    PICTURE X.
           02  FILLER REDEFINES SSNF.
             03 SSNA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SSNI  PIC X(11).
           02  APPIDL    COMP  PIC  S9(4).
           02  APPIDF    PICTURE X.
           02  FILLER REDEFINES APPIDF.
             03 APPIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  APPIDI  PIC X(8).
           02  APPLCNTL    COMP  PIC  S9(4).
           02  APPLCNTF    PICTURE X.
           02  FILLER REDEFINES APPLCNTF.
             03 APPLCNTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  APPLCNTI  PIC X(30).
           02  STATUSL    COMP  PIC  S9(4).
           02  STATUSF    PICTURE X.
           02  FILLER REDEFINES STATUSF.
             03 STATUSA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  STATUSI  PIC X(8).
           02  AMOUNTL    COMP  PIC  S9(4).
           02  AMOUNTF    PICTURE X.
           02  FILLER REDEFINES AMOUNTF.
             03 AMOUNTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMOUNTI  PIC X(8).
           02  PERIODL    COMP  PIC  S9(4).
           02  PERIODF    PICTURE X.
           02  FILLER REDEFINES PERIODF.
             03 PERIODA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PERIODI  PIC X(2).
           02  INTRSTL    COMP  PIC  S9(4).
           02  INTRSTF    PICTURE X.
           02  FILLER REDEFINES INTRSTF.
             03 INTRSTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  INTRSTI  PIC X(1).
           02  MNTHLYL    COMP  PIC  S9(4).
           02  MNTHLYF    PICTURE X.
           02  FILLER REDEFINES MNTHLYF.
             03 MNTHLYA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MNTHLYI  PIC X(11).
           02  TOTALL    COMP  PIC  S9(4).
           02  TOTALF    PICTURE X.
           02  FILLER REDEFINES TOTALF.
             03 TOTALA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TOTALI  PIC X(11).
           02  MESSAGEL    COMP  PIC  S9(4).
           02  MESSAGEF    PICTURE X.
           02  FILLER REDEFINES MESSAGEF.
             03 MESSAGEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MESSAGEI  PIC X(40).
       01  LSEARCHO REDEFINES LSEARCHI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  SSNC    PICTURE X.
           02  SSNH    PICTURE X.
           02  SSNO  PIC X(11).
           02  FILLER PICTURE X(3).
           02  APPIDC    PICTURE X.
           02  APPIDH    PICTURE X.
           02  APPIDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  APPLCNTC    PICTURE X.
           02  APPLCNTH    PICTURE X.
           02  APPLCNTO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  STATUSC    PICTURE X.
           02  STATUSH    PICTURE X.
           02  STATUSO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  AMOUNTC    PICTURE X.
           02  AMOUNTH    PICTURE X.
           02  AMOUNTO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PERIODC    PICTURE X.
           02  PERIODH    PICTURE X.
           02  PERIODO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  INTRSTC    PICTURE X.
           02  INTRSTH    PICTURE X.
           02  INTRSTO PIC 9.
           02  FILLER PICTURE X(3).
           02  MNTHLYC    PICTURE X.
           02  MNTHLYH    PICTURE X.
           02  MNTHLYO PIC ZZZZZZZZ.99.
           02  FILLER PICTURE X(3).
           02  TOTALC    PICTURE X.
           02  TOTALH    PICTURE X.
           02  TOTALO PIC ZZZZZZZZ.99.
           02  FILLER PICTURE X(3).
           02  MESSAGEC    PICTURE X.
           02  MESSAGEH    PICTURE X.
           02  MESSAGEO  PIC X(40).
