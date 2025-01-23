//BINDDB2 JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*PLIB    JCLLIB ORDER=(KALA15.LOANAPP.PROCLIB)
//*
//********************************************************************
//*        BIND
//********************************************************************
//BIND     EXEC PGM=IKJEFT01
//STEPLIB  DD  DISP=SHR,DSN=DSNA10.DBAG.SDSNEXIT
//         DD  DISP=SHR,DSN=DSNA10.SDSNLOAD
//DBRMLIB  DD  DSN=KALA15.LOANAPP.DBRMLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSTSIN  DD  *
DSN SYSTEM (DBAG   )
BIND  MEMBER    (LOANDB2 )  -
      PLAN      (KALA15A)   -
      ACTION    (REP)       -
      ISOLATION (CS)        -
      VALIDATE  (BIND)      -
      RELEASE   (COMMIT)    -
      QUALIFIER (KALA15 )   -
      ENCODING  (1047)
/*
