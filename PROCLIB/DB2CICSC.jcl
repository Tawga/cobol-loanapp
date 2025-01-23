//********************************************************************
//*        DB2CICS - DB2 precompile,                                 *
//*                  CICS translator,                                *
//*                  IBM COBOL compile,                              *
//*                  Link edit                                       *
//********************************************************************
//DB2CICSC PROC DCLGLIB=KALA15.LOANAPP.COPYLIB,
//             COPYLIB=KALA15.LOANAPP.COPYLIB,
//             DBRMLIB=KALA15.LOANAPP.DBRMLIB,
//             SRCLIB=KALA15.LOANAPP.COBOL,
//             MEMBER='ERROR.MEMBER'
//********************************************************************
//*        DB2 Precompile IBM COBOL program                          *
//********************************************************************
//PC       EXEC PGM=DSNHPC,
//             PARM='HOST(IBMCOB),XREF,SOURCE,FLAG(I),APOST'
//STEPLIB  DD  DISP=SHR,DSN=DSNA10.DBAG.SDSNEXIT
//         DD  DISP=SHR,DSN=DSNA10.SDSNLOAD
//SYSCIN   DD  DSN=&&DSNHOUT,DISP=(MOD,PASS),UNIT=SYSDA,
//             SPACE=(800,(500,500))
//SYSIN    DD  DSN=&SRCLIB(&MEMBER),
//             DISP=SHR
//SYSLIB   DD  DSN=&DCLGLIB,
//             DISP=SHR
//         DD  DSN=&COPYLIB,
//             DISP=SHR
//DBRMLIB  DD  DSN=&DBRMLIB(&MEMBER),
//             DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSUT1   DD  SPACE=(800,(500,500),,,ROUND),UNIT=SYSDA
//SYSUT2   DD  SPACE=(800,(500,500),,,ROUND),UNIT=SYSDA
//*
//********************************************************************
//*        CICS Translator                                           *
//*        if the precompile return code is 4 or less                *
//********************************************************************
//TRN    EXEC PGM=DFHECP1$,
//            PARM='COBOL3',
//            REGION=4M,
//            COND=(4,LT,PC)
//STEPLIB  DD DSN=DFH420.CICS.SDFHLOAD,DISP=SHR
//SYSIN    DD DSN=&&DSNHOUT,DISP=(OLD,DELETE)
//SYSPRINT DD SYSOUT=*
//SYSPUNCH DD DSN=&&SYSCIN,
//            DISP=(,PASS),UNIT=SYSDA,
//            DCB=BLKSIZE=400,
//            SPACE=(400,(400,100))
//*
//********************************************************************
//*        Compile the IBM COBOL program if the precompile           *
//*        return code is 4 or less.                                 *
//********************************************************************
//COB    EXEC PGM=IGYCRCTL,REGION=4M,COND=((4,LT,PC),(4,LT,TRN)),
//  PARM='NODYNAM,LIB,OBJECT,RES,APOST,MAP,XREF,NOSEQUENCE'
//STEPLIB  DD DSN=IGY420.SIGYCOMP,DISP=SHR
//SYSLIB   DD DSN=&COPYLIB,DISP=SHR
//         DD DSN=DFH420.CICS.SDFHCOB,DISP=SHR
//         DD DSN=DFH420.CICS.SDFHMAC,DISP=SHR
//         DD DSN=DFH420.CICS.SDFHSAMP,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DSN=&&SYSCIN,DISP=(OLD,DELETE)
//SYSLIN   DD DSN=&&LOADSET,DISP=(MOD,PASS),
//            UNIT=SYSDA,SPACE=(80,(250,100))
//SYSUT1   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT2   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT3   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT4   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT5   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT6   DD UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT7   DD UNIT=SYSDA,SPACE=(460,(350,100))
//*
//COPYLINK EXEC PGM=IEBGENER,COND=(7,LT,COB)
//SYSUT1   DD DSN=DFH420.CICS.SDFHSAMP(DFHEILID),DISP=SHR
//SYSUT2   DD DSN=&&COPYLINK,DISP=(NEW,PASS),
//            DCB=(LRECL=80,BLKSIZE=400,RECFM=FB),
//            UNIT=SYSDA,SPACE=(400,(20,20))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
//LKED   EXEC PGM=IEWL,REGION=4M,
//            COND=((4,LT,PC),(4,LT,TRN),(5,LT,COB)),
//            PARM='LIST,XREF'
//SYSLIB   DD DSN=DFH420.CICS.SDFHLOAD,DISP=SHR
//         DD DSN=CEE.SCEELKED,DISP=SHR
//         DD DSN=DSNA10.SDSNLOAD,DISP=SHR
//         DD DSN=ISP.SISPLOAD,DISP=SHR
//         DD DSN=GDDM.SADMMOD,DISP=SHR
//         DD DSN=MATE1.CICS.LOADLIB,DISP=SHR
//SYSLMOD  DD DSN=MATE1.CICS.LOADLIB(&MEMBER),DISP=SHR
//SYSUT1   DD UNIT=SYSDA,DCB=BLKSIZE=1024,
//            SPACE=(1024,(200,20))
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=&&COPYLINK,DISP=(OLD,DELETE)
//         DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD DDNAME=SYSIN
//SYSIN    DD  DUMMY
