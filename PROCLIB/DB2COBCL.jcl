//********************************************************************
//*        DSNHICOB  - DB2 precompile, IBM COBOL compile, pre-link,  *
//*                    and link edit a DB2 SQL program.              *
//********************************************************************
//DB2COBCL PROC DCLGLIB=KALA15.LOANAPP.COPYLIB,
//             COPYLIB='SYS1.CPYLIB',
//             DBRMLIB=KALA15.LOANAPP.DBRMLIB,
//             SRCLIB=KALA15.LOANAPP.COBOL,
//             MEMBER='ERROR.MEMBER'
//********************************************************************
//*        Precompile the IBM COBOL program                          *
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
//********************************************************************
//*        Compile the IBM COBOL program if the precompile           *
//*        return code is 4 or less.                                 *
//********************************************************************
//COB      EXEC PGM=IGYCRCTL,
//             PARM=('SIZE(4000K),BUFSIZE(32760),LIST,LIB,MAP,OBJECT',
//             'DATA(31),XREF,RENT'),
//             COND=(4,LT,PC)
//STEPLIB  DD  DSNAME=IGY420.SIGYCOMP,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSLIN   DD  DSN=&&LOADSET,DISP=(MOD,PASS),UNIT=SYSDA,
//             SPACE=(800,(500,500))
//SYSLIB   DD  DSN=&COPYLIB,DISP=SHR
//SYSIN    DD  DSN=&&DSNHOUT,DISP=(OLD,DELETE)
//SYSUT1   DD  SPACE=(800,(500,500),,,ROUND),UNIT=SYSDA
//SYSUT2   DD  SPACE=(800,(500,500),,,ROUND),UNIT=SYSDA
//SYSUT3   DD  SPACE=(800,(500,500),,,ROUND),UNIT=SYSDA
//SYSUT4   DD  SPACE=(800,(500,500),,,ROUND),UNIT=SYSDA
//SYSUT5   DD  SPACE=(800,(500,500),,,ROUND),UNIT=SYSDA
//SYSUT6   DD  SPACE=(800,(500,500),,,ROUND),UNIT=SYSDA
//SYSUT7   DD  SPACE=(800,(500,500),,,ROUND),UNIT=SYSDA
//********************************************************************
//*        Linkedit if the precompile and compile                    *
//*        return codes are 4 or less.                               *
//********************************************************************
//LKED     EXEC PGM=IEWL,PARM='MAP,XREF',
//         COND=((4,LT,PC),(4,LT,COB))
//SYSLIB   DD  DISP=SHR,DSN=CEE.SCEELKED
//         DD  DISP=SHR,DSN=DSNA10.SDSNLOAD
//         DD  DISP=SHR,DSN=ISP.SISPLOAD
//         DD  DISP=SHR,DSN=GDDM.SADMMOD
//         DD  DISP=SHR,DSN=&LOADLIB
//SYSLIN   DD  DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DSN=&LOADLIB(&MEMBER),
//             DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  SPACE=(1024,(50,50)),UNIT=SYSDA
//SYSIN    DD  DUMMY























