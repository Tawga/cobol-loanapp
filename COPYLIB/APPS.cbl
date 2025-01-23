      ******************************************************************
      * DCLGEN TABLE(KALA15.APPLICATIONS)                              *
      *        LIBRARY(KALA15.LOANAPP.COPYLIB(APPS))                   *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE KALA15.APPLICATIONS TABLE
           ( APP_ID                         INTEGER NOT NULL,
             CUST_SSN                       CHAR(11) NOT NULL,
             STATUS                         INTEGER NOT NULL,
             LOAN_AMN                       INTEGER NOT NULL,
             LOAN_PERIOD                    INTEGER NOT NULL,
             INTEREST                       INTEGER,
             MO_PAYMNT                      DECIMAL(10, 2),
             TOTAL_LOAN                     DECIMAL(10, 2),
             LOAN_USAGE                     VARCHAR(40)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KALA15.APPLICATIONS                *
      ******************************************************************
       01  DCLAPPLICATIONS.
           10 APP-ID               PIC S9(9) USAGE COMP.
           10 CUST-SSN             PIC X(11).
           10 APP-STATUS           PIC S9(9) USAGE COMP.
           10 LOAN-AMN             PIC S9(9) USAGE COMP.
           10 LOAN-PERIOD          PIC S9(9) USAGE COMP.
           10 INTEREST             PIC S9(9) USAGE COMP.
           10 MO-PAYMNT            PIC S9(8)V9(2) USAGE COMP-3.
           10 TOTAL-LOAN           PIC S9(8)V9(2) USAGE COMP-3.
           10 LOAN-USAGE           PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
