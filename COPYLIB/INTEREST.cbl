      ******************************************************************
      * DCLGEN TABLE(KALA15.INTEREST)                                  *
      *        LIBRARY(KALA15.LOANAPP.COPYLIB(INTEREST))               *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE KALA15.INTEREST TABLE
           ( INTEREST_ID                    INTEGER NOT NULL,
             RATE                           DECIMAL(3, 2),
             RANGE_START                    INTEGER,
             RANGE_END                      INTEGER,
             INTEREST                       INTEGER
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KALA15.INTEREST                    *
      ******************************************************************
       01  DCLINTEREST.
           10 INTEREST-ID          PIC S9(9) USAGE COMP.
           10 RATE                 PIC S9(1)V9(2) USAGE COMP-3.
           10 RANGE-START          PIC S9(9) USAGE COMP.
           10 RANGE-END            PIC S9(9) USAGE COMP.
           10 INTEREST             PIC S9(9) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
