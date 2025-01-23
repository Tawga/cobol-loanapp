      ******************************************************************
      * DCLGEN TABLE(KALA15.CUSTOMER)                                  *
      *        LIBRARY(KALA15.LOANAPP.COPYLIB(CUSTOMER))               *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE KALA15.CUSTOMER TABLE
           ( SSN                            CHAR(11) NOT NULL,
             F_NAME                         VARCHAR(15) NOT NULL,
             L_NAME                         VARCHAR(15) NOT NULL,
             ADDRESS                        VARCHAR(20),
             POSTAL_CODE                    CHAR(5),
             CITY                           VARCHAR(20)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KALA15.CUSTOMER                    *
      ******************************************************************
       01  DCLCUSTOMER.
           10 SSN                  PIC X(11).
           10 F-NAME               PIC X(15).
      *       49 F-NAME-LEN        PIC S9(4) USAGE COMP.
      *       49 F-NAME-TEXT       PIC X(15).
           10 L-NAME               PIC X(15).
      *       49 L-NAME-LEN        PIC S9(4) USAGE COMP.
      *       49 L-NAME-TEXT       PIC X(15).
           10 CUSTOMER-ADDRESS     PIC X(20).
      *       49 ADDRESS-LEN       PIC S9(4) USAGE COMP.
      *       49 ADDRESS-TEXT      PIC X(20).
           10 POSTAL-CODE          PIC X(5).
           10 CITY                 PIC X(20).
      *       49 CITY-LEN          PIC S9(4) USAGE COMP.
      *       49 CITY-TEXT         PIC X(20).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
