          05 :TAG:-CRUD-SW          PIC X.
             88 SW-READ             VALUE 'R'.
             88 SW-INSERT           VALUE 'I'.
             88 SW-DELETE           VALUE 'D'.
             88 SUCCESS             VALUE 'S'.

          05 :TAG:-MESSAGE          PIC X(40).
          05 :TAG:-DATA.
             10 :TAG:-SSN           PIC X(11).
             10 :TAG:-F-NAME        PIC X(15).
             10 :TAG:-L-NAME        PIC X(15).
             10 :TAG:-ADDRESS       PIC X(20).
             10 :TAG:-POSTAL-CODE   PIC X(5).
             10 :TAG:-CITY          PIC X(20).
             10 :TAG:-APP-ID        PIC 9(8).
             10 :TAG:-STATUS        PIC 9.
             10 :TAG:-LOAN-AMN      PIC 9(8).
             10 :TAG:-LOAN-PERIOD   PIC 9(2).
             10 :TAG:-INTEREST      PIC 9V9(2).
             10 :TAG:-MO-PAYMNT     PIC 9(8)V9(4).
             10 :TAG:-TOTAL-LOAN    PIC 9(10)V9(2).
             10 :TAG:-LOAN-USAGE    PIC X(40).
             10 :TAG:-AGE           PIC 9(3).
