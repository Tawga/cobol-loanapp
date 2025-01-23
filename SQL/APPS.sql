--DROP TABLE KALA15.APPLICATIONS;                                       00001010
                                                                        00002010
CREATE TABLE KALA15.APPLICATIONS                                        00010001
 (APP_ID       INT NOT NULL GENERATED ALWAYS AS IDENTITY                00020001
               (START WITH 10000000 INCREMENT BY 23),                   00021005
  CUST_SSN     CHAR(11) NOT NULL,                                       00030001
  STATUS       INT NOT NULL,                                            00031008
  LOAN_AMN     INT NOT NULL,                                            00040001
  LOAN_PERIOD  INT NOT NULL,                                            00050001
  INTEREST     INT,                                                     00060001
  MO_PAYMNT    DECIMAL(10,2),                                           00070001
  TOTAL_LOAN   DECIMAL(10,2),                                           00071001
  LOAN_USAGE   VARCHAR(40),                                             00072006
  PRIMARY KEY  (APP_ID),                                                00080001
  FOREIGN KEY (CUST_SSN) REFERENCES CUSTOMER(SSN),                      00081003
  FOREIGN KEY (INTEREST) REFERENCES INTEREST(INTEREST_ID))              00082003
  IN DBMATE1.TSKALA15                                                   00090000
  ;                                                                     00091000
  CREATE UNIQUE INDEX KALA15.APP_PK                                     00100002
         ON KALA15.APPLICATIONS                                         00110002
         (APP_ID ASC)                                                   00111004
  ;                                                                     00120000
                                                                        00130010
