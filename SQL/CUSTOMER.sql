DROP TABLE KALA15.CUSTOMER;                                             00001004
CREATE TABLE KALA15.CUSTOMER                                            00010000
 (SSN          CHAR(11) NOT NULL,                                       00020000
  F_NAME       VARCHAR(15) NOT NULL,                                    00030000
  L_NAME       VARCHAR(15) NOT NULL,                                    00040000
  ADDRESS      VARCHAR(20),                                             00050000
  POSTAL_CODE  CHAR(5),                                                 00060000
  CITY         VARCHAR(20),                                             00070000
  PRIMARY KEY  (SSN))                                                   00080000
  IN DBMATE1.TSKALA15                                                   00090000
  ;                                                                     00091000
  CREATE UNIQUE INDEX KALA15.SSN_PK                                     00100000
         ON KALA15.CUSTOMER                                             00110000
         (SSN ASC)                                                      00111002
  ;                                                                     00120000
