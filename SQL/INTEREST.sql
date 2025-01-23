--DROP TABLE KALA15.INTEREST;                                           00001015
                                                                        00002015
CREATE TABLE KALA15.INTEREST                                            00010001
 (INTEREST_ID  INT NOT NULL GENERATED ALWAYS AS IDENTITY                00020002
               (START WITH 1 INCREMENT BY 1),                           00021005
  RATE         DECIMAL(3,2),                                            00030002
  RANGE_START  INT,                                                     00040002
  RANGE_END    INT,                                                     00050002
  PRIMARY KEY  (INTEREST_ID))                                           00080008
  IN DBMATE1.TSKALA15                                                   00090000
  ;                                                                     00091000
  CREATE UNIQUE INDEX KALA15.INTR_PK                                    00100003
         ON KALA15.INTEREST                                             00110002
         (INTEREST_ID ASC)                                              00111010
  ;                                                                     00120000
                                                                        00130012
-- INSERT FIRST ROW WITH 8% RATE                                        00140012
INSERT INTO KALA15.INTEREST (RATE, RANGE_START, RANGE_END)              00150012
VALUES (0.08, NULL, 1000);                                              00160012
                                                                        00170012
-- INSERT SECOND ROW WITH 5% RATE                                       00180012
INSERT INTO KALA15.INTEREST (RATE, RANGE_START, RANGE_END)              00190012
VALUES (0.05, 1001, 10000);                                             00200012
                                                                        00210012
-- INSERT THIRD ROW WITH 3% RATE                                        00220012
INSERT INTO KALA15.INTEREST (RATE, RANGE_START, RANGE_END)              00230012
VALUES (0.03, 10001, NULL);                                             00240012
                                                                        00250015
