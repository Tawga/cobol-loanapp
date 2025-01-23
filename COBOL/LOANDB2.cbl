       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. LOANDB2.                                             00020001
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
                                                                        00050000
       WORKING-STORAGE SECTION.                                         00060000
           EXEC SQL                                                     00070002
              INCLUDE SQLCA                                             00071002
           END-EXEC.                                                    00072002
           EXEC SQL                                                     00072102
              INCLUDE APPS                                              00072202
           END-EXEC.                                                    00072302
           EXEC SQL                                                     00072402
              INCLUDE CUSTOMER                                          00072502
           END-EXEC.                                                    00072602
           EXEC SQL                                                     00072702
              INCLUDE INTEREST                                          00072802
           END-EXEC.                                                    00072902
                                                                        00073002
           COPY DFHAID.                                                 00080000
           COPY DFHBMSCA.                                               00090000
                                                                        00091000
       01 WS-SSN                   PIC X(11).                           00092109
       01 WS-SSN-FOUND             PIC X.                               00093008
       01 WS-INTEREST              PIC 9V9(2).                          00093929
       01 SQL-DISP                 PIC ZZ9.                             00094033
                                                                        00094129
       01 WS-CALCULATION-VARS.                                          00094212
          05 WS-INT-RATE              PIC S9V9(8)     COMP-3.           00094313
          05 WS-MO-RATE               PIC S9V9(8)     COMP-3.           00094413
          05 WS-INTERMEDIATE-RES      PIC S9(10)V9(8) COMP-3.           00094513
                                                                        00094608
       01 WS-INDICATOR-VARS.                                            00094705
          05 IV-ADDRESS            PIC S9(4) COMP.                      00095005
          05 IV-POSTAL-CODE        PIC S9(4) COMP.                      00096005
          05 IV-CITY               PIC S9(4) COMP.                      00097005
          05 IV-INTEREST-ID        PIC S9(4) COMP.                      00097141
          05 IV-INTEREST-RATE      PIC S9(4) COMP.                      00098010
          05 IV-MO-PAYMNT          PIC S9(4) COMP.                      00099005
          05 IV-TOTAL-LOAN         PIC S9(4) COMP.                      00099105
          05 IV-LOAN-USAGE         PIC S9(4) COMP.                      00099205
                                                                        00100000
       LINKAGE SECTION.                                                 00110000
       01 DFHCOMMAREA.                                                  00110111
           COPY COMMDATA REPLACING ==:TAG:== BY ==LS==.                 00110210
                                                                        00115005
       PROCEDURE DIVISION USING DFHCOMMAREA.                            00120011
       000-MAIN-PARA.                                                   00130000
           MOVE LOW-VALUES TO WS-INDICATOR-VARS                         00130142
           MOVE LS-SSN TO WS-SSN                                        00130242
                                                                        00130342
           EVALUATE TRUE                                                00131404
           WHEN SW-READ                                                 00131504
              PERFORM 100-PROCESS-READ                                  00131633
                                                                        00131702
           WHEN SW-INSERT                                               00131804
              PERFORM 200-PROCESS-APPLICATION                           00133427
                                                                        00133525
           WHEN SW-DELETE                                               00133625
              PERFORM 300-PROCESS-DELETE                                00133733
                                                                        00133825
           END-EVALUATE                                                 00133925
                                                                        00134025
           PERFORM 400-RETURN-CONTROL                                   00134125
           GOBACK.                                                      00134225
                                                                        00134325
       100-PROCESS-READ.                                                00134433
           EXEC SQL                                                     00134525
              SELECT                                                    00134625
                 A.APP_ID,                                              00134725
                 A.STATUS,                                              00134825
                 A.LOAN_AMN,                                            00134925
                 A.LOAN_PERIOD,                                         00135025
                 COALESCE(I.RATE, 0) AS INTEREST_RATE,                  00135142
                 COALESCE(A.MO_PAYMNT, 0) AS MO_PAYMNT,                 00135242
                 COALESCE(A.TOTAL_LOAN, 0) AS TOTAL_LOAN,               00135342
                 COALESCE(A.LOAN_USAGE, ' ') AS LOAN_USAGE,             00135442
                 C.F_NAME,                                              00135525
                 C.L_NAME,                                              00135625
                 COALESCE(C.ADDRESS, ' ') AS ADDRESS,                   00135742
                 COALESCE(C.POSTAL_CODE, ' ') AS POSTAL_CODE,           00135842
                 COALESCE(C.CITY, ' ') AS CITY                          00135942
              INTO                                                      00136025
                 :APP-ID,                                               00136125
                 :APP-STATUS,                                           00136225
                 :LOAN-AMN,                                             00136325
                 :LOAN-PERIOD,                                          00136425
                 :RATE :IV-INTEREST-RATE,                               00136525
                 :MO-PAYMNT :IV-MO-PAYMNT,                              00136625
                 :TOTAL-LOAN :IV-TOTAL-LOAN,                            00136725
                 :LOAN-USAGE :IV-LOAN-USAGE,                            00136825
                 :F-NAME,                                               00136925
                 :L-NAME,                                               00137025
                 :CUSTOMER-ADDRESS :IV-ADDRESS,                         00137125
                 :POSTAL-CODE :IV-POSTAL-CODE,                          00137225
                 :CITY :IV-CITY                                         00137325
              FROM                                                      00137425
                 KALA15.CUSTOMER C                                      00137525
              JOIN                                                      00137625
                 KALA15.APPLICATIONS A                                  00137725
              ON                                                        00137825
                 C.SSN = A.CUST_SSN                                     00137925
              LEFT JOIN                                                 00138025
                 KALA15.INTEREST I                                      00138125
              ON                                                        00138225
                 A.INTEREST = I.INTEREST_ID                             00138325
              WHERE                                                     00138425
                 C.SSN = :WS-SSN                                        00138525
           END-EXEC.                                                    00138625
                                                                        00138725
           EVALUATE SQLCODE                                             00138825
              WHEN 00                                                   00138925
                 MOVE 'APPLICATION FOUND' TO LS-MESSAGE                 00139035
                 MOVE F-NAME              TO LS-F-NAME                  00139135
                 MOVE L-NAME              TO LS-L-NAME                  00139235
                 MOVE CUSTOMER-ADDRESS    TO LS-ADDRESS                 00139335
                 MOVE POSTAL-CODE         TO LS-POSTAL-CODE             00139435
                 MOVE CITY                TO LS-CITY                    00139535
                 MOVE APP-ID              TO LS-APP-ID                  00139635
                 MOVE APP-STATUS          TO LS-STATUS                  00139735
                 MOVE LOAN-AMN            TO LS-LOAN-AMN                00139835
                 MOVE LOAN-PERIOD         TO LS-LOAN-PERIOD             00139935
                 MOVE MO-PAYMNT           TO LS-MO-PAYMNT               00140235
                 MOVE TOTAL-LOAN          TO LS-TOTAL-LOAN              00140335
                 MOVE LOAN-USAGE          TO LS-LOAN-USAGE              00140435
                 COMPUTE LS-INTEREST = RATE * 100                       00140535
                 SET SUCCESS TO TRUE                                    00140633
                                                                        00140725
              WHEN 100                                                  00140825
                 MOVE 'APPLICATION NOT FOUND' TO LS-MESSAGE             00140933
                                                                        00141025
              WHEN OTHER                                                00141125
                 MOVE SQLCODE TO SQL-DISP                               00141233
                 STRING 'ERROR FETCHING. SQLCODE: ' DELIMITED BY SIZE   00141333
                        SQL-DISP DELIMITED BY SIZE                      00141433
                        INTO LS-MESSAGE                                 00141533
                 END-STRING                                             00141633
                                                                        00141725
           END-EVALUATE                                                 00141825
           EXIT.                                                        00141925
                                                                        00142025
       200-PROCESS-APPLICATION.                                         00142425
           MOVE LS-SSN         TO SSN                                   00142528
           MOVE LS-SSN         TO CUST-SSN                              00142628
           MOVE LS-F-NAME      TO F-NAME                                00142728
           MOVE LS-L-NAME      TO L-NAME                                00142828
           MOVE LS-ADDRESS     TO CUSTOMER-ADDRESS                      00142928
           MOVE LS-POSTAL-CODE TO POSTAL-CODE                           00143028
           MOVE LS-CITY        TO CITY                                  00143128
           MOVE LS-LOAN-AMN    TO LOAN-AMN                              00143228
           MOVE LS-LOAN-PERIOD TO LOAN-PERIOD                           00143328
           MOVE LS-INTEREST    TO WS-INTEREST                           00143428
           MOVE LS-MO-PAYMNT   TO MO-PAYMNT                             00143528
           MOVE LS-TOTAL-LOAN  TO TOTAL-LOAN                            00143628
           MOVE LS-LOAN-USAGE  TO LOAN-USAGE                            00143742
                                                                        00143840
           EVALUATE TRUE                                                00143940
              WHEN LS-LOAN-AMN <= 00                                    00144040
                 MOVE 'LOAN AMOUNT CANNOT BE NEGATIVE NUMBER'           00144140
                      TO LS-MESSAGE                                     00144240
              WHEN LS-LOAN-PERIOD <= 00                                 00144340
                 MOVE 'LOAN PERIOD CANNOT BE NEGATIVE NUMBER'           00144440
                      TO LS-MESSAGE                                     00144540
              WHEN OTHER                                                00144640
                 PERFORM 210-APPROVE-APPLICATION                        00144941
           END-EVALUATE                                                 00145040
           EXIT.                                                        00145105
                                                                        00145205
       210-APPROVE-APPLICATION.                                         00145309
           PERFORM 211-EVALUATE-AGE                                     00145540
           IF APP-STATUS = 01                                           00145641
              PERFORM 220-FETCH-INTEREST-RATE                           00145736
           ELSE                                                         00145836
              MOVE -1 TO IV-INTEREST-ID                                 00146741
              MOVE -1 TO IV-MO-PAYMNT                                   00146842
              MOVE -1 TO IV-TOTAL-LOAN                                  00146942
                                                                        00147042
              PERFORM 240-INSERT                                        00147141
           END-IF                                                       00147236
           EXIT.                                                        00147336
                                                                        00147436
       211-EVALUATE-AGE.                                                00147536
      *   EVALUATE APPLICANT AGE                                        00147636
           EVALUATE TRUE                                                00147736
              WHEN LS-AGE < 18                                          00147837
                 MOVE 0 TO APP-STATUS                                   00147936
              WHEN LS-AGE > 80                                          00148038
                 IF LS-LOAN-AMN > 50000                                 00148138
                    MOVE 0 TO APP-STATUS                                00148238
                 ELSE                                                   00148341
                    PERFORM 212-EVALUATE-APPLICATION-INFO               00148441
                 END-IF                                                 00148538
              WHEN OTHER                                                00148636
                 PERFORM 212-EVALUATE-APPLICATION-INFO                  00148741
           END-EVALUATE                                                 00148841
           EXIT.                                                        00148941
                                                                        00149036
       212-EVALUATE-APPLICATION-INFO.                                   00149136
      *   EVALUATING APPLICATION INFORMATION                            00149236
           MOVE 0 TO APP-STATUS                                         00149339
           EVALUATE TRUE                                                00149436
              WHEN LS-LOAN-AMN < 1000                                   00149537
                 MOVE 1 TO APP-STATUS                                   00149636
                                                                        00149739
              WHEN LS-LOAN-AMN < 50000  AND LS-LOAN-PERIOD < 48         00149839
                 MOVE 1 TO APP-STATUS                                   00149939
                                                                        00150039
              WHEN LS-LOAN-AMN < 100000 AND LS-LOAN-PERIOD < 36         00150139
                  MOVE 1 TO APP-STATUS                                  00150238
                                                                        00150339
              WHEN LS-LOAN-AMN > 100000 AND LS-LOAN-PERIOD < 25         00150439
                  MOVE 1 TO APP-STATUS                                  00150538
                                                                        00150639
           END-EVALUATE                                                 00150741
           EXIT.                                                        00150841
                                                                        00150908
       220-FETCH-INTEREST-RATE.                                         00151010
           EXEC SQL                                                     00151119
               SELECT RATE, INTEREST_ID                                 00151224
               INTO :RATE, :INTEREST-ID                                 00151324
               FROM KALA15.INTEREST                                     00151421
               WHERE (RANGE_START IS NULL OR                            00151519
                      RANGE_START <= :LOAN-AMN)                         00151620
                 AND (RANGE_END IS NULL OR                              00151719
                      RANGE_END >= :LOAN-AMN)                           00151820
           END-EXEC                                                     00151919
                                                                        00152021
           EVALUATE SQLCODE                                             00152121
              WHEN 00                                                   00152221
                  PERFORM 230-CALCULATE-LOAN                            00152321
              WHEN OTHER                                                00152421
                  STRING 'ERR INTEREST FETCH - LOAN-AMN:'               00152527
                            DELIMITED BY SIZE                           00152627
                          LS-LOAN-AMN DELIMITED BY SIZE                 00152727
                          INTO LS-MESSAGE                               00152827
                  END-STRING                                            00152927
           EXIT.                                                        00153042
                                                                        00153142
       230-CALCULATE-LOAN.                                              00153242
           MOVE RATE TO WS-INT-RATE                                     00153321
      *    CALCULATE MOTHLY RATE                                        00153413
           COMPUTE WS-MO-RATE = WS-INT-RATE / 12                        00153510
      *    HELPER CALC TO KEEP PRECISSION                               00153622
           COMPUTE WS-INTERMEDIATE-RES =                                00153713
                   (1 - (1 + WS-MO-RATE) ** (-1 * LOAN-PERIOD))         00153822
      *    CALCULATE MONTHLY PAYMENT                                    00153913
           COMPUTE MO-PAYMNT =                                          00154022
                   (LOAN-AMN * WS-MO-RATE) / WS-INTERMEDIATE-RES        00154122
                                                                        00154210
      *    CALCULATE TOTAL LOAN AMOUNT                                  00154322
           COMPUTE TOTAL-LOAN = MO-PAYMNT * LOAN-PERIOD                 00154422
           PERFORM 240-INSERT                                           00154525
           EXIT.                                                        00154608
                                                                        00154708
       240-INSERT.                                                      00154808
           IF LOAN-USAGE = SPACES OR LOAN-USAGE = LOW-VALUE             00155742
              MOVE -1 TO IV-LOAN-USAGE                                  00155842
           END-IF                                                       00155942
                                                                        00156042
           IF CUSTOMER-ADDRESS = SPACES OR CUSTOMER-ADDRESS = LOW-VALUE 00156142
              MOVE -1 TO IV-ADDRESS                                     00156242
           END-IF                                                       00156342
                                                                        00156442
           IF POSTAL-CODE = SPACES OR POSTAL-CODE = LOW-VALUE           00156542
              MOVE -1 TO IV-POSTAL-CODE                                 00156642
           END-IF                                                       00156742
                                                                        00156842
           IF CITY = SPACES OR CITY = LOW-VALUE                         00156942
              MOVE -1 TO IV-CITY                                        00157042
           END-IF                                                       00157142
                                                                        00157442
      *    SQL MERGE I/U TO CUSTOMER                                    00157542
           EXEC SQL                                                     00157642
              MERGE INTO KALA15.CUSTOMER AS C                           00157742
              USING (VALUES (:SSN, :F-NAME, :L-NAME,                    00157842
                    :CUSTOMER-ADDRESS :IV-ADDRESS,                      00157942
                    :POSTAL-CODE :IV-POSTAL-CODE,                       00158042
                    :CITY :IV-CITY)) AS S                               00158142
                    (SSN, F_NAME, L_NAME, ADDRESS, POSTAL_CODE, CITY)   00158242
              ON C.SSN = S.SSN                                          00158342
              WHEN MATCHED THEN                                         00158442
                 UPDATE SET                                             00158642
                    C.F_NAME = S.F_NAME,                                00158742
                    C.L_NAME = S.L_NAME,                                00158842
                    C.ADDRESS = S.ADDRESS,                              00158942
                    C.POSTAL_CODE = S.POSTAL_CODE,                      00159042
                    C.CITY = S.CITY                                     00159142
              WHEN NOT MATCHED THEN                                     00159242
                 INSERT (SSN, F_NAME, L_NAME, ADDRESS, POSTAL_CODE,     00159342
                 CITY)                                                  00159442
                 VALUES (S.SSN, S.F_NAME, S.L_NAME, S.ADDRESS,          00159542
                         S.POSTAL_CODE, S.CITY)                         00159642
           END-EXEC                                                     00159742
                                                                        00159842
           IF SQLCODE NOT = 00                                          00159942
              MOVE 'ERROR WHILE INSERTING CUSTOMER' TO LS-MESSAGE       00160042
              PERFORM 400-RETURN-CONTROL                                00160142
           END-IF                                                       00160242
                                                                        00160342
      *    SQL MERGE I/U TO APPLICATIONS                                00160442
           EXEC SQL                                                     00160542
               MERGE INTO KALA15.APPLICATIONS AS A                      00160642
               USING (VALUES (:WS-SSN, :APP-STATUS, :LOAN-AMN,          00160742
                       :LOAN-PERIOD, :INTEREST-ID :IV-INTEREST-ID,      00160842
                       :MO-PAYMNT, :TOTAL-LOAN, :LOAN-USAGE)) AS S      00160942
                     (CUST_SSN, STATUS, LOAN_AMN, LOAN_PERIOD, INTEREST,00161042
                     MO_PAYMNT, TOTAL_LOAN, LOAN_USAGE)                 00161142
               ON A.CUST_SSN = S.CUST_SSN                               00161242
               WHEN MATCHED THEN                                        00161342
                  UPDATE SET                                            00161442
                    A.STATUS = S.STATUS,                                00161542
                    A.LOAN_AMN = S.LOAN_AMN,                            00161642
                    A.LOAN_PERIOD = S.LOAN_PERIOD,                      00161742
                    A.INTEREST = S.INTEREST,                            00161842
                    A.MO_PAYMNT = S.MO_PAYMNT,                          00161942
                    A.TOTAL_LOAN = S.TOTAL_LOAN,                        00162042
                    A.LOAN_USAGE = S.LOAN_USAGE                         00162142
               WHEN NOT MATCHED THEN                                    00162242
                  INSERT (CUST_SSN, STATUS, LOAN_AMN, LOAN_PERIOD,      00162342
                         INTEREST, MO_PAYMNT, TOTAL_LOAN, LOAN_USAGE)   00162442
                  VALUES (S.CUST_SSN, S.STATUS, S.LOAN_AMN,             00162542
                          S.LOAN_PERIOD, S.INTEREST, S.MO_PAYMNT,       00162642
                         S.TOTAL_LOAN, S.LOAN_USAGE)                    00162742
           END-EXEC                                                     00162842
                                                                        00162942
           EVALUATE SQLCODE                                             00163042
           WHEN 00                                                      00163142
              IF APP-STATUS = 1                                         00163242
                 MOVE 'APPLICATION SUBMITTED' TO LS-MESSAGE             00163342
              ELSE                                                      00163442
                 MOVE 'APPLICATION SUBMITTED AS REJECT' TO LS-MESSAGE   00163542
              END-IF                                                    00163642
                                                                        00163742
           WHEN OTHER                                                   00163842
              MOVE 'ERROR WHILE INSERTING APPLICATION' TO LS-MESSAGE    00163942
           END-EVALUATE                                                 00164042
                                                                        00164142
           EXIT.                                                        00164242
                                                                        00164342
       300-PROCESS-DELETE.                                              00164442
           EXEC SQL                                                     00164542
               SELECT 'Y'                                               00164642
               INTO :WS-SSN-FOUND                                       00164742
               FROM KALA15.CUSTOMER                                     00164842
               WHERE SSN = :WS-SSN                                      00164942
               FETCH FIRST 1 ROWS ONLY                                  00165042
           END-EXEC                                                     00165142
           IF SQLCODE = 00 AND WS-SSN-FOUND = 'Y'                       00165242
              PERFORM 310-DELTE-FROM-DATABASE                           00165342
           ELSE                                                         00165442
              MOVE 'SSN NOT FOUND' TO LS-MESSAGE                        00165542
           END-IF                                                       00165642
           EXIT.                                                        00165742
                                                                        00165842
       310-DELTE-FROM-DATABASE.                                         00165942
           EXEC SQL                                                     00166042
               DELETE FROM KALA15.APPLICATIONS                          00166142
               WHERE CUST_SSN = :WS-SSN                                 00166242
           END-EXEC                                                     00166342
           EVALUATE SQLCODE                                             00166442
           WHEN 00                                                      00166542
              MOVE 'APPLICATION DELETED' TO LS-MESSAGE                  00166642
                                                                        00166742
           WHEN OTHER                                                   00166842
              MOVE 'ERROR WHILE DELETING APPLICATION' TO LS-MESSAGE     00166942
           END-EVALUATE                                                 00167042
           EXIT.                                                        00167142
                                                                        00167242
       400-RETURN-CONTROL.                                              00167342
           EXEC CICS RETURN END-EXEC                                    00168042
           EXIT.                                                        00170008
