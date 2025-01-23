       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. LOANSRCH.                                            00020001
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
       WORKING-STORAGE SECTION.                                         00050000
       01 COMMUNICATION-AREA       PIC X.                               00060001
       01 WS-RESPCODE              PIC S9(08) COMP.                     00080001
       01 WS-ANUM-CONV-VAR         PIC ZZZZZZZZZ9.99.                   00080105
       01 WS-SSN-LENGTH            PIC 9(11) VALUE ZERO.                00081006
       01 WS-APP-STATUS            PIC X(8).                            00081107
       01 WS-LOAN-AMNT-CONV        PIC ZZZZZZZ9.                        00081207
       01 DATAGROUP.                                                    00082004
           COPY COMMDATA REPLACING ==:TAG:== BY ==WS==.                 00083004
                                                                        00090001
           COPY LMAPST3.                                                00100001
           COPY DFHAID.                                                 00110001
           COPY DFHBMSCA.                                               00120001
                                                                        00130001
       LINKAGE SECTION.                                                 00140000
       01 DFHCOMMAREA              PIC X.                               00150000
                                                                        00161004
       PROCEDURE DIVISION.                                              00170000
       000-MAIN-PARA.                                                   00180000
                                                                        00190002
           MOVE LOW-VALUES TO DATAGROUP                                 00191015
           EVALUATE TRUE                                                00200002
      *       WHEN PROGRAM STARTS FOR THE FIRST TIME                    00210001
              WHEN EIBCALEN = ZERO                                      00220002
                 MOVE LOW-VALUE TO LSEARCHO                             00230002
                 PERFORM 100-SEND-MAP                                   00240002
                                                                        00250003
      *       WHEN USER PRESSES ENTER                                   00260003
              WHEN EIBAID = DFHENTER                                    00270003
                 PERFORM 300-RECEIVE-DATA                               00280004
                 EVALUATE TRUE                                          00340004
                    WHEN WS-SSN-LENGTH = 11                             00380004
                       PERFORM 400-READ-INFORMATION                     00380109
                       PERFORM 100-SEND-MAP                             00400006
                    WHEN OTHER                                          00401004
                       STRING                                           00401106
                         'INVALID SOCIAL SECURITY NUMBER' DELIMITED     00401206
                          BY SIZE                                       00401306
                          ' ' DELIMITED BY SIZE                         00401406
                          SSNI DELIMITED BY SIZE                        00401506
                          INTO MESSAGEO                                 00401606
                       END-STRING                                       00401706
      *                MOVE 'INVALID SOCIAL SECURITY NUMBER ' WS-SSN    00402006
      *                     TO MESSAGEO                                 00402106
                       PERFORM 100-SEND-MAP                             00403006
                 END-EVALUATE                                           00410004
                                                                        00420004
      *       WHEN USER PRESSES PF4 DELETE APPLICATION                  00520003
              WHEN EIBAID = DFHPF4                                      00530003
                 PERFORM 300-RECEIVE-DATA                               00530115
                 EVALUATE WS-SSN-LENGTH                                 00530215
                    WHEN 11                                             00530315
                       MOVE SSNI TO WS-SSN                              00530415
                       IF DFHCOMMAREA = 'S'                             00530515
                          PERFORM 500-DELETE-APPLICATION                00530615
                          PERFORM 100-SEND-MAP                          00530715
                       ELSE                                             00530815
                          MOVE 'SELECT APPLICATION FIRST' TO MESSAGEO   00530915
                          PERFORM 100-SEND-MAP                          00531615
                       END-IF                                           00531715
                    WHEN OTHER                                          00531815
                       MOVE 'INVALID SOCIAL SECURITY NUMBER' TO MESSAGEO00531915
                 END-EVALUATE                                           00532015
                                                                        00533009
      *       WHEN USER PRESSES PF5 EDIT APPLICATION                    00630003
              WHEN EIBAID = DFHPF5                                      00640003
                 MOVE 'FEATURE NOT AVAILABLE YET' TO MESSAGEO           00641004
                 PERFORM 100-SEND-MAP                                   00641113
                                                                        00731004
      *       WHEN USER PRESSES PF3 RETURN TO MENU                      00740003
              WHEN EIBAID = DFHPF3                                      00750002
                 PERFORM 200-RETURN-TO-MENU                             00760003
                                                                        00770002
      *       WHEN USER PRESSES ANY OTHER AID KEY SEND INVAL MESSAGE    00780000
              WHEN OTHER                                                00790002
                  MOVE LOW-VALUE TO LSEARCHO                            00800002
                  MOVE 'INVALID KEY PRESSED' TO MESSAGEO                00810003
                  PERFORM 100-SEND-MAP                                  00820002
                                                                        00830002
           END-EVALUATE                                                 00840002
           EXEC CICS RETURN                                             00850002
                     TRANSID('LSCH')                                    00860003
                     COMMAREA(WS-CRUD-SW)                               00870010
           END-EXEC.                                                    00880002
           EXIT.                                                        00890002
                                                                        00900002
       100-SEND-MAP.                                                    00910002
           INSPECT SSNO REPLACING ALL ' ' BY '_'                        00912006
           EXEC CICS SEND                                               00920002
              MAP('LSEARCH')                                            00930002
              MAPSET('LMAPST3')                                         00940002
              FROM(LSEARCHO)                                            00941006
              ERASE                                                     00950002
              FREEKB                                                    00960002
           END-EXEC.                                                    00970002
           EXIT.                                                        00980002
                                                                        00988006
      *       RETURN TO LOANMENU                                        00990006
       200-RETURN-TO-MENU.                                              01000003
           EXEC CICS XCTL                                               01010003
                PROGRAM('LOANMENU')                                     01030003
                RESP(WS-RESPCODE)                                       01041004
           END-EXEC                                                     01050003
           EXIT.                                                        01060002
                                                                        01060104
      *       RECEIVE DATA FROM MAPSET                                  01061004
       300-RECEIVE-DATA.                                                01062004
           MOVE LOW-VALUE TO LSEARCHI                                   01063004
           EXEC CICS RECEIVE                                            01064004
              MAP('LSEARCH')                                            01065004
              MAPSET('LMAPST3')                                         01066004
              INTO(LSEARCHI)                                            01067004
              RESP(WS-RESPCODE)                                         01068004
           END-EXEC                                                     01069004
                                                                        01069106
           INSPECT SSNI REPLACING ALL '_' BY ' '                        01069206
                                                                        01069304
           INSPECT SSNI TALLYING WS-SSN-LENGTH                          01069404
                   FOR CHARACTERS BEFORE INITIAL ' '                    01069512
           EXIT.                                                        01069612
                                                                        01069712
      *       LINK TO LOANDB2 TO GET APPLICATION INFORMATION            01069804
       400-READ-INFORMATION.                                            01069909
           SET SW-READ TO TRUE                                          01070104
           MOVE SSNI TO WS-SSN                                          01070205
                                                                        01070304
           EXEC CICS LINK                                               01070404
              PROGRAM('LOANDB2')                                        01070504
              COMMAREA(DATAGROUP)                                       01070604
              RESP(WS-RESPCODE)                                         01071004
           END-EXEC                                                     01072004
                                                                        01072104
           IF WS-RESPCODE NOT = DFHRESP(NORMAL)                         01072204
              MOVE 'ERROR SEARCHING INFORMATION' TO MESSAGEO            01072304
              PERFORM 100-SEND-MAP                                      01072404
           ELSE                                                         01072605
              MOVE WS-MESSAGE TO MESSAGEO                               01072705
                                                                        01072805
              IF SUCCESS                                                01072911
                 STRING WS-F-NAME DELIMITED BY SPACE                    01073011
                        SPACE     DELIMITED BY SIZE                     01073111
                        WS-L-NAME DELIMITED BY SPACE                    01073211
                        INTO APPLCNTO                                   01073311
                 END-STRING                                             01073411
                                                                        01073507
                 MOVE WS-APP-ID TO APPIDO                               01073616
                                                                        01073707
                 EVALUATE WS-STATUS                                     01073811
                 WHEN 1                                                 01073911
                    MOVE 'APPROVED'       TO WS-APP-STATUS              01074016
                    MOVE WS-APP-STATUS    TO STATUSO                    01074116
                                                                        01074216
                    MOVE WS-INTEREST      TO INTRSTO                    01074316
                                                                        01074416
                    MOVE ZEROS            TO WS-ANUM-CONV-VAR           01074516
                    MOVE WS-MO-PAYMNT     TO WS-ANUM-CONV-VAR           01074616
                    MOVE WS-ANUM-CONV-VAR TO MNTHLYO                    01074716
                                                                        01074816
                    MOVE ZEROS            TO WS-ANUM-CONV-VAR           01074916
                    MOVE WS-TOTAL-LOAN    TO WS-ANUM-CONV-VAR           01075016
                    MOVE WS-ANUM-CONV-VAR TO TOTALO                     01075116
                 WHEN 0                                                 01075211
                    MOVE 'REJECTED'       TO WS-APP-STATUS              01075316
                    MOVE WS-APP-STATUS    TO STATUSO                    01075416
                 WHEN OTHER                                             01075511
                    MOVE ' '              TO WS-APP-STATUS              01075616
                 END-EVALUATE                                           01075711
                                                                        01075807
                 MOVE ZEROS               TO WS-ANUM-CONV-VAR           01075916
                 MOVE WS-LOAN-AMN         TO WS-LOAN-AMNT-CONV          01076016
                 MOVE WS-LOAN-AMNT-CONV   TO AMOUNTO                    01076116
                 MOVE WS-LOAN-PERIOD      TO PERIODO                    01076216
                                                                        01076316
              END-IF                                                    01076811
           END-IF                                                       01076911
           EXIT.                                                        01077011
                                                                        01077106
      *          LOANDB2 PROGRAM WILL DELETE APPLICATION                01077209
       500-DELETE-APPLICATION.                                          01077309
           SET SW-DELETE TO TRUE                                        01077509
                                                                        01077709
           EXEC CICS LINK                                               01077809
              PROGRAM('LOANDB2')                                        01077909
              COMMAREA(DATAGROUP)                                       01078009
              RESP(WS-RESPCODE)                                         01078109
           END-EXEC                                                     01078209
                                                                        01078309
           IF WS-RESPCODE NOT = DFHRESP(NORMAL)                         01078409
              MOVE 'ERROR SEARCHING INFORMATION' TO MESSAGEO            01078509
           ELSE                                                         01078609
              MOVE LOW-VALUE  TO LSEARCHO                               01078713
              MOVE WS-MESSAGE TO MESSAGEO                               01078811
           END-IF                                                       01078911
           EXIT.                                                        01079011
                                                                        01080009
