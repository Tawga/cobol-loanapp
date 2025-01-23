       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. LOANMENU.                                            00020000
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
                                                                        00050000
       WORKING-STORAGE SECTION.                                         00060000
           COPY LMAPST1.                                                00070000
           COPY DFHAID.                                                 00080000
           COPY DFHBMSCA.                                               00090000
                                                                        00091000
       01 COMMUNICATION-AREA       PIC X.                               00092001
       01 WS-EXIT-MESG             PIC X(23).                           00093001
       01 WS-RESPCODE              PIC S9(08) COMP.                     00094001
                                                                        00100000
       LINKAGE SECTION.                                                 00110000
       01 DFHCOMMAREA              PIC X.                               00111001
                                                                        00112000
       PROCEDURE DIVISION.                                              00120000
       000-MAIN-PARA.                                                   00130000
           EVALUATE TRUE                                                00131000
      *       WHEN PROGRAM STARTS FOR THE FIRST TIME                    00131100
              WHEN EIBCALEN = ZERO                                      00132000
                 MOVE LOW-VALUE TO LMENUO                               00132101
                 PERFORM 100-SEND-MAP                                   00132201
                                                                        00132900
      *       WHEN USER PRESSES PF3 EXIT THE APP                        00133000
              WHEN EIBAID = DFHPF3                                      00133100
                 PERFORM 200-TERMINATE                                  00133201
                                                                        00133300
      *       WHEN USER PRESSES PF4 XCTL TO LOANFORM PROGRAM            00133400
              WHEN EIBAID = DFHPF4                                      00133500
                 EXEC CICS XCTL                                         00133701
                      PROGRAM('LOANFORM')                               00133801
                      RESP(WS-RESPCODE)                                 00133901
                 END-EXEC                                               00134001
                 IF WS-RESPCODE = DFHRESP(NORMAL)                       00134101
                    CONTINUE                                            00134201
                 ELSE                                                   00134301
                    MOVE LOW-VALUE TO LMENUO                            00134401
                    MOVE "PRGM NOT FOUND"  TO MESSAGEO                  00134501
                    PERFORM 100-SEND-MAP                                00134601
                 END-IF                                                 00134701
                                                                        00134800
      *       WHEN USER PRESSES PF5 XCTL TO LOANSRCH PROGRAM            00134900
              WHEN EIBAID = DFHPF5                                      00135000
                 EXEC CICS XCTL                                         00135101
                      PROGRAM('LOANSRCH')                               00135201
                      RESP(WS-RESPCODE)                                 00135301
                 END-EXEC                                               00135401
                 IF WS-RESPCODE = DFHRESP(NORMAL)                       00135501
                    CONTINUE                                            00135601
                 ELSE                                                   00135701
                    MOVE LOW-VALUE TO LMENUO                            00135801
                    MOVE "PRGM NOT FOUND"  TO MESSAGEO                  00135901
                    PERFORM 100-SEND-MAP                                00136001
                 END-IF                                                 00136101
                                                                        00136201
      *       WHEN USER PRESSES ANY OTHER AID KEY SEND INVAL MESSAGE    00136300
              WHEN OTHER                                                00136400
                  MOVE LOW-VALUE TO LMENUO                              00136500
                  MOVE 'INVALID KEY PRESSED.' TO MESSAGEO               00136600
                  PERFORM 100-SEND-MAP                                  00136700
                                                                        00136800
           END-EVALUATE                                                 00136900
           PERFORM 300-RETURN                                           00137001
           EXIT.                                                        00160000
                                                                        00170000
       100-SEND-MAP.                                                    00180000
           EXEC CICS SEND                                               00190000
              MAP('LMENU')                                              00200000
              MAPSET('LMAPST1')                                         00210000
              ERASE                                                     00220000
              FREEKB                                                    00221001
           END-EXEC.                                                    00230000
           EXIT.                                                        00240000
                                                                        00250000
       200-TERMINATE.                                                   00260001
           MOVE 'LOAN PROGRAM TERMINATED' TO WS-EXIT-MESG               00260101
           EXEC CICS SEND TEXT FROM (WS-EXIT-MESG)                      00261001
                ERASE                                                   00263001
                FREEKB                                                  00263101
           END-EXEC.                                                    00264001
           EXEC CICS RETURN END-EXEC.                                   00270001
           EXIT.                                                        00300000
                                                                        00301001
       300-RETURN.                                                      00302001
           EXEC CICS RETURN                                             00303001
                     TRANSID('LOAN')                                    00304001
                     COMMAREA(COMMUNICATION-AREA)                       00305001
           END-EXEC.                                                    00306001
           EXIT.                                                        00307001
                                                                        00310000
