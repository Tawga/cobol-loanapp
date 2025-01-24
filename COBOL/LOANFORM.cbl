       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. LOANFORM.                                            00020001
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
                                                                        00050000
       WORKING-STORAGE SECTION.                                         00060000
           COPY LMAPST2.                                                00070001
           COPY DFHAID.                                                 00080000
           COPY DFHBMSCA.                                               00090000
       01 DATAGROUP.                                                    00100005
           COPY COMMDATA REPLACING ==:TAG:== BY ==WS==.                 00110004
                                                                        00120013
       01 COMMUNICATION-AREA       PIC X.                               00130000
       01 WS-RESPCODE              PIC S9(08) COMP.                     00150001
                                                                        00211326
       01 LEAP-YEAR                PIC X VALUE SPACE.                   00211426
          88 IS-LEAP-YEAR          VALUE 'Y'.                           00211526
                                                                        00212026
       01 WS-SOCIAL-SEC-NUM.                                            00221025
          05 WS-SSN-BEGIN          PIC X(6) VALUE SPACES.               00222025
          05 WS-SSN-DOB            REDEFINES WS-SSN-BEGIN.              00222125
             10 WS-DOB-DAY         PIC 9(2).                            00223025
             10 WS-DOB-MONTH       PIC 9(2).                            00224025
             10 WS-DOB-YEAR        PIC 9(2).                            00225025
          05 WS-SSN-MIDDLE         PIC X    VALUE SPACES.               00226025
          05 WS-SSN-END            PIC X(4) VALUE SPACES.               00227025
       01 WS-SSN-LENGTH            PIC 9(2) VALUE ZERO.                 00230025
       01 WS-INVAL-CHARS           PIC 9 VALUE ZERO.                    00230127
                                                                        00230225
       01 WS-AGE-CALCULATIONS.                                          00230325
          05 WS-BIRTH-YEAR         PIC 9(4).                            00231025
          05 WS-CURR-DATE.                                              00231125
             10 WS-CURRENT-YEAR    PIC 9(4).                            00231225
             10 WS-CURRENT-MONTH   PIC 9(2).                            00231325
             10 WS-CURRENT-DAY     PIC 9(2).                            00231425
                                                                        00232025
                                                                        00430016
       LINKAGE SECTION.                                                 00450000
       01 DFHCOMMAREA              PIC X.                               00460000
                                                                        00470000
       PROCEDURE DIVISION.                                              00480000
       000-MAIN-PARA.                                                   00490000
           INITIALIZE DATAGROUP                                         00491025
           EVALUATE TRUE                                                00500000
      *       WHEN PROGRAM STARTS FOR THE FIRST TIME                    00510000
              WHEN EIBCALEN = ZERO                                      00520000
                 MOVE LOW-VALUES TO LFORMO                              00530003
                 PERFORM 100-SEND-MAP                                   00540000
                                                                        00550000
      *       WHEN USER PRESSES PF3 RETURN TO LMENU                     00560001
              WHEN EIBAID = DFHPF3                                      00570000
                 PERFORM 300-RETURN-LMENU                               00580007
                                                                        00590000
      *       WHEN USER PRESSES PF4 LINK DB2LOAN PROGRAM                00600001
              WHEN EIBAID = DFHPF4 OR EIBAID = DFHENTER                 00610004
                 PERFORM 400-RECEIVE-DATA                               00620007
                 PERFORM 500-VALIDATE-INPUTS                            00630022
                                                                        00700022
      *       WHEN USER PRESSES PF5 CLEAR FORM                          00710001
              WHEN EIBAID = DFHPF5                                      00720000
                 MOVE LOW-VALUES TO LFORMO                              00740004
                 MOVE 'FORM CLEARED' TO MSGO                            00750004
                 PERFORM 100-SEND-MAP                                   00760001
                                                                        00800000
      *       WHEN USER PRESSES ANY OTHER AID KEY SEND INVAL MESSAGE    00810000
              WHEN OTHER                                                00820000
                  MOVE 'INVALID KEY PRESSED.' TO MSGO                   00830001
                  PERFORM 100-SEND-MAP                                  00840000
                                                                        00850000
           END-EVALUATE                                                 00860000
      *                                                                 00870004
           EXEC CICS RETURN                                             00880004
                TRANSID('FORM')                                         00890004
                COMMAREA(COMMUNICATION-AREA)                            00900004
           END-EXEC                                                     00910025
           EXIT.                                                        00920000
                                                                        00930000
      *    SEND MAP, CLEAR THE SCREEN FROM USER INPUT                   00940003
       100-SEND-MAP.                                                    00950000
           EXEC CICS SEND                                               00960000
                MAP('LFORM')                                            00970001
                MAPSET('LMAPST2')                                       00980001
                FROM(LFORMO)                                            00990003
                ERASE                                                   01000001
                FREEKB                                                  01010004
                RESP(WS-RESPCODE)                                       01020003
           END-EXEC                                                     01030025
           EXIT.                                                        01090000
                                                                        01100004
      *    SEND DATA-ONLY WITHOUT RESETING ANYTHING                     01110020
       200-SEND-DATAONLY.                                               01120007
           EXEC CICS SEND                                               01140007
                MAPSET('LMAPST2')                                       01150007
                MAP('LFORM')                                            01160007
                RESP(WS-RESPCODE)                                       01170009
                DATAONLY                                                01180007
                CURSOR                                                  01190008
           END-EXEC                                                     01200007
                                                                        01250016
           EXIT.                                                        01260007
                                                                        01270007
      *    NAVIGATE BACK TO LOANMENU BY XCTL F3                         01280020
       300-RETURN-LMENU.                                                01290007
           EXEC CICS XCTL                                               01300001
                PROGRAM('LOANMENU')                                     01310003
                RESP(WS-RESPCODE)                                       01320003
           END-EXEC.                                                    01330001
                                                                        01340006
           IF WS-RESPCODE = DFHRESP(NORMAL)                             01350006
              CONTINUE                                                  01360006
           END-IF                                                       01370010
                                                                        01380016
           EXIT.                                                        01390000
                                                                        01400000
      *    RECEIVE DATA FROM USER INPUT BY F4 OR ENTER                  01410006
       400-RECEIVE-DATA.                                                01420007
           MOVE LOW-VALUES TO LFORMI                                    01430022
           EXEC CICS RECEIVE                                            01440004
                MAP('LFORM')                                            01450004
                MAPSET('LMAPST2')                                       01460004
                INTO(LFORMI)                                            01470003
                RESP(WS-RESPCODE)                                       01480003
           END-EXEC                                                     01490025
           EXIT.                                                        01570003
                                                                        01571025
       500-VALIDATE-INPUTS.                                             01580025
           MOVE SSNI   TO WS-SSN-BEGIN                                  01580125
           MOVE SVNTHI TO WS-SSN-MIDDLE                                 01580225
           MOVE RESTI  TO WS-SSN-END                                    01580325
                                                                        01580627
           INSPECT WS-SSN-BEGIN CONVERTING '!"#$%&/()=?' TO SPACES      01580727
                                                                        01580827
           INSPECT WS-SOCIAL-SEC-NUM TALLYING WS-SSN-LENGTH             01580925
                   FOR CHARACTERS BEFORE INITIAL SPACE                  01581025
                                                                        01581127
           EVALUATE TRUE                                                01581225
              WHEN WS-SSN-LENGTH < 11                                   01581825
                 MOVE LOW-VALUES TO LFORMO                              01581925
                 MOVE 'INVALID SOCIAL SECURITY NUMBER'                  01582027
                      TO MSGO                                           01582125
                 MOVE -1 TO SSNL                                        01582225
                 PERFORM 200-SEND-DATAONLY                              01582325
                                                                        01582425
              WHEN FNAMEI = SPACES OR FNAMEI = LOW-VALUES               01582525
                 MOVE 'FILL FIRST NAME FIELD' TO MSGO                   01582625
                 MOVE -1 TO FNAMEL                                      01582725
                 PERFORM 200-SEND-DATAONLY                              01582825
                                                                        01582925
              WHEN LNAMEI = SPACES OR LNAMEI = LOW-VALUES               01583025
                 MOVE 'FILL LAST NAME FIELD' TO MSGO                    01583125
                 MOVE -1 TO LNAMEL                                      01583225
                 PERFORM 200-SEND-DATAONLY                              01583325
                                                                        01583425
              WHEN LAMNTI = SPACES OR LAMNTI = LOW-VALUES               01583525
                 MOVE 'FILL LOAN AMOUNT FIELD' TO MSGO                  01583625
                 MOVE -1 TO LAMNTL                                      01583725
                 PERFORM 200-SEND-DATAONLY                              01583825
                                                                        01583925
              WHEN LPRDI = SPACES OR LPRDI = LOW-VALUES                 01584025
                 MOVE 'FILL LOAN PERIOD FIELD' TO MSGO                  01584125
                 MOVE -1 TO LPRDL                                       01584225
                 PERFORM 200-SEND-DATAONLY                              01584325
                                                                        01584425
              WHEN OTHER                                                01584525
                 PERFORM 510-VALIDATE-MIDDLE-CHAR                       01584626
                                                                        01584725
           END-EVALUATE                                                 01584825
           EXIT.                                                        01584925
                                                                        01585026
       510-VALIDATE-MIDDLE-CHAR.                                        01585126
           EVALUATE WS-SSN-MIDDLE                                       01585226
              WHEN '-'                                                  01585326
              WHEN 'A'                                                  01585426
              WHEN 'B'                                                  01585526
              WHEN 'C'                                                  01585626
              WHEN 'C'                                                  01585726
              WHEN 'D'                                                  01585826
              WHEN 'E'                                                  01585926
              WHEN 'F'                                                  01586026
                 PERFORM 520-VALIDATE-DOB-FIELD                         01586126
              WHEN OTHER                                                01586226
                 MOVE 'INVALID MIDDLE CHARACTER' TO MSGO                01586326
                 MOVE -1 TO SVNTHL                                      01586426
                 PERFORM 200-SEND-DATAONLY                              01586526
           END-EVALUATE                                                 01586626
           EXIT.                                                        01586726
                                                                        01586825
       520-VALIDATE-DOB-FIELD.                                          01586926
           EVALUATE TRUE                                                01588725
              WHEN WS-DOB-MONTH <= ZERO OR WS-DOB-MONTH > 12            01589325
                 MOVE  'INVALID MONTH' TO MSGO                          01589425
                 MOVE -1 TO SSNL                                        01589525
                 PERFORM 200-SEND-DATAONLY                              01589625
                                                                        01589725
              WHEN WS-DOB-YEAR < ZERO                                   01589825
                 MOVE  'INVALID YEAR' TO MSGO                           01589925
                 MOVE -1 TO SSNL                                        01590025
                 PERFORM 200-SEND-DATAONLY                              01590125
                                                                        01592026
              WHEN OTHER                                                01592125
      * CALCULATE THE BIRTH YEAR                                        01592226
                 EVALUATE WS-SSN-MIDDLE                                 01592326
                    WHEN '-'                                            01592426
                        MOVE 1900 TO WS-BIRTH-YEAR                      01592526
                    WHEN OTHER                                          01592626
                        MOVE 2000 TO WS-BIRTH-YEAR                      01592726
                 END-EVALUATE                                           01592826
                 MOVE WS-DOB-YEAR TO WS-BIRTH-YEAR(3:2)                 01592926
                                                                        01593026
      * CHECK IF IT'S LEAP YEAR                                         01593126
                 IF FUNCTION MOD(WS-BIRTH-YEAR, 4) = 0 AND              01593226
                    (FUNCTION MOD(WS-BIRTH-YEAR, 100) NOT = 0 OR        01593326
                    FUNCTION MOD(WS-BIRTH-YEAR, 400) = 0)               01593426
                                                                        01593526
                    SET IS-LEAP-YEAR TO TRUE                            01593626
                 END-IF                                                 01593726
                                                                        01593826
      * EVALUATE DAYS                                                   01593926
                 EVALUATE TRUE                                          01594026
                    WHEN WS-DOB-DAY <= ZERO OR WS-DOB-DAY > 31          01594126
                       MOVE  'INVALID DAY'   TO MSGO                    01594226
                       MOVE -1 TO SSNL                                  01594326
                       PERFORM 200-SEND-DATAONLY                        01594426
                                                                        01594526
                    WHEN (WS-DOB-MONTH = 04 OR WS-DOB-MONTH = 06 OR     01594626
                         WS-DOB-MONTH = 09  OR WS-DOB-MONTH = 11)       01594726
                         AND WS-DOB-DAY > 30                            01594826
                       MOVE  'INVALID DAY'   TO MSGO                    01594926
                       MOVE -1 TO SSNL                                  01595026
                       PERFORM 200-SEND-DATAONLY                        01595126
                                                                        01595226
                    WHEN WS-DOB-MONTH = 02 AND WS-DOB-DAY > 29          01595326
                       MOVE  'INVALID DAY'   TO MSGO                    01595426
                       MOVE -1 TO SSNL                                  01595526
                       PERFORM 200-SEND-DATAONLY                        01595626
                                                                        01595726
                    WHEN WS-DOB-MONTH = 02 AND WS-DOB-DAY > 28 AND      01595826
                         IS-LEAP-YEAR                                   01595926
                       MOVE  'INVALID DAY'   TO MSGO                    01596026
                       MOVE -1 TO SSNL                                  01596126
                       PERFORM 200-SEND-DATAONLY                        01596226
                    WHEN OTHER                                          01596326
                       PERFORM 600-CALCULATE-AGE                        01596426
                                                                        01596526
                 END-EVALUATE                                           01596626
           END-EVALUATE                                                 01596726
           EXIT.                                                        01596826
                                                                        01596926
       600-CALCULATE-AGE.                                               01597026
           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURR-DATE             01597126
                                                                        01597226
           IF WS-CURRENT-YEAR < WS-BIRTH-YEAR                           01597326
              MOVE 'INVALID AGE' TO MSGO                                01597426
              PERFORM 100-SEND-MAP                                      01597526
           ELSE                                                         01597626
              COMPUTE WS-AGE = WS-CURRENT-YEAR - WS-BIRTH-YEAR          01597726
              IF (WS-CURRENT-MONTH < WS-DOB-MONTH) OR                   01597826
                 (WS-CURRENT-MONTH = WS-DOB-MONTH AND                   01597926
                  WS-CURRENT-DAY < WS-DOB-DAY)                          01598026
                 SUBTRACT 1 FROM WS-AGE                                 01598126
                                                                        01598226
              PERFORM 700-SUBMIT-DATA                                   01598326
           END-IF                                                       01598426
           EXIT.                                                        01598526
                                                                        01598626
       700-SUBMIT-DATA.                                                 01598726
           MOVE 'N' TO WS-LINK-C                                        01598826
                                                                        01598926
           SET  SW-INSERT         TO TRUE                               01599026
           MOVE WS-SOCIAL-SEC-NUM TO WS-SSN                             01599126
           MOVE LNAMEI            TO WS-L-NAME                          01599226
           MOVE FNAMEI            TO WS-F-NAME                          01599326
           MOVE ADDRESSI          TO WS-ADDRESS                         01599426
           MOVE POSTCDI           TO WS-POSTAL-CODE                     01599526
           MOVE CITYI             TO WS-CITY                            01599626
           MOVE LAMNTI            TO WS-LOAN-AMN                        01599726
           MOVE LPRDI             TO WS-LOAN-PERIOD                     01599826
           MOVE USAGEI            TO WS-LOAN-USAGE                      01599926
                                                                        01600026
           EXEC CICS LINK                                               01600126
                PROGRAM('LOANDB2')                                      01600226
                COMMAREA(DATAGROUP)                                     01600326
                RESP(WS-RESPCODE)                                       01600426
           END-EXEC                                                     01600526
                                                                        01600626
           EVALUATE TRUE                                                01600726
              WHEN WS-RESPCODE = DFHRESP(NORMAL)                        01600826
      *          MOVE LOW-VALUES TO LFORMO                              01600926
                 MOVE WS-MESSAGE TO MSGO                                01601026
                 MOVE -1 TO SSNL                                        01601126
                 PERFORM 200-SEND-DATAONLY                              01601226
              WHEN WS-RESPCODE = DFHRESP(MAPFAIL)                       01601326
                 MOVE 'MAPFAIL' TO MSGO                                 01601426
                 PERFORM 100-SEND-MAP                                   01601526
           END-EVALUATE                                                 01601626
                                                                        01601726
           EXIT.                                                        01601826
