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
       01 WS-FLD-CNT               PIC 9(7) VALUE ZERO.                 00160008
       01 WS-LINK-C                PIC X VALUE 'N'.                     00170017
       01 I                        PIC 9(1) VALUE 0.                    00180022
       01 WS-S-I                   PIC X VALUE 'N'.                     00190020
       01 V-SSN                    PIC X(6) VALUE SPACES.               00200025
          88 SSN-VALID VALUE '000000' '999999'.                         00210021
                                                                        00211025
       01 MAPDATA-SW               PIC X VALUE 'M'.                     00211126
          88 DATA-ONLY                   VALUE 'D'.                     00211226
                                                                        00211326
       01 LEAP-YEAR                PIC X VALUE SPACE.                   00211426
          88 IS-LEAP-YEAR          VALUE 'Y'.                           00211526
                                                                        00212026
      * VVVVVVVVVVVVV                                                   00220025
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
      * AAAAAAAAAAAAA                                                   00231525
                                                                        00232025
                                                                        00233025
       01 SSNI-VARS.                                                    00240011
          05 WS-VALI-SSNI          PIC X(6) VALUE SPACE.                00250013
          05 WS-7TH                PIC X(1) VALUE SPACE.                00260014
          05 WS-REST               PIC X(4) VALUE SPACE.                00270013
          05 WS-SSN-YY             PIC X(2) VALUE SPACE.                00280013
          05 WS-SSN-DD             PIC X(2) VALUE SPACE.                00290013
          05 WS-SSN-MM             PIC X(2) VALUE SPACE.                00300013
          05 WS-LEAP-Y             PIC X(1) VALUE 'N'.                  00310014
          05 WS-20                 PIC 9(4) VALUE 2000.                 00320012
          05 WS-19                 PIC 9(4) VALUE 1900.                 00330012
      *   05 WS-SSN-NUM            PIC 9(6) VALUE ZERO.                 00340023
          05 WS-DD-NUM             PIC 9(2) VALUE ZERO.                 00350014
          05 WS-MM-NUM             PIC 9(2) VALUE ZERO.                 00360014
          05 WS-YY-NUM             PIC 9(2) VALUE ZERO.                 00370014
          05 WS-CURRENT-DATE       PIC X(21) VALUE SPACE.               00380025
          05 WS-C-Y                PIC 9(4) VALUE ZERO.                 00390014
          05 WS-20-F               PIC X(1) VALUE 'N'.                  00400015
          05 WS-19-F               PIC X(1) VALUE 'N'.                  00410015
          05 WS-SSNI               PIC X(11) VALUE SPACE.               00420016
                                                                        00430016
                                                                        00440016
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
      *          PERFORM 600-VALI-CHAR                                  00640025
      *          PERFORM 650-COUNT-AGE                                  00650025
      *                                                                 00660025
      *          IF WS-LINK-C = 'Y'                                     00670025
      *             PERFORM 900-LINK                                    00680025
      *          END-IF                                                 00690025
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
      *    MOVE LOW-VALUES TO LFORMO                                    01130023
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
                                                                        01601926
      *    VALIDATES SSNI FIELD (FIRST 6 DIGITS)                        01602026
       500-VALIDATE-INPUTS2.                                            01603025
           MOVE 'Y' TO WS-S-I                                           01610022
      *    PERFORM UNTIL WS-S-I = 'N' OR I = 6                          01620023
      *       ADD 1 TO I                                                01630023
      *       IF SSNI(I:1) NOT = '0' OR SSNI(I:1) NOT = '1'             01640023
      *       OR SSNI(I:1) NOT = '2' OR SSNI(I:1) NOT = '3'             01650023
      *       OR SSNI(I:1) NOT = '4' OR SSNI(I:1) NOT = '5'             01660023
      *       OR SSNI(I:1) NOT = '6' OR SSNI(I:1) NOT = '7'             01670023
      *       OR SSNI(I:1) NOT = '8' OR SSNI(I:1) NOT = '9'             01680023
      *          MOVE 'N' TO WS-S-I                                     01690023
      *          MOVE 'ERR' TO MSGO                                     01700023
      *          MOVE -1 TO SSNL                                        01710023
      *          PERFORM 200-SEND-DATAONLY                              01720023
      *          EXIT                                                   01730022
      *       ELSE                                                      01740022
      *          CONTINUE                                               01750022
      *       END-IF                                                    01760023
                                                                        01770022
      *    END-PERFORM.                                                 01780023
      *    IF WS-S-I = 'Y'                                              01790023
      *       PERFORM 550-VALIDATE-LENGTHS                              01800023
      *    ELSE                                                         01810022
      *       MOVE 'ERR VALITSEKKI' TO MSGO                             01820022
      *       MOVE -1 TO SSNL                                           01830022
      *       PERFORM 200-SEND-DATAONLY                                 01840022
      *    END-IF.                                                      01850023
           IF SSNI IS NOT NUMERIC                                       01860023
              MOVE 'ERR VALITSEKKI' TO MSGO                             01870023
              MOVE -1 TO SSNL                                           01880023
              PERFORM 200-SEND-DATAONLY                                 01890023
           ELSE                                                         01900023
              PERFORM 550-VALIDATE-LENGTHS                              01910023
           END-IF                                                       01920023
                                                                        01930023
           EXIT.                                                        01940022
      *    VALIDATE IF REQUIRED INPUT FIELDS ARE EMPTY OR NOT           01950022
      *    BTW: NOWADAYS BUGGY IF IT EVEN WORKS AT ALL                  01960022
       550-VALIDATE-LENGTHS.                                            01970020
      *    MOVE 'Y' TO WS-S-I                                           02000022
      ********      STRING THE SSNI TOGETHER                            02010022
                                                                        02020023
           STRING                                                       02030016
              SSNI   DELIMITED BY SIZE                                  02040016
              SVNTHI DELIMITED BY SIZE                                  02050016
              RESTI  DELIMITED BY SIZE                                  02060016
                 INTO WS-SSNI                                           02070016
           END-STRING                                                   02080016
                                                                        02090023
           INSPECT WS-SSNI TALLYING WS-FLD-CNT FOR CHARACTERS           02100023
           BEFORE INITIAL ' '                                           02110023
                                                                        02120022
                                                                        02130022
           EVALUATE TRUE                                                02140022
              WHEN WS-FLD-CNT < 11                                      02150023
                 MOVE LOW-VALUES TO LFORMO                              02160023
                 MOVE 'SSN 11 CHARS PLEASE' TO MSGO                     02170023
                 MOVE -1 TO SSNL                                        02180023
                 PERFORM 200-SEND-DATAONLY                              02190023
                                                                        02200022
              WHEN FNAMEI = SPACES OR FNAMEI = LOW-VALUES               02210022
                 MOVE 'FILL FIRST NAME FIELD' TO MSGO                   02220022
                 MOVE -1 TO FNAMEL                                      02230022
                 PERFORM 200-SEND-DATAONLY                              02240022
                                                                        02250022
              WHEN LNAMEI = SPACES OR LNAMEI = LOW-VALUES               02260022
                 MOVE 'FILL LAST NAME FIELD' TO MSGO                    02270022
                 MOVE -1 TO LNAMEL                                      02280022
                 PERFORM 200-SEND-DATAONLY                              02290022
                                                                        02300022
              WHEN LAMNTI = SPACES OR LAMNTI = LOW-VALUES               02310022
                 MOVE 'FILL LOAN AMOUNT FIELD' TO MSGO                  02320022
                 MOVE -1 TO LAMNTL                                      02330022
                 PERFORM 200-SEND-DATAONLY                              02340022
                                                                        02350022
              WHEN LPRDI = SPACES OR LPRDI = LOW-VALUES                 02360022
                 MOVE 'FILL LOAN PERIOD FIELD' TO MSGO                  02370022
                 MOVE -1 TO LPRDL                                       02380022
                 PERFORM 200-SEND-DATAONLY                              02390022
                                                                        02400022
              WHEN OTHER                                                02410022
                 CONTINUE                                               02420023
                                                                        02430022
           END-EVALUATE                                                 02440022
      *                                                                 02450008
           EXIT.                                                        02460008
                                                                        02461025
      *     WORKS LIKE A CHARM, AT LEAST THE 7TH CHAR VALIDATE          02470022
       600-VALI-CHAR.                                                   02480019
      *    IF LENGTH OF RESTI < 4                                       02490022
      *       MOVE 'ERR TOO SHORT SSN' TO MSGO                          02500022
      *       MOVE -1 TO SSNL                                           02510022
      *       PERFORM 100-SEND-MAP                                      02520022
      *    ELSE                                                         02530022
      *       CONTINUE                                                  02540022
      *    END-IF.                                                      02550022
                                                                        02560022
           EVALUATE SVNTHI                                              02570016
              WHEN '-'                                                  02580016
                 PERFORM 620-VALI-REF                                   02590019
              WHEN 'A'                                                  02600016
                 PERFORM 620-VALI-REF                                   02610019
              WHEN 'B'                                                  02620016
                 PERFORM 620-VALI-REF                                   02630019
              WHEN 'C'                                                  02640019
                 PERFORM 620-VALI-REF                                   02650019
              WHEN 'C'                                                  02660016
                 PERFORM 620-VALI-REF                                   02670019
              WHEN 'D'                                                  02680016
                 PERFORM 620-VALI-REF                                   02690019
              WHEN 'F'                                                  02700016
                 PERFORM 620-VALI-REF                                   02710019
              WHEN 'E'                                                  02720016
                 PERFORM 620-VALI-REF                                   02730019
              WHEN OTHER                                                02740016
                 MOVE 'INVALID CHAR AFTER BDAY' TO MSGO                 02750022
                 MOVE -1 TO SVNTHL                                      02760016
                 PERFORM 200-SEND-DATAONLY                              02770022
           END-EVALUATE                                                 02780016
           EXIT.                                                        02790019
      *    REF MODS TO MOVE AROUND PIC XS TO NUMBERS AND SO ON          02800022
       620-VALI-REF.                                                    02810019
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                02820019
      *    REF MODS                                                     02830019
           MOVE WS-CURRENT-DATE(1:4) TO WS-C-Y(1:4)                     02840019
                                                                        02850019
      *    MOVE SSNI TO WS-SSN-NUM                                      02860023
                                                                        02870014
           MOVE SSNI(5:2) TO WS-SSN-YY                                  02880016
           MOVE SSNI(3:2) TO WS-SSN-MM                                  02890016
           MOVE SSNI(1:2) TO WS-SSN-DD                                  02900016
           MOVE WS-SSN-YY TO WS-YY-NUM                                  02910013
           MOVE WS-SSN-MM TO WS-MM-NUM                                  02920013
           MOVE WS-SSN-DD TO WS-DD-NUM                                  02930013
                                                                        02940014
      *    CHECKS IF THE MONTHS ARE VALID                               02950022
           IF WS-MM-NUM > 12 OR WS-MM-NUM = 00                          02960021
              MOVE  'VALID MONTHS: 01-12' TO MSGO                       02970014
              MOVE -1 TO SSNL                                           02980014
              PERFORM 100-SEND-MAP                                      02990023
           ELSE                                                         03000016
              CONTINUE                                                  03010016
           END-IF                                                       03020014
           EXIT.                                                        03030017
      *     DONT TOUCH THIS :) WORKS                                    03040022
       650-COUNT-AGE.                                                   03050017
           IF WS-SSN-YY(1:1) = '0'                                      03060016
              ADD WS-YY-NUM TO WS-20                                    03070016
              COMPUTE WS-AGE = WS-C-Y - WS-20                           03080016
              MOVE 'Y' TO WS-20-F                                       03090016
           ELSE                                                         03100016
              ADD WS-YY-NUM TO WS-19                                    03110016
              COMPUTE WS-AGE = WS-C-Y - WS-19                           03120016
              MOVE 'Y' TO WS-19-F                                       03130016
           END-IF                                                       03140016
                                                                        03150016
           IF WS-MM-NUM = 02                                            03160022
              PERFORM 670-VALI-LEAP                                     03170022
           ELSE                                                         03180022
              PERFORM 800-VALI-REST                                     03190022
           END-IF                                                       03200022
                                                                        03210017
           EXIT.                                                        03220017
      *    LEAP YEAR CHECKING, WORKS                                    03230022
       670-VALI-LEAP.                                                   03240017
           EVALUATE TRUE                                                03250015
              WHEN WS-YY-NUM(1:1) = 0 AND WS-DD-NUM > 28                03260022
                 MOVE 'Y' TO WS-20-F                                    03270022
                 PERFORM 700-LEAP-CHECK                                 03280022
                                                                        03290016
              WHEN WS-YY-NUM(1:1) NOT = 0 AND WS-DD-NUM > 28            03300022
                 MOVE 'Y' TO WS-19-F                                    03310022
                 PERFORM 700-LEAP-CHECK                                 03320022
                                                                        03330015
              WHEN OTHER                                                03340015
                 PERFORM 780-VALI-FEB                                   03350019
                                                                        03360015
           END-EVALUATE.                                                03370015
                                                                        03380013
           EXIT.                                                        03390013
      *    MORE LEAP YEAR, WORKS                                        03400022
       700-LEAP-CHECK.                                                  03410017
           IF WS-20-F = 'Y'                                             03420014
              EVALUATE TRUE                                             03430014
                 WHEN FUNCTION MOD(WS-20, 400) = 0                      03440014
                    MOVE 'Y' TO WS-LEAP-Y                               03450014
                                                                        03460014
                 WHEN FUNCTION MOD(WS-20, 100) = 0                      03470015
                    MOVE 'N' TO WS-LEAP-Y                               03480015
                                                                        03490014
                 WHEN FUNCTION MOD(WS-20, 4) = 0                        03500014
                    MOVE 'Y' TO WS-LEAP-Y                               03510014
                                                                        03520014
              END-EVALUATE                                              03530014
              PERFORM 780-VALI-FEB                                      03540017
           END-IF.                                                      03550014
                                                                        03560014
           IF WS-19-F = 'Y'                                             03570014
              EVALUATE TRUE                                             03580014
                 WHEN FUNCTION MOD(WS-19, 400) = 0                      03590014
                    MOVE 'Y' TO WS-LEAP-Y                               03600014
                                                                        03610014
                 WHEN FUNCTION MOD(WS-19, 100) = 0                      03620015
                    MOVE 'N' TO WS-LEAP-Y                               03630015
                                                                        03640014
                 WHEN FUNCTION MOD(WS-19, 4) = 0                        03650014
                    MOVE 'Y' TO WS-LEAP-Y                               03660014
                                                                        03670014
              END-EVALUATE                                              03680014
              PERFORM 780-VALI-FEB                                      03690017
           END-IF.                                                      03700014
                                                                        03710014
           EXIT.                                                        03720013
      *    CHECKS DATES FOR FEBRUARY                                    03730022
       780-VALI-FEB.                                                    03740017
           EVALUATE TRUE                                                03750014
              WHEN WS-LEAP-Y = 'N' AND WS-MM-NUM = 02                   03760014
                                                                        03770018
                                                                        03780014
                 IF WS-DD-NUM > 28 OR WS-DD-NUM < 01                    03790022
                    MOVE 'INVALID DAY FOR FEBRUARY' TO MSGO             03800022
                    MOVE -1 TO SSNL                                     03810022
                    PERFORM 200-SEND-DATAONLY                           03820022
                 ELSE                                                   03830014
                    MOVE 'Y' TO WS-LINK-C                               03840022
                 END-IF                                                 03850014
                                                                        03860014
                                                                        03870014
              WHEN WS-LEAP-Y = 'Y' AND WS-MM-NUM = 02                   03880014
                                                                        03890018
                                                                        03900014
                 IF WS-DD-NUM > 29 OR WS-DD-NUM < 01                    03910022
                    MOVE 'INVALID DAY FOR FEB WITH LEAP-YEAR' TO MSGO   03920022
                    MOVE -1 TO SSNL                                     03930022
                    PERFORM 200-SEND-DATAONLY                           03940022
                 ELSE                                                   03950014
                    MOVE 'N' TO WS-LEAP-Y                               03960022
                    MOVE 'Y' TO WS-LINK-C                               03970022
                 END-IF                                                 03980014
                                                                        03990014
           END-EVALUATE                                                 04000014
                                                                        04010011
           EXIT.                                                        04020011
      *    CHECKS DATES FOR OTHERS THAN FEB                             04030022
       800-VALI-REST.                                                   04040014
           EVALUATE TRUE                                                04050011
              WHEN WS-MM-NUM = 04 OR WS-MM-NUM = 06                     04060014
              OR WS-MM-NUM = 09 OR WS-MM-NUM = 11                       04070014
                                                                        04080014
                 IF WS-DD-NUM >= 31 OR WS-DD-NUM < 01                   04090022
                    MOVE 'INVALID DAY FOR APRIL/JUNE/SEPT/NOVEMBER'     04100011
                       TO MSGO                                          04110011
                    MOVE -1 TO SSNL                                     04120011
                    PERFORM 200-SEND-DATAONLY                           04130022
                 ELSE                                                   04140014
                    MOVE 'Y' TO WS-LINK-C                               04150017
                 END-IF                                                 04160011
                                                                        04170014
           END-EVALUATE                                                 04180014
                                                                        04190014
           EVALUATE TRUE                                                04200014
              WHEN WS-MM-NUM = 01 OR WS-MM-NUM = 03 OR WS-MM-NUM = 05   04210014
              OR WS-MM-NUM = 07 OR WS-MM-NUM = 08 OR WS-MM-NUM = 10     04220014
              OR WS-MM-NUM = 12                                         04230014
                                                                        04240014
                 IF WS-DD-NUM >= 32 OR WS-DD-NUM < 01                   04250022
                    MOVE 'INVALID DAYS FOR JAN/MARCH/JULY/AUG/OCT/DEC'  04260014
                    TO MSGO                                             04270014
                    MOVE -1 TO SSNL                                     04280014
                    PERFORM 200-SEND-DATAONLY                           04290022
                 ELSE                                                   04300014
                    MOVE 'Y' TO WS-LINK-C                               04310017
                 END-IF                                                 04320017
                                                                        04330014
           END-EVALUATE                                                 04340011
                                                                        04350014
                                                                        04360011
           EXIT.                                                        04370011
      *    FINAL LINK                                                   04380022
       900-LINK.                                                        04390017
           MOVE 'N' TO WS-LINK-C                                        04400017
                                                                        04410017
           SET SW-INSERT TO TRUE                                        04420008
           MOVE WS-SSNI  TO WS-SSN                                      04430016
           MOVE LNAMEI   TO WS-L-NAME                                   04440008
           MOVE FNAMEI   TO WS-F-NAME                                   04450008
           MOVE ADDRESSI TO WS-ADDRESS                                  04460008
           MOVE POSTCDI  TO WS-POSTAL-CODE                              04470008
           MOVE CITYI    TO WS-CITY                                     04480008
           MOVE LAMNTI   TO WS-LOAN-AMN                                 04490008
           MOVE LPRDI    TO WS-LOAN-PERIOD                              04500008
           MOVE USAGEI   TO WS-LOAN-USAGE                               04510008
                                                                        04520008
           EXEC CICS LINK                                               04530008
                PROGRAM('LOANDB2')                                      04540008
                COMMAREA(DATAGROUP)                                     04550008
                RESP(WS-RESPCODE)                                       04560008
           END-EXEC                                                     04570008
                                                                        04580008
           EVALUATE TRUE                                                04590008
              WHEN WS-RESPCODE = DFHRESP(NORMAL)                        04600008
                 MOVE LOW-VALUES TO LFORMO                              04611025
                 MOVE WS-MESSAGE TO MSGO                                04612025
                 MOVE -1 TO SSNL                                        04613025
                 PERFORM 200-SEND-DATAONLY                              04614025
              WHEN WS-RESPCODE = DFHRESP(MAPFAIL)                       04620008
                 MOVE 'MAPFAIL' TO MSGO                                 04630008
                 PERFORM 100-SEND-MAP                                   04640008
           END-EVALUATE                                                 04650008
           EXIT.                                                        04710008
                                                                        04720008
