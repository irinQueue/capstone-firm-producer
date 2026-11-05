      *================================================================*
      * PROGRAM  : INTAKE                                              *
      * SYSTEM   : FIRM-PRODUCER DAILY SYNC (TRAINING CAPSTONE)        *
      * PURPOSE  : VALIDATE THE DAILY FILE DATA/LNA.DAT.               *
      *            FILE CHECKS : NO-HDR, NO-TRL, BAD-CNT  (RC 8)        *
      *            RECORD RULES: BLANK-ID, BAD-TIN, NO-FIRM (1ST WINS)  *
      * OUTPUTS  : OUT/VALID.DAT  OUT/REJECT.DAT  OUT/INTAKE.RPT       *
      * RC       : 0 ACCEPTED, 8 FILE REJECTED, 12 OPEN FAILURE        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTAKE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LNA-FILE    ASSIGN TO "data/LNA.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-LNA.
           SELECT VALID-FILE  ASSIGN TO "out/VALID.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-VALID.
           SELECT REJECT-FILE ASSIGN TO "out/REJECT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-REJECT.
           SELECT REPORT-FILE ASSIGN TO "out/INTAKE.RPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-REPORT.

       DATA DIVISION.
       FILE SECTION.
       FD  LNA-FILE.
       01  LNA-LINE                PIC X(60).

       FD  VALID-FILE.
       01  VALID-LINE              PIC X(60).

       FD  REJECT-FILE.
       01  REJECT-LINE.
           05  RJ-RECORD           PIC X(60).
           05  RJ-GAP              PIC X.
           05  RJ-REASON           PIC X(8).

       FD  REPORT-FILE.
       01  REPORT-LINE             PIC X(40).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-LNA           PIC XX.
           05  WS-FS-VALID         PIC XX.
           05  WS-FS-REJECT        PIC XX.
           05  WS-FS-REPORT        PIC XX.

       01  WS-SWITCHES.
           05  WS-EOF-SW           PIC X      VALUE "N".
               88  END-OF-LNA                 VALUE "Y".
           05  WS-FIRM-FOUND-SW    PIC X      VALUE "N".
               88  FIRM-FOUND                 VALUE "Y".

       01  WS-LIMITS.
           05  WS-MAX-RECS         PIC 9(4)   VALUE 500.

      * WHOLE FILE IS HELD IN MEMORY: A PRODUCER MAY APPEAR BEFORE
      * THE FIRM IT REFERS TO, SO ALL FIRMS MUST BE KNOWN FIRST.
       01  WS-FILE-TABLE.
           05  WS-LINE-ENTRY       PIC X(60)  OCCURS 500 TIMES.
       01  WS-LINE-COUNT           PIC 9(4)   VALUE ZERO.

       01  WS-GOOD-FIRMS.
           05  WS-GOOD-FIRM-ID     PIC X(8)   OCCURS 500 TIMES.
       01  WS-GOOD-FIRM-COUNT      PIC 9(4)   VALUE ZERO.

       01  WS-SUBSCRIPTS.
           05  WS-IX               PIC 9(4).
           05  WS-FX               PIC 9(4).
           05  WS-LAST-DETAIL      PIC 9(4).

       COPY ADBREC REPLACING ==:X:== BY ==CUR==.

       01  WS-FILE-REASON          PIC X(8)   VALUE SPACES.
       01  WS-REC-REASON           PIC X(8)   VALUE SPACES.
       01  WS-TRAILER-COUNT        PIC 9(8)   VALUE ZERO.

       01  WS-COUNTERS.
           05  WS-CNT-READ         PIC 9(4)   VALUE ZERO.
           05  WS-CNT-VALID        PIC 9(4)   VALUE ZERO.
           05  WS-CNT-REJECT       PIC 9(4)   VALUE ZERO.
           05  WS-CNT-FIRMS        PIC 9(4)   VALUE ZERO.
           05  WS-CNT-PRODS        PIC 9(4)   VALUE ZERO.

       01  WS-RPT-DETAIL.
           05  WS-RPT-LABEL        PIC X(24).
           05  WS-RPT-COUNT        PIC ZZZ9.
           05  FILLER              PIC X(12)  VALUE SPACES.
       01  WS-RPT-TEXT REDEFINES WS-RPT-DETAIL.
           05  FILLER              PIC X(24).
           05  WS-RPT-VALUE        PIC X(16).

       PROCEDURE DIVISION.
       0000-MAINLINE.
           PERFORM 1000-LOAD-DAILY-FILE
           PERFORM 2000-CHECK-FILE
           PERFORM 3000-OPEN-OUTPUTS
           IF WS-FILE-REASON = SPACES
               PERFORM 4000-VALIDATE-RECORDS
               PERFORM 6000-WRITE-CONTROL-REPORT
               MOVE 0 TO RETURN-CODE
           ELSE
               PERFORM 7000-REJECT-WHOLE-FILE
               MOVE 8 TO RETURN-CODE
           END-IF
           PERFORM 9000-CLOSE-OUTPUTS
           DISPLAY "INTAKE: READ " WS-CNT-READ
                   " VALID " WS-CNT-VALID
                   " REJECT " WS-CNT-REJECT
                   " FILE-REASON [" WS-FILE-REASON "]"
           STOP RUN.

      *----------------------------------------------------------------*
       1000-LOAD-DAILY-FILE.
           OPEN INPUT LNA-FILE
           IF WS-FS-LNA NOT = "00"
               DISPLAY "INTAKE: OPEN FAILED data/LNA.DAT STATUS "
                       WS-FS-LNA
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM UNTIL END-OF-LNA
               READ LNA-FILE
                   AT END
                       SET END-OF-LNA TO TRUE
                   NOT AT END
                       IF WS-LINE-COUNT < WS-MAX-RECS
                           ADD 1 TO WS-LINE-COUNT
                           MOVE LNA-LINE
                             TO WS-LINE-ENTRY (WS-LINE-COUNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LNA-FILE.

      *----------------------------------------------------------------*
      * FILE-LEVEL CHECKS, IN ORDER. THE FIRST ONE THAT FAILS WINS.
       2000-CHECK-FILE.
           IF WS-LINE-COUNT = ZERO
               MOVE "NO-HDR" TO WS-FILE-REASON
           ELSE
               IF WS-LINE-ENTRY (1) (1:1) NOT = "H"
                   MOVE "NO-HDR" TO WS-FILE-REASON
               ELSE
                   IF WS-LINE-ENTRY (WS-LINE-COUNT) (1:1) NOT = "T"
                       MOVE "NO-TRL" TO WS-FILE-REASON
                   END-IF
               END-IF
           END-IF
           IF WS-FILE-REASON = SPACES
               PERFORM 2100-CHECK-TRAILER-COUNT
           END-IF.

       2100-CHECK-TRAILER-COUNT.
           COMPUTE WS-LAST-DETAIL = WS-LINE-COUNT - 1
           PERFORM VARYING WS-IX FROM 2 BY 1
                   UNTIL WS-IX > WS-LAST-DETAIL
               IF WS-LINE-ENTRY (WS-IX) (1:1) = "F"
               OR WS-LINE-ENTRY (WS-IX) (1:1) = "P"
                   ADD 1 TO WS-CNT-READ
               END-IF
           END-PERFORM
           MOVE WS-LINE-ENTRY (WS-LINE-COUNT) TO CUR-REC
           IF CUR-ID IS NOT NUMERIC
               MOVE "BAD-CNT" TO WS-FILE-REASON
           ELSE
               MOVE CUR-ID TO WS-TRAILER-COUNT
               IF WS-TRAILER-COUNT NOT = WS-CNT-READ
                   MOVE "BAD-CNT" TO WS-FILE-REASON
               END-IF
           END-IF
           IF WS-FILE-REASON NOT = SPACES
               MOVE ZERO TO WS-CNT-READ
           END-IF.

      *----------------------------------------------------------------*
       3000-OPEN-OUTPUTS.
           OPEN OUTPUT VALID-FILE
           IF WS-FS-VALID NOT = "00"
               DISPLAY "INTAKE: OPEN FAILED out/VALID.DAT STATUS "
                       WS-FS-VALID
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT REJECT-FILE
           IF WS-FS-REJECT NOT = "00"
               DISPLAY "INTAKE: OPEN FAILED out/REJECT.DAT STATUS "
                       WS-FS-REJECT
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-FS-REPORT NOT = "00"
               DISPLAY "INTAKE: OPEN FAILED out/INTAKE.RPT STATUS "
                       WS-FS-REPORT
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
       4000-VALIDATE-RECORDS.
           PERFORM 4100-COLLECT-GOOD-FIRMS
           PERFORM VARYING WS-IX FROM 2 BY 1
                   UNTIL WS-IX > WS-LAST-DETAIL
               MOVE WS-LINE-ENTRY (WS-IX) TO CUR-REC
               IF CUR-TYPE = "F" OR CUR-TYPE = "P"
                   PERFORM 4200-APPLY-RULES
                   IF WS-REC-REASON = SPACES
                       PERFORM 5000-WRITE-VALID
                   ELSE
                       PERFORM 5100-WRITE-REJECT
                   END-IF
               END-IF
           END-PERFORM.

      * A FIRM IS GOOD WHEN IT IS IN TODAY'S FILE WITH A NON-BLANK ID.
       4100-COLLECT-GOOD-FIRMS.
           PERFORM VARYING WS-IX FROM 2 BY 1
                   UNTIL WS-IX > WS-LAST-DETAIL
               MOVE WS-LINE-ENTRY (WS-IX) TO CUR-REC
               IF CUR-TYPE = "F" AND CUR-ID NOT = SPACES
                   ADD 1 TO WS-GOOD-FIRM-COUNT
                   MOVE CUR-ID TO WS-GOOD-FIRM-ID (WS-GOOD-FIRM-COUNT)
               END-IF
           END-PERFORM.

      * RULE ORDER: BLANK-ID, THEN BAD-TIN, THEN NO-FIRM.
       4200-APPLY-RULES.
           MOVE SPACES TO WS-REC-REASON
           EVALUATE TRUE
               WHEN CUR-TYPE = "F" AND CUR-ID = SPACES
                   MOVE "BLANK-ID" TO WS-REC-REASON
               WHEN CUR-TYPE = "P" AND CUR-TIN IS NOT NUMERIC
                   MOVE "BAD-TIN" TO WS-REC-REASON
               WHEN CUR-TYPE = "P"
                   PERFORM 4300-FIND-PARENT-FIRM
                   IF NOT FIRM-FOUND
                       MOVE "NO-FIRM" TO WS-REC-REASON
                   END-IF
           END-EVALUATE.

       4300-FIND-PARENT-FIRM.
           MOVE "N" TO WS-FIRM-FOUND-SW
           IF CUR-PARENT NOT = SPACES
               PERFORM VARYING WS-FX FROM 1 BY 1
                       UNTIL WS-FX > WS-GOOD-FIRM-COUNT
                          OR FIRM-FOUND
                   IF WS-GOOD-FIRM-ID (WS-FX) = CUR-PARENT
                       SET FIRM-FOUND TO TRUE
                   END-IF
               END-PERFORM
           END-IF.

      *----------------------------------------------------------------*
       5000-WRITE-VALID.
           MOVE SPACES TO VALID-LINE
           MOVE CUR-REC TO VALID-LINE
           WRITE VALID-LINE
           ADD 1 TO WS-CNT-VALID
           IF CUR-TYPE = "F"
               ADD 1 TO WS-CNT-FIRMS
           ELSE
               ADD 1 TO WS-CNT-PRODS
           END-IF.

      * CLEAR THE WHOLE FD RECORD FIRST: LOW-VALUES IN A LINE
      * SEQUENTIAL WRITE GIVE FILE STATUS 71.
       5100-WRITE-REJECT.
           MOVE SPACES TO REJECT-LINE
           MOVE CUR-REC TO RJ-RECORD
           MOVE WS-REC-REASON TO RJ-REASON
           WRITE REJECT-LINE
           ADD 1 TO WS-CNT-REJECT.

      *----------------------------------------------------------------*
       6000-WRITE-CONTROL-REPORT.
           PERFORM 6100-WRITE-TITLE
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE "FILE DATE" TO WS-RPT-LABEL
           MOVE WS-LINE-ENTRY (1) (2:8) TO WS-RPT-VALUE
           PERFORM 6900-PUT-LINE
           MOVE "RECORDS READ" TO WS-RPT-LABEL
           MOVE WS-CNT-READ TO WS-RPT-COUNT
           PERFORM 6800-PUT-COUNT
           MOVE "RECORDS VALID" TO WS-RPT-LABEL
           MOVE WS-CNT-VALID TO WS-RPT-COUNT
           PERFORM 6800-PUT-COUNT
           MOVE "RECORDS REJECTED" TO WS-RPT-LABEL
           MOVE WS-CNT-REJECT TO WS-RPT-COUNT
           PERFORM 6800-PUT-COUNT
           MOVE "FIRMS VALID" TO WS-RPT-LABEL
           MOVE WS-CNT-FIRMS TO WS-RPT-COUNT
           PERFORM 6800-PUT-COUNT
           MOVE "PRODUCERS VALID" TO WS-RPT-LABEL
           MOVE WS-CNT-PRODS TO WS-RPT-COUNT
           PERFORM 6800-PUT-COUNT.

       6100-WRITE-TITLE.
           MOVE SPACES TO REPORT-LINE
           MOVE "INTAKE CONTROL REPORT" TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE.

       6800-PUT-COUNT.
           MOVE SPACES TO REPORT-LINE
           MOVE WS-RPT-DETAIL TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO WS-RPT-DETAIL.

       6900-PUT-LINE.
           MOVE SPACES TO REPORT-LINE
           MOVE WS-RPT-TEXT TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO WS-RPT-DETAIL.

      *----------------------------------------------------------------*
       7000-REJECT-WHOLE-FILE.
           MOVE SPACES TO REJECT-LINE
           MOVE WS-FILE-REASON TO RJ-REASON
           WRITE REJECT-LINE
           PERFORM 6100-WRITE-TITLE
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE "FILE REJECTED" TO WS-RPT-LABEL
           MOVE WS-FILE-REASON TO WS-RPT-VALUE
           PERFORM 6900-PUT-LINE.

       9000-CLOSE-OUTPUTS.
           CLOSE VALID-FILE REJECT-FILE REPORT-FILE.
