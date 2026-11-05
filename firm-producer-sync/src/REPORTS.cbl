      *================================================================*
      * PROGRAM  : REPORTS                                             *
      * SYSTEM   : FIRM-PRODUCER DAILY SYNC (TRAINING CAPSTONE)        *
      * PURPOSE  : PRINT THE FIRM AND PRODUCER REPORT FROM THE NEW     *
      *            MASTER, AND THE TIN CHANGE REPORT (OLD VS NEW).     *
      * INPUTS   : DATA/MASTER.DAT (OLD)  OUT/NEWMASTER.DAT (NEW)      *
      * OUTPUTS  : OUT/FIRMPROD.RPT  OUT/TINCHG.RPT                    *
      * RC       : 0 NORMAL, 12 OPEN FAILURE                           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OLDMAST-FILE  ASSIGN TO "data/MASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-OLD.
           SELECT NEWMAST-FILE  ASSIGN TO "out/NEWMASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-NEW.
           SELECT FIRMPROD-FILE ASSIGN TO "out/FIRMPROD.RPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-FP.
           SELECT TINCHG-FILE   ASSIGN TO "out/TINCHG.RPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-TC.

       DATA DIVISION.
       FILE SECTION.
       FD  OLDMAST-FILE.
       01  OLDMAST-LINE            PIC X(60).
       FD  NEWMAST-FILE.
       01  NEWMAST-LINE            PIC X(60).
       FD  FIRMPROD-FILE.
       01  FIRMPROD-LINE           PIC X(80).
       FD  TINCHG-FILE.
       01  TINCHG-LINE             PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-OLD           PIC XX.
           05  WS-FS-NEW           PIC XX.
           05  WS-FS-FP            PIC XX.
           05  WS-FS-TC            PIC XX.

       01  WS-EOF-SW               PIC X.
           88  AT-EOF                         VALUE "Y".

       01  WS-OLD-TABLE.
           05  WS-OLD-ENTRY        PIC X(60)  OCCURS 500 TIMES.
       01  WS-OLD-COUNT            PIC 9(4)   VALUE ZERO.
       01  WS-NEW-TABLE.
           05  WS-NEW-ENTRY        PIC X(60)  OCCURS 500 TIMES.
       01  WS-NEW-COUNT            PIC 9(4)   VALUE ZERO.

       01  WS-SUBSCRIPTS.
           05  WS-IX               PIC 9(4).
           05  WS-PX               PIC 9(4).
           05  WS-OX               PIC 9(4).

       COPY ADBREC REPLACING ==:X:== BY ==FRM==.
       COPY ADBREC REPLACING ==:X:== BY ==PRD==.
       COPY ADBREC REPLACING ==:X:== BY ==OLD==.

       01  WS-MATCH-SW             PIC X.
           88  OLD-FOUND                      VALUE "Y".

       01  WS-TOTALS.
           05  WS-TOT-FIRMS        PIC 9(4)   VALUE ZERO.
           05  WS-TOT-PRODS        PIC 9(4)   VALUE ZERO.
           05  WS-TOT-CHANGED      PIC 9(4)   VALUE ZERO.

       01  WS-FIRM-DETAIL.
           05  FILLER              PIC X(5)   VALUE "FIRM ".
           05  FD-ID               PIC X(8).
           05  FILLER              PIC X      VALUE SPACE.
           05  FD-NAME             PIC X(20).
           05  FILLER              PIC X      VALUE SPACE.
           05  FD-STATUS           PIC X.

       01  WS-PROD-DETAIL.
           05  FILLER              PIC X(7)   VALUE "  PROD ".
           05  PD-ID               PIC X(8).
           05  FILLER              PIC X      VALUE SPACE.
           05  PD-NAME             PIC X(20).
           05  FILLER              PIC X      VALUE SPACE.
           05  PD-TIN              PIC X(9).
           05  FILLER              PIC X      VALUE SPACE.
           05  PD-STATUS           PIC X.

       01  WS-TIN-DETAIL.
           05  FILLER              PIC X(5)   VALUE "PROD ".
           05  TD-ID               PIC X(8).
           05  FILLER              PIC X(5)   VALUE " OLD ".
           05  TD-OLD-TIN          PIC X(9).
           05  FILLER              PIC X(5)   VALUE " NEW ".
           05  TD-NEW-TIN          PIC X(9).

       01  WS-TOTAL-DETAIL.
           05  TL-LABEL            PIC X(24).
           05  TL-COUNT            PIC ZZZ9.

       PROCEDURE DIVISION.
       0000-MAINLINE.
           PERFORM 1000-OPEN-FILES
           PERFORM 1100-LOAD-OLD-MASTER
           PERFORM 1200-LOAD-NEW-MASTER
           PERFORM 2000-FIRM-PRODUCER-REPORT
           PERFORM 3000-TIN-CHANGE-REPORT
           CLOSE OLDMAST-FILE NEWMAST-FILE FIRMPROD-FILE TINCHG-FILE
           DISPLAY "REPORTS: FIRMS " WS-TOT-FIRMS
                   " PRODUCERS " WS-TOT-PRODS
                   " TIN CHANGES " WS-TOT-CHANGED
           MOVE 0 TO RETURN-CODE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-OPEN-FILES.
           OPEN INPUT OLDMAST-FILE
           IF WS-FS-OLD NOT = "00"
               DISPLAY "REPORTS: OPEN FAILED data/MASTER.DAT STATUS "
                       WS-FS-OLD
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN INPUT NEWMAST-FILE
           IF WS-FS-NEW NOT = "00"
               DISPLAY "REPORTS: OPEN FAILED out/NEWMASTER.DAT STATUS "
                       WS-FS-NEW
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT FIRMPROD-FILE
           IF WS-FS-FP NOT = "00"
               DISPLAY "REPORTS: OPEN FAILED out/FIRMPROD.RPT STATUS "
                       WS-FS-FP
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT TINCHG-FILE
           IF WS-FS-TC NOT = "00"
               DISPLAY "REPORTS: OPEN FAILED out/TINCHG.RPT STATUS "
                       WS-FS-TC
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF.

       1100-LOAD-OLD-MASTER.
           MOVE "N" TO WS-EOF-SW
           PERFORM UNTIL AT-EOF
               READ OLDMAST-FILE
                   AT END
                       SET AT-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-OLD-COUNT
                       MOVE OLDMAST-LINE
                         TO WS-OLD-ENTRY (WS-OLD-COUNT)
               END-READ
           END-PERFORM.

       1200-LOAD-NEW-MASTER.
           MOVE "N" TO WS-EOF-SW
           PERFORM UNTIL AT-EOF
               READ NEWMAST-FILE
                   AT END
                       SET AT-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-NEW-COUNT
                       MOVE NEWMAST-LINE
                         TO WS-NEW-ENTRY (WS-NEW-COUNT)
               END-READ
           END-PERFORM.

      *----------------------------------------------------------------*
      * NEWMASTER IS IN KEY ORDER (TYPE + ID), SO FIRMS ARE IN ID
      * ORDER AND PRODUCERS ARE IN ID ORDER.
       2000-FIRM-PRODUCER-REPORT.
           MOVE "FIRM AND PRODUCER REPORT" TO WS-TOTAL-DETAIL
           PERFORM 2900-WRITE-FP-TEXT
           MOVE SPACES TO WS-TOTAL-DETAIL
           PERFORM 2900-WRITE-FP-TEXT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > WS-NEW-COUNT
               MOVE WS-NEW-ENTRY (WS-IX) TO FRM-REC
               EVALUATE FRM-TYPE
                   WHEN "F"
                       ADD 1 TO WS-TOT-FIRMS
                       PERFORM 2100-PRINT-FIRM
                   WHEN "P"
                       ADD 1 TO WS-TOT-PRODS
               END-EVALUATE
           END-PERFORM
           MOVE SPACES TO WS-TOTAL-DETAIL
           PERFORM 2900-WRITE-FP-TEXT
           MOVE "TOTAL FIRMS" TO TL-LABEL
           MOVE WS-TOT-FIRMS TO TL-COUNT
           PERFORM 2900-WRITE-FP-TEXT
           MOVE SPACES TO WS-TOTAL-DETAIL
           MOVE "TOTAL PRODUCERS" TO TL-LABEL
           MOVE WS-TOT-PRODS TO TL-COUNT
           PERFORM 2900-WRITE-FP-TEXT.

       2100-PRINT-FIRM.
           MOVE FRM-ID     TO FD-ID
           MOVE FRM-NAME   TO FD-NAME
           MOVE FRM-STATUS TO FD-STATUS
           MOVE SPACES TO FIRMPROD-LINE
           MOVE WS-FIRM-DETAIL TO FIRMPROD-LINE
           WRITE FIRMPROD-LINE
           PERFORM VARYING WS-PX FROM 1 BY 1
                   UNTIL WS-PX > WS-NEW-COUNT
               MOVE WS-NEW-ENTRY (WS-PX) TO PRD-REC
               IF PRD-TYPE = "P" AND PRD-PARENT = FRM-ID
                   PERFORM 2200-PRINT-PRODUCER
               END-IF
           END-PERFORM.

       2200-PRINT-PRODUCER.
           MOVE PRD-ID     TO PD-ID
           MOVE PRD-NAME   TO PD-NAME
           MOVE PRD-TIN    TO PD-TIN
           MOVE PRD-STATUS TO PD-STATUS
           MOVE SPACES TO FIRMPROD-LINE
           MOVE WS-PROD-DETAIL TO FIRMPROD-LINE
           WRITE FIRMPROD-LINE.

       2900-WRITE-FP-TEXT.
           MOVE SPACES TO FIRMPROD-LINE
           MOVE WS-TOTAL-DETAIL TO FIRMPROD-LINE
           WRITE FIRMPROD-LINE.

      *----------------------------------------------------------------*
       3000-TIN-CHANGE-REPORT.
           MOVE SPACES TO TINCHG-LINE
           MOVE "TIN CHANGE REPORT" TO TINCHG-LINE
           WRITE TINCHG-LINE
           MOVE SPACES TO TINCHG-LINE
           WRITE TINCHG-LINE
           PERFORM VARYING WS-PX FROM 1 BY 1
                   UNTIL WS-PX > WS-NEW-COUNT
               MOVE WS-NEW-ENTRY (WS-PX) TO PRD-REC
               IF PRD-TYPE = "P"
                   PERFORM 3100-FIND-OLD-PRODUCER
                   IF OLD-FOUND AND OLD-TIN NOT = PRD-TIN
                       PERFORM 3200-PRINT-TIN-CHANGE
                   END-IF
               END-IF
           END-PERFORM
           MOVE SPACES TO TINCHG-LINE
           WRITE TINCHG-LINE
           MOVE SPACES TO WS-TOTAL-DETAIL
           MOVE "TOTAL CHANGED" TO TL-LABEL
           MOVE WS-TOT-CHANGED TO TL-COUNT
           MOVE SPACES TO TINCHG-LINE
           MOVE WS-TOTAL-DETAIL TO TINCHG-LINE
           WRITE TINCHG-LINE.

       3100-FIND-OLD-PRODUCER.
           MOVE "N" TO WS-MATCH-SW
           PERFORM VARYING WS-OX FROM 1 BY 1
                   UNTIL WS-OX > WS-OLD-COUNT OR OLD-FOUND
               MOVE WS-OLD-ENTRY (WS-OX) TO OLD-REC
               IF OLD-KEY = PRD-KEY
                   SET OLD-FOUND TO TRUE
               END-IF
           END-PERFORM.

       3200-PRINT-TIN-CHANGE.
           ADD 1 TO WS-TOT-CHANGED
           MOVE PRD-ID  TO TD-ID
           MOVE OLD-TIN TO TD-OLD-TIN
           MOVE PRD-TIN TO TD-NEW-TIN
           MOVE SPACES TO TINCHG-LINE
           MOVE WS-TIN-DETAIL TO TINCHG-LINE
           WRITE TINCHG-LINE.
