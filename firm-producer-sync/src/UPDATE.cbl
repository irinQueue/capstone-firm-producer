      *================================================================*
      * PROGRAM  : UPDATE                                              *
      * SYSTEM   : FIRM-PRODUCER DAILY SYNC (TRAINING CAPSTONE)        *
      * PURPOSE  : BALANCED-LINE MATCH OF OUT/VALID.DAT (TODAY) WITH   *
      *            DATA/MASTER.DAT (YESTERDAY), BOTH IN KEY ORDER.     *
      *            KEY = TYPE + ID (COLUMNS 1-9).                      *
      *            ONLY IN VALID   -> ADD      (ACTIVITY "A")          *
      *            IN BOTH         -> REPLACE  (ACTIVITY "C" IF ANY    *
      *                               OF NAME/PARENT/TIN/STATUS DIFFER)*
      *            ONLY IN MASTER  -> CARRY FORWARD (NO ACTIVITY)      *
      * OUTPUTS  : OUT/NEWMASTER.DAT  OUT/ACTIVITY.DAT                 *
      * RC       : 0 NORMAL, 12 OPEN FAILURE                           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VALID-FILE     ASSIGN TO "out/VALID.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-VALID.
           SELECT MASTER-FILE    ASSIGN TO "data/MASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-MASTER.
           SELECT NEWMASTER-FILE ASSIGN TO "out/NEWMASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-NEWMAST.
           SELECT ACTIVITY-FILE  ASSIGN TO "out/ACTIVITY.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-ACTIVITY.

       DATA DIVISION.
       FILE SECTION.
       FD  VALID-FILE.
       01  VALID-LINE              PIC X(60).
       FD  MASTER-FILE.
       01  MASTER-LINE             PIC X(60).
       FD  NEWMASTER-FILE.
       01  NEWMASTER-LINE          PIC X(60).
       FD  ACTIVITY-FILE.
       01  ACTIVITY-LINE.
           05  AC-CODE             PIC X.
           05  AC-RECORD           PIC X(60).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-VALID         PIC XX.
           05  WS-FS-MASTER        PIC XX.
           05  WS-FS-NEWMAST       PIC XX.
           05  WS-FS-ACTIVITY      PIC XX.

       COPY ADBREC REPLACING ==:X:== BY ==TRN==.
       COPY ADBREC REPLACING ==:X:== BY ==OLD==.

      * HIGH-VALUES IN A KEY MEANS THAT FILE IS AT END.
       01  WS-TRN-KEY              PIC X(9).
       01  WS-OLD-KEY              PIC X(9).

       01  WS-ACT-CODE             PIC X.

       01  WS-COUNTERS.
           05  WS-CNT-ADD          PIC 9(4)   VALUE ZERO.
           05  WS-CNT-CHANGE       PIC 9(4)   VALUE ZERO.
           05  WS-CNT-SAME         PIC 9(4)   VALUE ZERO.
           05  WS-CNT-CARRY        PIC 9(4)   VALUE ZERO.
           05  WS-CNT-NEWMAST      PIC 9(4)   VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAINLINE.
           PERFORM 1000-OPEN-FILES
           PERFORM 2100-READ-VALID
           PERFORM 2200-READ-MASTER
           PERFORM 3000-MATCH
               UNTIL WS-TRN-KEY = HIGH-VALUES
                 AND WS-OLD-KEY = HIGH-VALUES
           CLOSE VALID-FILE MASTER-FILE NEWMASTER-FILE ACTIVITY-FILE
           DISPLAY "UPDATE: ADD " WS-CNT-ADD
                   " CHANGE " WS-CNT-CHANGE
                   " SAME " WS-CNT-SAME
                   " CARRY " WS-CNT-CARRY
                   " NEWMASTER " WS-CNT-NEWMAST
           MOVE 0 TO RETURN-CODE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-OPEN-FILES.
           OPEN INPUT VALID-FILE
           IF WS-FS-VALID NOT = "00"
               DISPLAY "UPDATE: OPEN FAILED out/VALID.DAT STATUS "
                       WS-FS-VALID
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN INPUT MASTER-FILE
           IF WS-FS-MASTER NOT = "00"
               DISPLAY "UPDATE: OPEN FAILED data/MASTER.DAT STATUS "
                       WS-FS-MASTER
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT NEWMASTER-FILE
           IF WS-FS-NEWMAST NOT = "00"
               DISPLAY "UPDATE: OPEN FAILED out/NEWMASTER.DAT STATUS "
                       WS-FS-NEWMAST
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN OUTPUT ACTIVITY-FILE
           IF WS-FS-ACTIVITY NOT = "00"
               DISPLAY "UPDATE: OPEN FAILED out/ACTIVITY.DAT STATUS "
                       WS-FS-ACTIVITY
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
       2100-READ-VALID.
           READ VALID-FILE
               AT END
                   MOVE HIGH-VALUES TO WS-TRN-KEY
               NOT AT END
                   MOVE VALID-LINE TO TRN-REC
                   MOVE TRN-KEY TO WS-TRN-KEY
           END-READ.

       2200-READ-MASTER.
           READ MASTER-FILE
               AT END
                   MOVE HIGH-VALUES TO WS-OLD-KEY
               NOT AT END
                   MOVE MASTER-LINE TO OLD-REC
                   MOVE OLD-KEY TO WS-OLD-KEY
           END-READ.

      *----------------------------------------------------------------*
       3000-MATCH.
           EVALUATE TRUE
               WHEN WS-TRN-KEY < WS-OLD-KEY
                   PERFORM 3100-ADD-RECORD
                   PERFORM 2100-READ-VALID
               WHEN WS-TRN-KEY = WS-OLD-KEY
                   PERFORM 3200-REPLACE-RECORD
                   PERFORM 2100-READ-VALID
                   PERFORM 2200-READ-MASTER
               WHEN OTHER
                   PERFORM 3300-CARRY-FORWARD
                   PERFORM 2200-READ-MASTER
           END-EVALUATE.

       3100-ADD-RECORD.
           PERFORM 4000-WRITE-NEW-FROM-TRN
           MOVE "A" TO WS-ACT-CODE
           PERFORM 4100-WRITE-ACTIVITY
           ADD 1 TO WS-CNT-ADD.

       3200-REPLACE-RECORD.
           PERFORM 4000-WRITE-NEW-FROM-TRN
           IF TRN-NAME   NOT = OLD-NAME
           OR TRN-PARENT NOT = OLD-PARENT
           OR TRN-TIN    NOT = OLD-TIN
           OR TRN-STATUS NOT = OLD-STATUS
               MOVE "C" TO WS-ACT-CODE
               PERFORM 4100-WRITE-ACTIVITY
               ADD 1 TO WS-CNT-CHANGE
           ELSE
               ADD 1 TO WS-CNT-SAME
           END-IF.

       3300-CARRY-FORWARD.
           MOVE SPACES TO NEWMASTER-LINE
           MOVE OLD-REC TO NEWMASTER-LINE
           WRITE NEWMASTER-LINE
           ADD 1 TO WS-CNT-NEWMAST
           ADD 1 TO WS-CNT-CARRY.

      *----------------------------------------------------------------*
       4000-WRITE-NEW-FROM-TRN.
           MOVE SPACES TO NEWMASTER-LINE
           MOVE TRN-REC TO NEWMASTER-LINE
           WRITE NEWMASTER-LINE
           ADD 1 TO WS-CNT-NEWMAST.

       4100-WRITE-ACTIVITY.
           MOVE SPACES TO ACTIVITY-LINE
           MOVE WS-ACT-CODE TO AC-CODE
           MOVE TRN-REC TO AC-RECORD
           WRITE ACTIVITY-LINE.
