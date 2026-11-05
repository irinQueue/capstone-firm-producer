      *----------------------------------------------------------------*
      * ADBREC  - FIRM / PRODUCER 60-BYTE RECORD (ALL FILES)           *
      * USAGE   - COPY ADBREC REPLACING ==:X:== BY ==PFX==.            *
      * COL 1     TYPE   H=HEADER F=FIRM P=PRODUCER T=TRAILER          *
      * COL 2-9   ID     (H: FILE DATE YYYYMMDD, T: DETAIL COUNT)      *
      * COL 10-17 PARENT FIRM ID OF A PRODUCER                         *
      * COL 18-37 NAME                                                 *
      * COL 38-46 TIN    9 DIGITS ON A PRODUCER                        *
      * COL 47    STATUS                                               *
      * COL 48-60 SPACES                                               *
      *----------------------------------------------------------------*
       01  :X:-REC.
           05  :X:-KEY.
               10  :X:-TYPE        PIC X.
               10  :X:-ID          PIC X(8).
           05  :X:-PARENT          PIC X(8).
           05  :X:-NAME            PIC X(20).
           05  :X:-TIN             PIC X(9).
           05  :X:-STATUS          PIC X.
           05  FILLER              PIC X(13).
