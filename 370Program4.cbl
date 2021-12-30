       IDENTIFICATION DIVISION.
       PROGRAM-ID. CS370PROGRAM4.
       AUTHOR. P W ASKEW.
      *****************************************************************
      * This program brings together the concepts worked on in the 
      * first three programs. These being report spacing, control breaks
      * and multiple files. This project brings sorting and merging 
      * files into the mix.
      *
      * This is also an honors by contract assignment. Due to that, the 
      * sort routine will be more complex. Rather than evaluating the 
      * validity of a record after sorting, records with an error 
      * will be handled before being sorted.
      *
      ***********
      * This program sorts four files with an identical data layout and
      * then merges them into one file. This file is the one used for 
      * creating the output. The output is formatted according to a 
      * Printer spacing chart
      * *****
      * INPUT:
      *    Each NEW-INV-FILE-BX00.txt file contains the following:
      *        1. Warehouse ID (Major Key)
      *        2. Vender ID (Intermediate Key)
      *        3. Candy ID (Minor Key)
      *        4. Candy Data Array
      *            1. Candy Name
      *            2. Candy Box Size
      *            3. Candy Type
      *            4. Cases in Stock
      *            5. Purchase Price
      *
      * *****
      * OUTPUT:
      *        Each detail line of the report contains the following
      *            1. Candy Name
      *            2. Candy Box Size
      *            3. Candy Type
      *            4. Amount in stock
      *            5. Total Cost
      *
      *        Each key level will have a total As well as a grand 
      *        total line
      *
      **********
      * CALCULATIONS
      *    Each value of the detail line is added to the running totals
      *    Those totals will be added to the higher order total after
      *    a control break is triggered.
      *    
      *    The cost is calculated by doing Cases in Stock X Purchase
      *    Price = Cost
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT UNSORTED-B100
               ASSIGN TO 'NEW-INV-FILE-B100.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTED-B100
               ASSIGN TO 'SORTED-B100.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT UNSORTED-B200
               ASSIGN TO 'NEW-INV-FILE-B200.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTED-B200
               ASSIGN TO 'SORTED-B200.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT UNSORTED-B300
               ASSIGN TO 'NEW-INV-FILE-B300.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTED-B300
               ASSIGN TO 'SORTED-B300.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT UNSORTED-B400
               ASSIGN TO 'NEW-INV-FILE-B400.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTED-B400
               ASSIGN TO 'SORTED-B400.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MERGED-SORTED-FILE
               ASSIGN TO 'MERGED-CANDY-FILE.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORT-FILE
               ASSIGN TO 'CANDY-SORTING.tmp'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CANDY-REPORT-FILE
               ASSIGN TO PRINTER 'CANDY-REPORT.txt'.
           
           SELECT ERROR-FILE
               ASSIGN TO 'ERROR.txt'
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
               
       FD UNSORTED-B100
           RECORD CONTAINS 143 CHARACTERS.

       01  UNSORTED-B100-RECORD.
           05  UNS-B100-WAREHOUSE-ID           PIC X(4).
           05  UNS-B100-VENDOR-ID              PIC X.
           05  UNS-B100-CANDY-ID               PIC X(3).
           05  FILLER                          PIC X(135).

       FD SORTED-B100
           RECORD CONTAINS 143 CHARACTERS.

       01  SORTED-B100-RECORD.
           05  S-B100-WAREHOUSE-ID           PIC X(4).
           05  S-B100-VENDOR-ID              PIC X.
           05  S-B100-CANDY-ID               PIC X(3).
           05  FILLER                        PIC X(135).

       FD UNSORTED-B200
           RECORD CONTAINS 143 CHARACTERS.

       01  UNSORTED-B200-RECORD.
           05  UNS-B200-WAREHOUSE-ID           PIC X(4).
           05  UNS-B200-VENDOR-ID              PIC X.
           05  UNS-B200-CANDY-ID               PIC X(3).
           05  FILLER                          PIC X(135).

       FD SORTED-B200
           RECORD CONTAINS 143 CHARACTERS.

       01  SORTED-B200-RECORD.
           05  S-B200-WAREHOUSE-ID           PIC X(4).
           05  S-B200-VENDOR-ID              PIC X.
           05  S-B200-CANDY-ID               PIC X(3).
           05  FILLER                        PIC X(135).


       FD UNSORTED-B300
           RECORD CONTAINS 143 CHARACTERS.

       01  UNSORTED-B300-RECORD.
           05  UNS-B300-WAREHOUSE-ID           PIC X(4).
           05  UNS-B300-VENDOR-ID              PIC X.
           05  UNS-B300-CANDY-ID               PIC X(3).
           05  FILLER                          PIC X(135).

       FD SORTED-B300
           RECORD CONTAINS 143 CHARACTERS.

       01  SORTED-B300-RECORD.
           05  S-B300-WAREHOUSE-ID           PIC X(4).
           05  S-B300-VENDOR-ID              PIC X.
           05  S-B300-CANDY-ID               PIC X(3).
           05  FILLER                        PIC X(135).

       FD UNSORTED-B400
           RECORD CONTAINS 143 CHARACTERS.

       01  UNSORTED-B400-RECORD.
           05  UNS-B400-WAREHOUSE-ID           PIC X(4).
           05  UNS-B400-VENDOR-ID              PIC X.
           05  UNS-B400-CANDY-ID               PIC X(3).
           05  FILLER                          PIC X(135).

       FD SORTED-B400
           RECORD CONTAINS 143 CHARACTERS.

       01  SORTED-B400-RECORD.
           05  S-B400-WAREHOUSE-ID           PIC X(4).
           05  S-B400-VENDOR-ID              PIC X.
           05  S-B400-CANDY-ID               PIC X(3).
           05  FILLER                        PIC X(135).

       SD SORT-FILE
           RECORD CONTAINS 143 CHARACTERS.

       01  SORT-RECORD.
           05  SORT-WAREHOUSE-ID               PIC X(4).
           05  SORT-VENDOR-ID                  PIC X.
           05  SORT-CANDY-ID                   PIC X(3).
           05  FILLER                          PIC X(135).

       FD MERGED-SORTED-FILE
           RECORD CONTAINS 143 CHARACTERS.

       01  MERGED-SORTED-RECORD.
           05  WAREHOUSE-ID-IN                 PIC X(4).
           05  VENDOR-ID-IN                    PIC X.
           05  CANDY-ID-IN                     PIC X(3).
           05  CANDY-DATA OCCURS 5 TIMES.
               10  CANDY-NAME-IN               PIC X(15).
               10  CANDY-BOX-SIZE-IN           PIC A.
               10  CANDY-TYPE-IN               PIC AA.
               10  CANDY-STOCK-IN              PIC S9(4).
               10  PURCHASE-PRICE-IN           PIC S999V99.
       
       FD  ERROR-FILE
           RECORD CONTAINS 143 CHARACTERS.

       01  ERROR-RECORD                        PIC X(143).

       FD  CANDY-REPORT-FILE
           RECORD CONTAINS 68 CHARACTERS.

       01  CANDY-REPORT-RECORD                 PIC X(68).

       WORKING-STORAGE SECTION.
       
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                PIC X       VALUE ' '.
               88 NO-MORE-DATA                     VALUE 'N'.
           05  FIRST-RECORD                        VALUE 'Y'.
           05  FIRST-CANDY                         VALUE 'Y'.
           05  HOLD-WAREHOUSE          PIC X(4).
           05  HOLD-VENDOR             PIC X.
           05  HOLD-CANDY-ID           PIC X(3).

       01  WS-CURRENT-DATE.
           05  WS-YEAR                     PIC 99.
           05  WS-MONTH                    PIC 99.
           05  WS-DAY                      PIC 99.
       
       01  REPORT-FIELDS.
           05  PROPER-SPACING              PIC 9       VALUE 1.
       
       01  CANDY-TOTAL-FIELDS.
           05 TMP-CANDY-STOCK-TOTAL        PIC 99999        VALUE ZERO.
           05 TMP-CANDY-MATH-FIELD         PIC 9(7)v99      VALUE ZERO.
           05 TMP-CANDY-COST-TOTAL         PIC 9(7)v99      VALUE ZERO.
       
       01  VENDOR-TOTAL-FIELDS.
           05  TMP-VENDOR-STOCK-TOTAL      PIC 9(6)        VALUE ZERO.
           05  TMP-VENDOR-MATH-FIELD       PIC 9(8)v99     VALUE ZERO.
           05  TMP-VENDOR-COST-TOTAL       PIC 9(8)v99     VALUE ZERO.

       01  WAREHOUSE-TOTAL-FIELDS.
           05  TMP-WAREHOUSE-STOCK-TOTAL   PIC 9(7)        VALUE ZERO.
           05  TMP-WAREHOUSE-MATH-FIELD    PIC 9(9)v99     VALUE ZERO.
           05  TMP-WAREHOUSE-COST-TOTAL    PIC 9(9)v99     VALUE ZERO.

       01  GRAND-TOTAL-FIELDS.
           05  TMP-GRAND-TOTAL-STOCK       PIC 9(8)        VALUE ZERO.
           05  TMP-GRAND-TOTAL-COST        PIC 9(10)v99    VALUE ZERO.

       01  TEMP-FIELDS.
           05  SUB                     PIC 9           VALUE 1.
           05  CURRENT-LINE.
               10  CL-CANDY-NAME               PIC X(15).
               10  CL-CANDY-BOX-SIZE           PIC A.
               10  CL-CANDY-TYPE               PIC AA.
               10  CL-CANDY-STOCK              PIC S9(4).
               10  CL-PURCHASE-PRICE           PIC S999V99.

      
      *********************    OUTPUT AREA     *************************
       
      **** HEADINGS ****

       01  HEADING-ONE.
           05  FILLER              PIC X(28)   VALUE SPACES.
           05                      PIC X(23)   VALUE 
                                               'BENNET SWEETS AND MORE'.
       
       01  HEADING-ONEV2.
           05 FILLER               PIC X(32)   VALUE SPACES.
           05                      PIC X(15)   VALUE 'BENNET CANDIES'.

       01  HEADING-TWO.
           05                      PIC X(7)        VALUE SPACES.
           05  H2-DATE.
               10  H2-MONTH        PIC XX.
               10  FILLER          PIC X           VALUE '/'.
               10  H2-DAY          PIC XX.
               10  FILLER          PIC X           VALUE '/'.
               10  FILLER          PIC XX          VALUE '20'.
               10  H2-YEAR         PIC XX.
           05                      PIC X(15)       VALUE SPACES.
           05                      PIC X(16)       VALUE 
                                                   'INVENTORY REPORT'.
           05                      PIC X(14)       VALUE SPACES.
           05                      PIC XXX         VALUE 'PWA'.

       01  WAREHOUSE-HEADER.
           05                      PIC X(13)       VALUE 
                                                       '  WAREHOUSE: '.
           05  CURRENT-WAREHOUSE   PIC X(4).
       
       01  VENDOR-HEADER.
           05                      PIC X(13)       VALUE 
                                                       '     VENDOR: '.
           05  CURRENT-VENDOR      PIC X(18).

       01  CANDY-HEADER.
           05                      PIC X(13)       VALUE 
                                                       '      CANDY: '.
           05  CURRENT-CANDY       PIC X(3).

       01  COLUMN-HEADER.
           05                      PIC X(6)        VALUE SPACES.
           05                      PIC X(10)       VALUE 'CANDY NAME'.
           05                      PIC X(8)        VALUE SPACES.
           05                      PIC X(4)        VALUE 'SIZE'.
           05                      PIC X(8)        VALUE SPACES.
           05                      PIC X(4)        VALUE 'TYPE'.
           05                      PIC X(3)        VALUE SPACES.
           05                      PIC X(8)        VALUE 'IN STOCK'.
           05                      PIC X(6)        VALUE SPACES.
           05                      PIC X(10)       VALUE 'TOTAL COST'.

      **** TOTAL LINES ****

       01  CANDY-TOTAL-LINE.
           05                      PIC X(8)        VALUE SPACES.
           05                      PIC X(14)       VALUE 
                                                       'TOTAL CANDY:  '.
           05 TOTAL-CANDY-NAME     PIC X(15).
           05                      PIC X(6)        VALUE SPACES.
           05 TOTAL-CANDY-STOCK    PIC ZZ,ZZ9.
           05                      PIC X(5)        VALUE SPACES.
           05 TOTAL-CANDY-COST     PIC $$,$$$,$$$.99.

       01  VENDOR-TOTAL-LINE.
           05                      PIC X(3)        VALUE SPACES.
           05                      PIC X(19)       VALUE 
                                               'TOTAL FOR VENDOR:  '.
           05 TOTAL-VENDOR-NAME    PIC X(18).
           05                      PIC X(2)        VALUE SPACES.
           05 TOTAL-VENDOR-STOCK   PIC ZZZ,ZZ9.
           05                      PIC X(4)        VALUE SPACES.
           05 TOTAL-VENDOR-COST    PIC $$$,$$$,$$$.99.

       01  WAREHOUSE-TOTAL-LINE.
           05                      PIC X(22)       VALUE 
                                   'TOTAL FOR WAREHOUSE:  '.
           05 TOTAL-WAREHOUSE-NAME PIC X(4).
           05                      PIC X(14)        VALUE SPACES.
           05 TOTAL-WAREHOUSE-STOCK    PIC Z,ZZZ,ZZ9.
           05                      PIC X(3)        VALUE SPACES.
           05 TOTAL-WAREHOUSE-COST PIC $$$$,$$$,$$$.99.

       01  GRAND-TOTAL-LINE.
           05                      PIC X(25)       VALUE SPACES.
           05                      PIC X(14)       VALUE 
                                                   'GRAND TOTAL:  '.
           05 GRAND-TOTAL-STOCK    PIC ZZ,ZZZ,ZZ9.
           05                      PIC X(1)        VALUE SPACES.
           05 GRAND-TOTAL-COST     PIC $$,$$$,$$$,$$$.99.

      **** DETAIL LINE ****
       01  CANDY-DETAIL-LINE.
           05                      PIC X(3)        VALUE SPACES.
           05 DL-CANDY-NAME        PIC X(15).
           05                      PIC X(4)        VALUE SPACES.
           05 DL-CANDY-SIZE        PIC X(10).
           05                      PIC X(5)        VALUE SPACES.
           05 DL-CANDY-TYPE        PIC XX.
           05                      PIC X(5)        VALUE SPACES.
           05 DL-CANDY-STOCK       PIC Z,ZZ9.
           05                      PIC X(5)        VALUE SPACES.
           05 DL-COST              PIC $$,$$$,$$$.99.


       PROCEDURE DIVISION.
           
       10-CONTROL-MODULE.
           OPEN OUTPUT ERROR-FILE
           PERFORM 15-SORT-MERGE
           CLOSE ERROR-FILE
           PERFORM 30-HSKPING-ROUTINE
           PERFORM 50-READ-DATA
           PERFORM 200-CLOSING-ROUTINE
           .

       15-SORT-MERGE.
           SORT SORT-FILE
               ON ASCENDING KEY SORT-WAREHOUSE-ID
               ON ASCENDING KEY SORT-VENDOR-ID
               ON ASCENDING KEY SORT-CANDY-ID
               INPUT PROCEDURE IS 16-B100-INPUT-LOGIC
               GIVING SORTED-B100

           SORT SORT-FILE
               ON ASCENDING KEY SORT-WAREHOUSE-ID
               ON ASCENDING KEY SORT-VENDOR-ID
               ON ASCENDING KEY SORT-CANDY-ID
               INPUT PROCEDURE IS 17-B200-INPUT-LOGIC
               GIVING SORTED-B200

           SORT SORT-FILE
               ON ASCENDING KEY SORT-WAREHOUSE-ID
               ON ASCENDING KEY SORT-VENDOR-ID
               ON ASCENDING KEY SORT-CANDY-ID
               INPUT PROCEDURE IS 18-B300-INPUT-LOGIC
               GIVING SORTED-B300

           SORT SORT-FILE
               ON ASCENDING KEY SORT-WAREHOUSE-ID
               ON ASCENDING KEY SORT-VENDOR-ID
               ON ASCENDING KEY SORT-CANDY-ID
               INPUT PROCEDURE IS 19-B400-INPUT-LOGIC
               GIVING SORTED-B400
       
           MERGE SORT-FILE
               ON ASCENDING KEY SORT-WAREHOUSE-ID
               ON ASCENDING KEY SORT-VENDOR-ID
               ON ASCENDING KEY SORT-CANDY-ID
               USING SORTED-B100,
                     SORTED-B200,
                     SORTED-B300,
                     SORTED-B400
               GIVING MERGED-SORTED-FILE
           
           .
       
       16-B100-INPUT-LOGIC.
           OPEN INPUT UNSORTED-B100
           PERFORM UNTIL NO-MORE-DATA
               READ UNSORTED-B100
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 20-B100-DATA-LOOP
               END-READ
           END-PERFORM
           MOVE ' ' TO EOF-FLAG
           CLOSE UNSORTED-B100
           .
       
       17-B200-INPUT-LOGIC.
           OPEN INPUT UNSORTED-B200
           PERFORM UNTIL NO-MORE-DATA
               READ UNSORTED-B200
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 21-B200-DATA-LOOP
               END-READ
           END-PERFORM
           MOVE ' ' TO EOF-FLAG
           CLOSE UNSORTED-B200
           .

       18-B300-INPUT-LOGIC.
           OPEN INPUT UNSORTED-B300
           PERFORM UNTIL NO-MORE-DATA
               READ UNSORTED-B300
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 22-B300-DATA-LOOP
               END-READ
           END-PERFORM
           MOVE ' ' TO EOF-FLAG
           CLOSE UNSORTED-B300
           .

       19-B400-INPUT-LOGIC.
           OPEN INPUT UNSORTED-B400
           PERFORM UNTIL NO-MORE-DATA
               READ UNSORTED-B400
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 23-B400-DATA-LOOP
               END-READ
           END-PERFORM
           MOVE ' ' TO EOF-FLAG
           CLOSE UNSORTED-B400
           .
       
       20-B100-DATA-LOOP.
           IF UNS-B100-WAREHOUSE-ID IS EQUAL TO 'B100'
               EVALUATE UNS-B100-VENDOR-ID
                   WHEN 'A'
                       MOVE 'ANNI' TO UNS-B100-WAREHOUSE-ID
                       MOVE UNSORTED-B100-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'B'
                       MOVE 'ANNI' TO UNS-B100-WAREHOUSE-ID
                       MOVE UNSORTED-B100-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'N'
                       MOVE 'ANNI' TO UNS-B100-WAREHOUSE-ID
                       MOVE UNSORTED-B100-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'T'
                       MOVE 'ANNI' TO UNS-B100-WAREHOUSE-ID
                       MOVE UNSORTED-B100-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'U'
                       MOVE 'ANNI' TO UNS-B100-WAREHOUSE-ID
                       MOVE UNSORTED-B100-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'X'
                       MOVE 'ANNI' TO UNS-B100-WAREHOUSE-ID
                       MOVE UNSORTED-B100-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN OTHER
                       MOVE UNSORTED-B100-RECORD TO ERROR-RECORD
                       WRITE ERROR-RECORD
               END-EVALUATE

           ELSE
               MOVE UNSORTED-B100-RECORD TO ERROR-RECORD
               WRITE ERROR-RECORD
           END-IF
           .
       21-B200-DATA-LOOP.
           IF UNS-B200-WAREHOUSE-ID IS EQUAL TO 'B200'


               EVALUATE UNS-B200-VENDOR-ID
                   WHEN 'A'
                       MOVE 'BHAM' TO UNS-B200-WAREHOUSE-ID
                       MOVE UNSORTED-B200-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'B'
                       MOVE 'BHAM' TO UNS-B200-WAREHOUSE-ID
                       MOVE UNSORTED-B200-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'N'
                       MOVE 'BHAM' TO UNS-B200-WAREHOUSE-ID
                       MOVE UNSORTED-B200-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'T'
                       MOVE 'BHAM' TO UNS-B200-WAREHOUSE-ID
                       MOVE UNSORTED-B200-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'U'
                       MOVE 'BHAM' TO UNS-B200-WAREHOUSE-ID
                       MOVE UNSORTED-B200-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'X'
                       MOVE 'BHAM' TO UNS-B200-WAREHOUSE-ID
                       MOVE UNSORTED-B200-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN OTHER
                       MOVE UNSORTED-B200-RECORD TO ERROR-RECORD
                       WRITE ERROR-RECORD
               END-EVALUATE

           ELSE
               MOVE UNSORTED-B200-RECORD TO ERROR-RECORD
               WRITE ERROR-RECORD
           END-IF
           .
       22-B300-DATA-LOOP.
           IF UNS-B300-WAREHOUSE-ID IS EQUAL TO 'B300'

               EVALUATE UNS-B300-VENDOR-ID
                   WHEN 'A'
                        MOVE 'GADS' TO UNS-B300-WAREHOUSE-ID
                        MOVE UNSORTED-B300-RECORD TO SORT-RECORD
                        RELEASE SORT-RECORD
                   WHEN 'B'
                        MOVE 'GADS' TO UNS-B300-WAREHOUSE-ID
                        MOVE UNSORTED-B300-RECORD TO SORT-RECORD
                        RELEASE SORT-RECORD
                   WHEN 'N'
                        MOVE 'GADS' TO UNS-B300-WAREHOUSE-ID
                        MOVE UNSORTED-B300-RECORD TO SORT-RECORD
                        RELEASE SORT-RECORD
                   WHEN 'T'
                        MOVE 'GADS' TO UNS-B300-WAREHOUSE-ID
                        MOVE UNSORTED-B300-RECORD TO SORT-RECORD
                        RELEASE SORT-RECORD
                   WHEN 'U'
                        MOVE 'GADS' TO UNS-B300-WAREHOUSE-ID
                        MOVE UNSORTED-B300-RECORD TO SORT-RECORD
                        RELEASE SORT-RECORD
                   WHEN 'X'
                        MOVE 'GADS' TO UNS-B300-WAREHOUSE-ID
                        MOVE UNSORTED-B300-RECORD TO SORT-RECORD
                        RELEASE SORT-RECORD
                   WHEN OTHER
                       MOVE UNSORTED-B300-RECORD TO ERROR-RECORD
                       WRITE ERROR-RECORD
               END-EVALUATE

           ELSE
               MOVE UNSORTED-B300-RECORD TO ERROR-RECORD
               WRITE ERROR-RECORD
           END-IF
           .
       23-B400-DATA-LOOP.
           IF UNS-B400-WAREHOUSE-ID IS EQUAL TO 'B400'

               EVALUATE UNS-B400-VENDOR-ID
                   WHEN 'A'
                       MOVE 'MONT' TO UNS-B400-WAREHOUSE-ID
                       MOVE UNSORTED-B400-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'B'
                       MOVE 'MONT' TO UNS-B400-WAREHOUSE-ID
                       MOVE UNSORTED-B400-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'N'
                       MOVE 'MONT' TO UNS-B400-WAREHOUSE-ID
                       MOVE UNSORTED-B400-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'T'
                       MOVE 'MONT' TO UNS-B400-WAREHOUSE-ID
                       MOVE UNSORTED-B400-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'U'
                       MOVE 'MONT' TO UNS-B400-WAREHOUSE-ID
                       MOVE UNSORTED-B400-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN 'X'
                       MOVE 'MONT' TO UNS-B400-WAREHOUSE-ID
                       MOVE UNSORTED-B400-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
                   WHEN OTHER
                       MOVE UNSORTED-B400-RECORD TO ERROR-RECORD
                       WRITE ERROR-RECORD
               END-EVALUATE

           ELSE
               MOVE UNSORTED-B400-RECORD TO ERROR-RECORD
               WRITE ERROR-RECORD
           END-IF
           .

       30-HSKPING-ROUTINE.
           OPEN INPUT  MERGED-SORTED-FILE
                OUTPUT CANDY-REPORT-FILE

           ACCEPT WS-CURRENT-DATE FROM DATE 

           MOVE WS-MONTH TO H2-MONTH
           MOVE WS-DAY TO H2-DAY
           MOVE WS-YEAR TO H2-YEAR

           PERFORM 40-PAGE-HEADER

       .

       40-PAGE-HEADER.
           WRITE CANDY-REPORT-RECORD FROM HEADING-ONE
               AFTER ADVANCING PAGE
           MOVE 2 TO PROPER-SPACING
           WRITE CANDY-REPORT-RECORD FROM HEADING-TWO
               AFTER ADVANCING PROPER-SPACING
       .

       50-READ-DATA.
           PERFORM UNTIL NO-MORE-DATA
               READ MERGED-SORTED-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 60-PROCESS-DATA
               END-READ
           END-PERFORM
       .
           
       60-PROCESS-DATA.
           PERFORM 70-CONTROL-BREAK-CHECK
      ***************DO TOTAL STUFF TOO************
           PERFORM VARYING SUB
               FROM 1 BY 1 UNTIL SUB > 5
               IF CANDY-DATA (SUB) IS NOT EQUAL TO SPACES
                   MOVE CANDY-DATA (SUB) TO CURRENT-LINE
                   PERFORM 110-CANDY-WRITE
               END-IF
           END-PERFORM
           MOVE 'Y' TO FIRST-CANDY
       .

       70-CONTROL-BREAK-CHECK.
           EVALUATE TRUE
               WHEN FIRST-RECORD EQUALS 'Y'
                   MOVE 'N' TO FIRST-RECORD
                   MOVE WAREHOUSE-ID-IN TO HOLD-WAREHOUSE
                   MOVE VENDOR-ID-IN TO HOLD-VENDOR
                   MOVE CANDY-ID-IN TO HOLD-CANDY-ID
                   PERFORM 75-PRINT-FIRST-HEADER
                   PERFORM 90-PRINT-VENDOR-HEADER
                   PERFORM 100-PRINT-CANDY-HEADER
               WHEN WAREHOUSE-ID-IN IS NOT EQUAL TO HOLD-WAREHOUSE
                   PERFORM 120-WAREHOUSE-BREAK
                   MOVE WAREHOUSE-ID-IN TO HOLD-WAREHOUSE
                   MOVE VENDOR-ID-IN TO HOLD-VENDOR
                   MOVE CANDY-ID-IN TO HOLD-CANDY-ID
                   PERFORM 80-PRINT-WAREHOUSE-HEADER
                   PERFORM 90-PRINT-VENDOR-HEADER
                   PERFORM 100-PRINT-CANDY-HEADER
               WHEN VENDOR-ID-IN IS NOT EQUAL TO HOLD-VENDOR
                   PERFORM 130-VENDOR-BREAK
                   MOVE VENDOR-ID-IN TO HOLD-VENDOR
                   MOVE CANDY-ID-IN TO HOLD-CANDY-ID
                   MOVE 3 TO PROPER-SPACING
                   PERFORM 90-PRINT-VENDOR-HEADER
                   MOVE 2 TO PROPER-SPACING
                   PERFORM 100-PRINT-CANDY-HEADER
               WHEN CANDY-ID-IN IS NOT EQUAL TO HOLD-CANDY-ID
                   PERFORM 140-CANDY-BREAK
                   MOVE CANDY-ID-IN TO HOLD-CANDY-ID
                   MOVE 3 TO PROPER-SPACING
                   PERFORM 100-PRINT-CANDY-HEADER
           END-EVALUATE
       .

       75-PRINT-FIRST-HEADER.
           MOVE HOLD-WAREHOUSE TO CURRENT-WAREHOUSE
               WRITE CANDY-REPORT-RECORD FROM WAREHOUSE-HEADER
               AFTER ADVANCING PROPER-SPACING
       .

       80-PRINT-WAREHOUSE-HEADER.
           MOVE HOLD-WAREHOUSE TO CURRENT-WAREHOUSE
           WRITE CANDY-REPORT-RECORD FROM HEADING-ONEV2
               AFTER ADVANCING PAGE
           WRITE CANDY-REPORT-RECORD FROM HEADING-TWO
               AFTER ADVANCING PROPER-SPACING
           WRITE CANDY-REPORT-RECORD FROM WAREHOUSE-HEADER
               AFTER ADVANCING PROPER-SPACING
       .

       90-PRINT-VENDOR-HEADER.
           EVALUATE HOLD-VENDOR
               WHEN 'A'
                  MOVE 'Atomic Sweets' TO CURRENT-VENDOR
               WHEN 'B'
                   MOVE 'Boston Sweets' TO CURRENT-VENDOR
               WHEN 'N'
                   MOVE 'Nellies Sweet Shop' TO CURRENT-VENDOR
               WHEN 'T'
                   MOVE 'Tiger Treats' TO CURRENT-VENDOR
               WHEN 'U'
                   MOVE 'Unity Candy' TO CURRENT-VENDOR
               WHEN 'X'
                   MOVE 'Xtra Candies' TO CURRENT-VENDOR
               WHEN OTHER
                   MOVE 'INVALID VENDOR' TO CURRENT-VENDOR
                  CONTINUE
           END-EVALUATE
           

           

           WRITE CANDY-REPORT-RECORD FROM VENDOR-HEADER
               AFTER ADVANCING PROPER-SPACING
       .

       100-PRINT-CANDY-HEADER.
           MOVE HOLD-CANDY-ID TO CURRENT-CANDY
           WRITE CANDY-REPORT-RECORD FROM CANDY-HEADER
               AFTER ADVANCING PROPER-SPACING
           MOVE 2 TO PROPER-SPACING
           WRITE CANDY-REPORT-RECORD FROM COLUMN-HEADER
               AFTER ADVANCING PROPER-SPACING
       .
       110-CANDY-WRITE.
       IF CANDY-DATA (SUB) IS NOT EQUAL TO SPACES
           MOVE CANDY-DATA (SUB) TO CURRENT-LINE


           EVALUATE CL-CANDY-BOX-SIZE
               WHEN 'L'
                  MOVE 'Large' TO DL-CANDY-SIZE
               WHEN 'M'
                   MOVE 'Medium' TO DL-CANDY-SIZE
               WHEN 'S'
                   MOVE 'Small' TO DL-CANDY-SIZE
               WHEN 'F'
                   MOVE 'Fundraiser' TO DL-CANDY-SIZE
               WHEN 'X'
                   MOVE 'Sample' TO DL-CANDY-SIZE
               WHEN OTHER 
                   MOVE 'BAD-' TO DL-CANDY-SIZE
                   MOVE CL-CANDY-BOX-SIZE TO DL-CANDY-SIZE(5:1)
           END-EVALUATE
           

           IF CL-CANDY-TYPE = 'SU' OR CL-CANDY-TYPE = 'SF'
           MOVE CL-CANDY-TYPE TO DL-CANDY-TYPE
           ELSE 
               MOVE '**' TO DL-CANDY-TYPE
           END-IF

           IF CL-CANDY-STOCK IS NUMERIC
               ADD CL-CANDY-STOCK TO TMP-CANDY-STOCK-TOTAL GIVING 
                                               TMP-CANDY-STOCK-TOTAL
               ADD CL-CANDY-STOCK TO TMP-VENDOR-STOCK-TOTAL GIVING
                                               TMP-VENDOR-STOCK-TOTAL
               ADD CL-CANDY-STOCK TO TMP-WAREHOUSE-STOCK-TOTAL GIVING
                                               TMP-WAREHOUSE-STOCK-TOTAL
               ADD CL-CANDY-STOCK TO TMP-GRAND-TOTAL-STOCK GIVING
                                               TMP-GRAND-TOTAL-STOCK
               MOVE CL-CANDY-STOCK TO DL-CANDY-STOCK


           IF CL-PURCHASE-PRICE IS NUMERIC
               MULTIPLY CL-PURCHASE-PRICE BY CL-CANDY-STOCK GIVING
                                               TMP-CANDY-MATH-FIELD
               ADD TMP-CANDY-MATH-FIELD TO TMP-CANDY-COST-TOTAL GIVING
                                               TMP-CANDY-COST-TOTAL
               ADD TMP-CANDY-MATH-FIELD TO TMP-VENDOR-COST-TOTAL GIVING
                                               TMP-VENDOR-COST-TOTAL
               ADD TMP-CANDY-MATH-FIELD TO TMP-WAREHOUSE-COST-TOTAL 
                               GIVING TMP-WAREHOUSE-COST-TOTAL
               ADD TMP-CANDY-MATH-FIELD TO TMP-GRAND-TOTAL-COST
                               GIVING TMP-GRAND-TOTAL-COST

               MOVE TMP-CANDY-MATH-FIELD TO DL-COST 
               MOVE ZEROS TO TMP-CANDY-MATH-FIELD

           IF FIRST-CANDY IS EQUAL TO 'Y'
              MOVE CL-CANDY-NAME TO DL-CANDY-NAME
              MOVE CL-CANDY-NAME TO TOTAL-CANDY-NAME
              MOVE 'N' TO FIRST-CANDY
              MOVE 2 TO PROPER-SPACING
           ELSE
              MOVE SPACES TO DL-CANDY-NAME
              MOVE 1 TO PROPER-SPACING
           END-IF

               
               WRITE CANDY-REPORT-RECORD FROM CANDY-DETAIL-LINE
                   AFTER ADVANCING PROPER-SPACING
               MOVE 2 TO PROPER-SPACING

           ELSE
               MOVE ZEROS TO DL-COST
           END-IF

           ELSE
               MOVE ZEROS TO DL-CANDY-STOCK
           END-IF
       
       .

       120-WAREHOUSE-BREAK.
           PERFORM 130-VENDOR-BREAK
           MOVE HOLD-WAREHOUSE TO TOTAL-WAREHOUSE-NAME
           MOVE TMP-WAREHOUSE-STOCK-TOTAL TO TOTAL-WAREHOUSE-STOCK
           MOVE ZEROS TO TMP-WAREHOUSE-STOCK-TOTAL

           MOVE TMP-WAREHOUSE-COST-TOTAL TO TOTAL-WAREHOUSE-COST
           MOVE ZEROS TO TMP-WAREHOUSE-COST-TOTAL
           WRITE CANDY-REPORT-RECORD FROM WAREHOUSE-TOTAL-LINE
               AFTER ADVANCING PROPER-SPACING
       .

       130-VENDOR-BREAK.
           PERFORM 140-CANDY-BREAK
           MOVE CURRENT-VENDOR TO TOTAL-VENDOR-NAME
           MOVE TMP-VENDOR-STOCK-TOTAL TO TOTAL-VENDOR-STOCK
           MOVE ZEROS TO TMP-VENDOR-STOCK-TOTAL

           MOVE TMP-VENDOR-COST-TOTAL TO TOTAL-VENDOR-COST
           MOVE ZEROS TO TMP-VENDOR-COST-TOTAL
           WRITE CANDY-REPORT-RECORD FROM VENDOR-TOTAL-LINE
               AFTER ADVANCING PROPER-SPACING
       .

       140-CANDY-BREAK.
           MOVE TMP-CANDY-STOCK-TOTAL TO TOTAL-CANDY-STOCK
           MOVE ZEROS TO TMP-CANDY-STOCK-TOTAL

           MOVE TMP-CANDY-COST-TOTAL TO TOTAL-CANDY-COST
           MOVE ZEROS TO TMP-CANDY-COST-TOTAL

           WRITE CANDY-REPORT-RECORD FROM CANDY-TOTAL-LINE
               AFTER ADVANCING PROPER-SPACING
           
           MOVE 'Y' TO FIRST-CANDY
       .    

       200-CLOSING-ROUTINE.
           PERFORM 120-WAREHOUSE-BREAK
           
           MOVE TMP-GRAND-TOTAL-STOCK TO GRAND-TOTAL-STOCK
           MOVE TMP-GRAND-TOTAL-COST TO GRAND-TOTAL-COST

           WRITE CANDY-REPORT-RECORD FROM GRAND-TOTAL-LINE
               AFTER ADVANCING PROPER-SPACING

           CLOSE MERGED-SORTED-FILE
                 CANDY-REPORT-FILE

           STOP RUN
           
       .
       
