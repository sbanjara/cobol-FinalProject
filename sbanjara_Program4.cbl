
      ******************* IDENTIFICATION-DIVISION ********************
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROGRAM4.
       AUTHOR.      SBANJARA.
      *
      ****************************************************************
      *
      *  THIS PROGRAM PRODUCES A SUMMARY REPORT FROM THREE INPUT FILES
      *  SHOWING THE LIST OF PRODUCT INVENTORY BROKEN INTO 3 DIFFERENT
      *  WAREHOUSES AND 7 VENDORS.
      *
      *  THERE ARE THREE INPUT FILES IN THIS PROGRAM. THESE INPUT FILES
      *  NEED TO BE SORTED AND MERGGED BEFORE WRITING A SUMMARY REPORT.
      *  ALSO,VALIDATION OF RECORDS AND THEIR FIELDS IS DONE.
      *  EACH INPUT FILES CORRESPONDS A WAREHOUSE AND THE INVENTORY IS
      *  BROKEN INTO VENDORS AND PRODUCT TYPES.
      *
      *  INPUT:
      *     THREE UNSORTED INPUT FILES, EACH RECORD CONTAINING   
      *     WAREHOUSE ID, VENDOR ID, PRODUCT ID, AND PRODUCT DATA 
      *     (NAME, SIZE, TYPE, QUANTITY IN STOCK, AND PURCHASE PRICE)
      *
      *  OUTPUT:
      *      HEADER LINES:
      *         1. 2 REPORT HEADERS INCLUDING DATE AND PAGE NUMBER
      *         2. WAREHOUSE HEADER WITH WAREHOUSE ID
      *         3. VENDOR HEADER WITH EXPANDED VENDOR NAME
      *         4. 2 COLUMNS HEADER BEFORE THE DETAIL LINE
      *
      *      DETAIL LINES:
      *         1. PRODUCT NAME
      *         2. PRODUCT ID
      *         3. PRODUCT SIZE
      *         4. PRODUCT TYPE
      *         5. PRODUCT QUANTITY ON STOCK
      *         6. TOTAL COST = QUANTITY ON STOCK * PURCHASE PRICE
      *    
      *      GROUP LINES:
      *         1. PRODUCT TOTAL LINE INCLUDING PRODUCT NAME
      *         2. VENDOR TOTAL LINE INCLUDING VENDOR NAME
      *         3. WAREHOUSE TOTAL LINE INCLUDING WAREHOUSE ID
      *         4. GRAND TOTAL LINE
      *
      *  CALCULATIONS:
      *      PRODUCT-COST = QUANTITY IN STOCK * PURCHASE PRICE
      *      PRODUCT-TOTAL = PRODUCT-TOTAL + PRODUCT-COST
      *      VENDOR-TOTAL = VENDOR-TOTAL + PRODUCT-TOTAL
      *      WAREHOUSE-TOTAL = WAREHOUSE-TOTAL + VENDOR-TOTAL
      *      GRAND-TOTAL = GRAND-TOTAL + WAREHOUSE TOTAL
      *
      ****************************************************************

      ****************** ENVIRONMENT-DIVISION ************************
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LENEVO-PC.
       OBJECT-COMPUTER. LENEVO-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT UNSORTED-INPUTFILE-NV10
               ASSIGN TO 'PR4F19-NV10.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT UNSORTED-INPUTFILE-CA20
               ASSIGN TO 'PR4F19-CA20.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT UNSORTED-INPUTFILE-WA30
               ASSIGN TO 'PR4F19-WA30.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTED-OUTPUTFILE-NV10
                ASSIGN TO 'OUTPUTFILE-NV10.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTED-OUTPUTFILE-CA20
                ASSIGN TO 'OUTPUTFILE-CA20.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.
        
           SELECT SORTED-OUTPUTFILE-WA30
                ASSIGN TO 'OUTPUTFILE-WA30.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MERGED-PRODUCT-FILE
                ASSIGN TO 'MERGEDSORTEDPRODUCT.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRODUCT-SUMMARY-REPORT
                ASSIGN TO PRINTER 'SUMMARY-REPORT.TXT'.

           SELECT ERROR-FILE
                ASSIGN TO PRINTER 'ERROR-FILE.TXT'.

           SELECT SORTMERGE-FILE
                ASSIGN TO 'SORTMERGE.TMP'.
      *
      ************************ DATA-DIVISION *************************
      *
       DATA DIVISION.

       FILE SECTION.
      
      *
       FD  UNSORTED-INPUTFILE-NV10
           RECORD CONTAINS 128 CHARACTERS.
            
       01  UNSORTED-NV10-RECORD.
           05  UNVR-WAREHOUSEID    PIC X(4).
           05  UNVR-VENDORID       PIC X.
           05  UNVR-PRODUCTID      PIC X(3).
           05  FILLER              PIC X(120).
      *

      *
       FD  UNSORTED-INPUTFILE-CA20
           RECORD CONTAINS 128 CHARACTERS.

       01  UNSORTED-CA20-RECORD.
           05  UCAR-WAREHOUSEID    PIC X(4).
           05  UCAR-VENDORID       PIC X.
           05  UCAR-PRODUCTID      PIC X(3).
           05  FILLER              PIC X(120).
      *

      *
       FD  UNSORTED-INPUTFILE-WA30
           RECORD CONTAINS 128 CHARACTERS.

       01  UNSORTED-WA30-RECORD.
           05  UWAR-WAREHOUSEID    PIC X(4).
           05  UWAR-VENDORID       PIC X.
           05  UWAR-PRODUCTID      PIC X(3).
           05  FILLER              PIC X(120).
      *

      *
       FD  SORTED-OUTPUTFILE-NV10
           RECORD CONTAINS 128 CHARACTERS.

       01  SORTED-NV10-RECORD.
           05  SNVR-WAREHOUSEID     PIC X(4).
           05  SNVR-VENDORID        PIC X.
           05  SNVR-PRODUCTID       PIC X(3).
           05  FILLER               PIC X(120).
      * 

      *
       FD  SORTED-OUTPUTFILE-CA20
           RECORD CONTAINS 128 CHARACTERS.

       01  SORTED-CA20-RECORD.
           05  SCAR-WAREHOUSEID    PIC X(4).
           05  SCAR-VENDORID       PIC X.
           05  SCAR-PRODUCTID      PIC X(3).
           05  FILLER              PIC X(120).
      *

      *
       FD  SORTED-OUTPUTFILE-WA30
           RECORD CONTAINS 128 CHARACTERS.

       01  SORTED-WA30-RECORD.
           05  SWAR-WAREHOUSEID    PIC X(4).
           05  SWAR-VENDORID       PIC X.
           05  SWAR-PRODUCTID      PIC X(3).
           05  FILLER              PIC X(120).
      *

      *
       FD  MERGED-PRODUCT-FILE
           RECORD CONTAINS 128 CHARACTERS.

       01  MERGED-PRODUCT-RECORD.
           05  MPR-WAREHOUSEID           PIC X(4).
           05  MPR-VENDORID              PIC X.
           05  MPR-PRODUCTID             PIC X(3).
           05  MPR-PRODUCT-DATA          OCCURS 5 TIMES.
               10  MPR-PRODUCT-NAME      PIC X(13). 
               10  MPR-PRODUCT-SIZE      PIC A.
               10  MPR-PRODUCT-TYPE      PIC A.
               10  MPR-QUANTITY-INSTOCK  PIC S9(4).
               10  MPR-PURCHASE-PRICE    PIC S999V99.
      *

      *
       SD  SORTMERGE-FILE
            RECORD CONTAINS 128 CHARACTERS.

       01  SORTMERGE-RECORD.
           05  SM-WAREHOUSEID           PIC X(4).
           05  SM-VENDORID              PIC X.
           05  SM-PRODUCTID             PIC X(3).
           05  FILLER                   PIC X(120).
      *

      *
       FD  PRODUCT-SUMMARY-REPORT
           RECORD CONTAINS 70 CHARACTERS.

       01  REPORT-LINE                  PIC X(70).
      *

      *
       FD  ERROR-FILE
           RECORD CONTAINS 128 CHARACTERS.

       01  ERROR-FILE-RECORD            PIC X(128).
      *
      *
      ******************* WORKING-STORAGE ****************************
      *
       WORKING-STORAGE SECTION. 

       01  FLAGS-AND-SWITCHES.
           05  MORE-RECORDS            PIC X(3)    VALUE 'YES'.
           05  FIRST-RECORD            PIC X(3)    VALUE 'YES'.
           05  FIRST-PRODUCT           PIC X(3)    VALUE 'YES'.
		   05  INVALID-RECORD-MESSAGE  PIC X(80).

       01  REPORT-FIELDS.
           05  PROPER-SPACING          PIC 9       VALUE 1.
           05  PAGE-NUM                PIC 99      VALUE 0.
           05  INCORRECT-RECORD-COUNT  PIC 99      VALUE 0.
           05  PRODUCT-SUB             PIC 99.

       01  WS-CURRENT-DATE.
           05  WS-YEAR                 PIC 9(4).
           05  WS-MONTH                PIC 99.
           05  WS-DAY                  PIC 99.

       01  HOLD-FIELDS.
           05  WS-WAREHOUSE-HOLD       PIC X(4).
           05  WS-VENDOR-HOLD          PIC X.
           05  WS-PRODUCT-HOLD         PIC X(3).
		   05  WS-PRODUCT-NAME         PIC X(13).
           05  WS-INVALID-PRODUCT-SIZE PIC X(9).
           05  WS-INVALID-PRODUCT-TYPE PIC X(5).
           05  WS-STOCK-QUANTITY       PIC S9(4).
           05  WS-TOTAL-COST           PIC S9(6)V99.

       01  DETAIL-FIELDS.
           05  DF-VENDOR-NAME          PIC X(12).
           05  DF-VENDOR-NAME-ERROR    PIC X(9).
           05  DF-PRODUCT-TOTAL        PIC 9(7)V99.
           05  DF-VENDOR-TOTAL         PIC 9(7)V99.
           05  DF-WAREHOUSE-TOTAL      PIC 9(8)V99.
           05  DF-GRAND-TOTAL          PIC 9(9)V99.

       01  VENDOR-TEXT.
           05          PIC X(13)       VALUE 'IMADEINHOUSE'.
           05          PIC X(13)       VALUE 'TTANSIA CORP.'.
           05          PIC X(13)       VALUE 'AAMEL LTD.'.
           05          PIC X(13)       VALUE 'WWEST CORP.'.
           05          PIC X(13)       VALUE 'DDENIO CORP.'.
           05          PIC X(13)       VALUE 'VVISSON CORP.'.
           05          PIC X(13)       VALUE 'NNETON LTD.'.

       01  VENDOR-SEARCH-TABLE REDEFINES VENDOR-TEXT.
           05 VENDOR-ITEM OCCURS 7 TIMES
              INDEXED BY VT-INDEX.
              10  VST-VENDORID         PIC X.
              10  VST-VENDOR-NAME      PIC X(12).
      *
      *********************** OUTPUT-AREA ****************************
      *
       01  REPORT-TITLE-ONE.
           05               PIC X(35)    VALUE SPACES.
           05               PIC X(35)    VALUE 'DR. CHEEB'.

       01  REPORT-TITLE-TWO.
           05                   PIC X(10)    VALUE SPACES.
           05  H1-MONTH         PIC 99.
           05                   PIC X        VALUE '/'.
           05  H1-DAY           PIC 99.
           05                   PIC X        VALUE '/'.
           05  H1-YEAR          PIC 9(4).
           05                   PIC X(12)    VALUE SPACES.
           05                   PIC X(24)    VALUE 'INVENTORY REPORT'.
           05                   PIC X(6)     VALUE 'PAGE: '.
           05  RT2-PAGE-NUM     PIC 99.

       01  WAREHOUSE-GROUP-HEADER.
           05                   PIC X(13)    VALUE '  WAREHOUSE: '.
           05  WGH-WAREHOUSEID  PIC X(4).
           05                   PIC X(53)    VALUE SPACES.

       01  VENDOR-GROUP-HEADER.
           05                   PIC X(13)    VALUE '     VENDOR:'.
           05  VGH-VENDOR-NAME  PIC X(12).
           05                   PIC X(45)    VALUE SPACES.

       01  COLUMN-HEADER-ONE.
           05                   PIC X(7)     VALUE SPACES.
           05                   PIC X(14)    VALUE 'PRODUCT'.
           05                   PIC X(8)     VALUE 'PROD'.
           05                   PIC X(11)    VALUE 'PRODUCT'.
           05                   PIC X(8)     VALUE 'PROD'.
           05                   PIC X(9)     VALUE 'IN'.
           05                   PIC X(7)     VALUE 'TOTAL'.

       01  COLUMN-HEADER-TWO.
           05                   PIC X(9)    VALUE SPACES.
           05                   PIC X(13)    VALUE 'NAME'.
           05                   PIC X(8)     VALUE 'ID'.
           05                   PIC X(10)    VALUE 'SIZE'.
           05                   PIC X(7)     VALUE 'TYPE'.
           05                   PIC X(10)    VALUE 'STOCK'.
           05                   PIC X(7)     VALUE 'COST'.

       01  DETAIL-LINE.
           05                   PIC X(5)     VALUE SPACES.
           05  DL-PRODUCT-NAME  PIC X(17).
           05  DL-PRODUCT-ID    PIC X(5).
           05  DL-PRODUCT-SIZE  PIC X(13).
           05  DL-PRODUCT-TYPE  PIC X(8).
           05  DL-IN-STOCK      PIC Z999.
           05                   PIC X(3)     VALUE SPACES.
           05  DL-TOTAL-COST    PIC $$$,$$$.99.

       01  PRODUCT-TOTAL-LINE.
           05                   PIC X(21)    VALUE SPACES.
           05                   PIC X(9)     VALUE 'PRODUCT: '.
           05  PTL-PRODUCT-NAME PIC X(13).
           05                   PIC X(10)    VALUE ' TOTAL:'.
           05  PTL-PROD-TOTAL   PIC $,$$$,$$$.99.

       01  VENDOR-TOTAL-LINE.
           05                   PIC X(13)    VALUE SPACES.
           05                   PIC X(10)    VALUE 'TOTAL FOR'.
           05                   PIC X(8)     VALUE 'VENDOR: '.
           05  VTL-VENDOR-NAME  PIC X(12).
           05                   PIC X(10)    VALUE SPACES.
           05  VTL-VENDOR-TOTAL PIC $,$$$,$$$.99.

       01  WAREHOUSE-TOTAL-LINE.
           05                   PIC X(10)     VALUE SPACES.
           05                   PIC X(10)     VALUE 'TOTAL FOR '.
           05                   PIC X(10)     VALUE 'WAREHOUSE:'.
           05  WTL-WAREHOUSEID  PIC X(4).
           05                   PIC X(17)     VALUE SPACES.
           05  WTL-WAREH-TOTAL  PIC $$,$$$,$$$.99.

       01  GRAND-TOTAL-LINE.
           05                   PIC X(22)    VALUE SPACES.
           05                   PIC X(17)    VALUE 'GRAND TOTAL COST:'.
           05                   PIC X(12)    VALUE SPACES.
           05  GTL-GRAND-TOTAL  PIC $$$,$$$,$$$.99.
      *
      ********************** PROCEDURE-DIVISION **********************
      *
       PROCEDURE DIVISION.

       100-MAIN-MODULE.
          
           PERFORM 150-SORTMERGE-INPUT-FILES
           PERFORM 200-HOUSEKEEPIG-ROUTINE
           PERFORM 250-READ-INVENTORY-FILE
           PERFORM 1000-FINAL-ROUTINE

           .

       150-SORTMERGE-INPUT-FILES.
        
           SORT SORTMERGE-FILE
                ON ASCENDING KEY SM-WAREHOUSEID
                ON ASCENDING KEY SM-VENDORID
                ON ASCENDING KEY SM-PRODUCTID
                USING UNSORTED-INPUTFILE-NV10
                GIVING SORTED-OUTPUTFILE-NV10

           SORT SORTMERGE-FILE
                ON ASCENDING KEY SM-WAREHOUSEID
                ON ASCENDING KEY SM-VENDORID
                ON ASCENDING KEY SM-PRODUCTID
                USING UNSORTED-INPUTFILE-CA20
                GIVING SORTED-OUTPUTFILE-CA20

           SORT SORTMERGE-FILE
                ON ASCENDING KEY SM-WAREHOUSEID
                ON ASCENDING KEY SM-VENDORID
                ON ASCENDING KEY SM-PRODUCTID
                USING UNSORTED-INPUTFILE-WA30
                GIVING SORTED-OUTPUTFILE-WA30

           MERGE SORTMERGE-FILE
                 ON ASCENDING KEY SM-WAREHOUSEID
                 ON ASCENDING KEY SM-VENDORID
                 ON ASCENDING KEY SM-PRODUCTID
           USING SORTED-OUTPUTFILE-NV10, 
		         SORTED-OUTPUTFILE-CA20,
                 SORTED-OUTPUTFILE-WA30
           GIVING MERGED-PRODUCT-FILE

           .
		   
       200-HOUSEKEEPIG-ROUTINE.
           
           OPEN INPUT  MERGED-PRODUCT-FILE
                OUTPUT ERROR-FILE
                OUTPUT PRODUCT-SUMMARY-REPORT

           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY   TO H1-DAY
           MOVE WS-YEAR  TO H1-YEAR

           .

       250-READ-INVENTORY-FILE.

           PERFORM UNTIL MORE-RECORDS = 'NO'    
               READ MERGED-PRODUCT-FILE
                   AT END  
                       MOVE 'NO' TO MORE-RECORDS
                   NOT AT END
                       EVALUATE MPR-WAREHOUSEID

                           WHEN 'NV10'
                               PERFORM 600-PROCESS-ROUTINE   
                           WHEN 'CA20'
                               PERFORM 600-PROCESS-ROUTINE 
                           WHEN 'WA30'
                               PERFORM 600-PROCESS-ROUTINE 
                           WHEN OTHER
                               MOVE MERGED-PRODUCT-RECORD 
                               TO ERROR-FILE-RECORD
                               ADD 1 TO INCORRECT-RECORD-COUNT
                               WRITE ERROR-FILE-RECORD
                               AFTER ADVANCING 1 LINE

                       END-EVALUATE                         
               END-READ    
           END-PERFORM
           
           .

       300-WRITE-A-LINE.

           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING

           .

       350-REPORT-HEADER-ROUTINE.
           
           ADD 1 TO PAGE-NUM
		   MOVE PAGE-NUM TO RT2-PAGE-NUM

           MOVE REPORT-TITLE-ONE TO REPORT-LINE
		   PERFORM 300-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING

           MOVE REPORT-TITLE-TWO TO REPORT-LINE
           PERFORM 300-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING

           .

       400-PRINT-WAREHOUSE-HEADER.

           EVALUATE  TRUE

               WHEN MPR-WAREHOUSEID = 'NV10'
                   MOVE MPR-WAREHOUSEID TO WGH-WAREHOUSEID      
                   MOVE WAREHOUSE-GROUP-HEADER TO REPORT-LINE
                   PERFORM 300-WRITE-A-LINE
                   MOVE 2 TO PROPER-SPACING
           
               WHEN MPR-WAREHOUSEID = 'CA20'
                   MOVE MPR-WAREHOUSEID TO WGH-WAREHOUSEID       
                   MOVE WAREHOUSE-GROUP-HEADER TO REPORT-LINE
                   PERFORM 300-WRITE-A-LINE
                   MOVE 2 TO PROPER-SPACING
           
               WHEN MPR-WAREHOUSEID = 'WA30'
                   MOVE MPR-WAREHOUSEID TO WGH-WAREHOUSEID 
                   MOVE WAREHOUSE-GROUP-HEADER TO REPORT-LINE
                   PERFORM 300-WRITE-A-LINE
                   MOVE 2 TO PROPER-SPACING

           END-EVALUATE
    
           .

       450-PRINT-VENDOR-HEADER.
           
           SET VT-INDEX TO 1

           SEARCH VENDOR-ITEM
               AT END
                   STRING 
                       'INVALID-' DELIMITED BY SIZE
                       MPR-VENDORID DELIMITED BY SIZE
                       INTO DF-VENDOR-NAME-ERROR
                   END-STRING
                   MOVE DF-VENDOR-NAME-ERROR TO DF-VENDOR-NAME 
				   MOVE SPACES TO DF-VENDOR-NAME-ERROR
                   MOVE DF-VENDOR-NAME TO VGH-VENDOR-NAME
				   
               WHEN MPR-VENDORID = VST-VENDORID(VT-INDEX)
                   MOVE VST-VENDOR-NAME(VT-INDEX) TO DF-VENDOR-NAME
                   MOVE DF-VENDOR-NAME TO VGH-VENDOR-NAME
           END-SEARCH

		   MOVE VENDOR-GROUP-HEADER TO REPORT-LINE
           PERFORM 300-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
           MOVE SPACES TO DF-VENDOR-NAME-ERROR

           .

       500-PRINT-COLUMN-HEADER.

           MOVE COLUMN-HEADER-ONE TO REPORT-LINE
           PERFORM 300-WRITE-A-LINE 

           MOVE 1 TO PROPER-SPACING
           MOVE COLUMN-HEADER-TWO TO REPORT-LINE
           PERFORM 300-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING

           MOVE 'YES' TO FIRST-PRODUCT

           .

       600-PROCESS-ROUTINE.

           EVALUATE TRUE  
              
               WHEN FIRST-RECORD = 'YES'
                   MOVE MPR-WAREHOUSEID TO WS-WAREHOUSE-HOLD
                   MOVE MPR-VENDORID    TO WS-VENDOR-HOLD
                   MOVE MPR-VENDORID    TO WS-VENDOR-HOLD
                   MOVE MPR-PRODUCTID   TO WS-PRODUCT-HOLD
                   MOVE 'NO' TO FIRST-RECORD
                   PERFORM 350-REPORT-HEADER-ROUTINE
                   PERFORM 400-PRINT-WAREHOUSE-HEADER
                   PERFORM 450-PRINT-VENDOR-HEADER
                   PERFORM 500-PRINT-COLUMN-HEADER
                
               WHEN WS-WAREHOUSE-HOLD NOT = MPR-WAREHOUSEID 
                   PERFORM 700-WAREHOUSE-CONTROL-BREAK
				   MOVE 3 TO PROPER-SPACING
                   PERFORM 400-PRINT-WAREHOUSE-HEADER
                   PERFORM 450-PRINT-VENDOR-HEADER
                   PERFORM 500-PRINT-COLUMN-HEADER

               WHEN WS-VENDOR-HOLD NOT = MPR-VENDORID
                   PERFORM 750-VENDOR-CONTROL-BREAK
                   PERFORM 450-PRINT-VENDOR-HEADER
                   PERFORM 500-PRINT-COLUMN-HEADER
                
               WHEN WS-PRODUCT-HOLD NOT = MPR-PRODUCTID
                   PERFORM 800-PRODUCT-CONTROL-BREAK
                   PERFORM 500-PRINT-COLUMN-HEADER

           END-EVALUATE

           PERFORM 900-TRAVERSE-PRODUCT-ARRAY
                   VARYING PRODUCT-SUB FROM 1 BY 1
                      UNTIL PRODUCT-SUB > 5

           .

       900-TRAVERSE-PRODUCT-ARRAY.
           
           IF MPR-PRODUCT-NAME(PRODUCT-SUB) = SPACES
               CONTINUE
           ELSE

               EVALUATE TRUE
                   WHEN FIRST-PRODUCT = 'YES'
                       MOVE 'NO' TO FIRST-PRODUCT
                       MOVE MPR-PRODUCT-NAME(PRODUCT-SUB) 
                       TO WS-PRODUCT-NAME
                       MOVE WS-PRODUCT-NAME TO DL-PRODUCT-NAME
                   WHEN FIRST-PRODUCT = 'NO'
                       MOVE ' ' TO DL-PRODUCT-NAME
               END-EVALUATE


               MOVE MPR-PRODUCTID TO DL-PRODUCT-ID

               EVALUATE TRUE

                   WHEN MPR-PRODUCT-SIZE(PRODUCT-SUB) = 'X'
                       MOVE 'EXTRA LARGE' TO DL-PRODUCT-SIZE
                   WHEN MPR-PRODUCT-SIZE(PRODUCT-SUB) = 'L'
                       MOVE 'LARGE' TO DL-PRODUCT-SIZE
                   WHEN MPR-PRODUCT-SIZE(PRODUCT-SUB) = 'M'
                       MOVE 'MEDIUM' TO DL-PRODUCT-SIZE
                   WHEN MPR-PRODUCT-SIZE(PRODUCT-SUB) = 'S'
                       MOVE 'SMALL' TO DL-PRODUCT-SIZE
                   WHEN MPR-PRODUCT-SIZE(PRODUCT-SUB) = 'A'
                       MOVE 'SAMPLE' TO DL-PRODUCT-SIZE
                   WHEN OTHER
                       STRING
                           'BAD-' DELIMITED BY SIZE
                           MPR-PRODUCT-SIZE(PRODUCT-SUB) 
                               DELIMITED BY SIZE
                           INTO WS-INVALID-PRODUCT-SIZE
                       END-STRING
                       MOVE WS-INVALID-PRODUCT-SIZE TO DL-PRODUCT-SIZE

               END-EVALUATE

               EVALUATE TRUE

                   WHEN MPR-PRODUCT-TYPE(PRODUCT-SUB) = 'C'
                       MOVE 'CREAM' TO DL-PRODUCT-TYPE
                   WHEN MPR-PRODUCT-TYPE(PRODUCT-SUB) = 'O'
                       MOVE 'OIL'   TO DL-PRODUCT-TYPE
                   WHEN OTHER
                       STRING
                           'BAD-' DELIMITED BY SIZE
                           MPR-PRODUCT-TYPE(PRODUCT-SUB) 
                               DELIMITED BY SIZE
                           INTO WS-INVALID-PRODUCT-TYPE
                       END-STRING
                       MOVE WS-INVALID-PRODUCT-TYPE TO DL-PRODUCT-TYPE

               END-EVALUATE

               MOVE SPACES TO WS-INVALID-PRODUCT-SIZE
               MOVE SPACES TO WS-INVALID-PRODUCT-TYPE

               IF MPR-QUANTITY-INSTOCK(PRODUCT-SUB) IS NUMERIC
                   MOVE MPR-QUANTITY-INSTOCK(PRODUCT-SUB)
                        TO WS-STOCK-QUANTITY
               ELSE 
                   MOVE ZEROS TO WS-STOCK-QUANTITY
               END-IF

               MOVE WS-STOCK-QUANTITY TO DL-IN-STOCK

               IF MPR-PURCHASE-PRICE(PRODUCT-SUB) IS NUMERIC
			       MOVE MPR-PURCHASE-PRICE(PRODUCT-SUB)
				   TO WS-TOTAL-COST
                   MULTIPLY WS-STOCK-QUANTITY BY WS-TOTAL-COST
               ELSE 
                   MOVE ZEROS TO WS-TOTAL-COST
               END-IF

               MOVE WS-TOTAL-COST TO DL-TOTAL-COST
               ADD  WS-TOTAL-COST TO DF-PRODUCT-TOTAL
               ADD  WS-TOTAL-COST TO DF-VENDOR-TOTAL
               ADD  WS-TOTAL-COST TO DF-WAREHOUSE-TOTAL
               ADD  WS-TOTAL-COST TO DF-GRAND-TOTAL

               MOVE 0 TO WS-TOTAL-COST
               MOVE DETAIL-LINE TO REPORT-LINE
               PERFORM 300-WRITE-A-LINE
               MOVE 1 TO PROPER-SPACING

           END-IF

           .

       700-WAREHOUSE-CONTROL-BREAK.
       
           PERFORM 750-VENDOR-CONTROL-BREAK
           MOVE WS-WAREHOUSE-HOLD  TO WTL-WAREHOUSEID
           MOVE DF-WAREHOUSE-TOTAL TO WTL-WAREH-TOTAL

           MOVE WAREHOUSE-TOTAL-LINE TO REPORT-LINE
           MOVE 2 TO PROPER-SPACING
           PERFORM 300-WRITE-A-LINE

           MOVE MPR-WAREHOUSEID   TO WS-WAREHOUSE-HOLD
           MOVE 0 TO DF-WAREHOUSE-TOTAL

           .

       750-VENDOR-CONTROL-BREAK.

           PERFORM 800-PRODUCT-CONTROL-BREAK
           MOVE DF-VENDOR-NAME TO VTL-VENDOR-NAME
           MOVE DF-VENDOR-TOTAL TO VTL-VENDOR-TOTAL

           MOVE VENDOR-TOTAL-LINE TO REPORT-LINE
           MOVE 3 TO PROPER-SPACING
           PERFORM 300-WRITE-A-LINE

           MOVE MPR-VENDORID TO WS-VENDOR-HOLD
           MOVE 0 TO DF-VENDOR-TOTAL

           .

       800-PRODUCT-CONTROL-BREAK.

           MOVE WS-PRODUCT-NAME  TO PTL-PRODUCT-NAME
           MOVE DF-PRODUCT-TOTAL TO PTL-PROD-TOTAL

           MOVE PRODUCT-TOTAL-LINE TO REPORT-LINE
           MOVE 2 TO PROPER-SPACING
           PERFORM 300-WRITE-A-LINE
		   MOVE 3 TO PROPER-SPACING

           MOVE MPR-PRODUCTID TO WS-PRODUCT-HOLD
           MOVE 0 TO DF-PRODUCT-TOTAL

           .

       1000-FINAL-ROUTINE.

           PERFORM 700-WAREHOUSE-CONTROL-BREAK

           MOVE DF-GRAND-TOTAL TO GTL-GRAND-TOTAL
           MOVE GRAND-TOTAL-LINE TO REPORT-LINE
           MOVE 2 TO PROPER-SPACING
           PERFORM 300-WRITE-A-LINE
		   
		   STRING 
		       'THERE ARE ' DELIMITED BY SIZE
			   INCORRECT-RECORD-COUNT DELIMITED 
			   BY SIZE
			   ' INVALID RECORDS ALLTOGETHER WHICH ARE' 
			   DELIMITED BY SIZE
			   ' WRRITEN IN THE ERROR FILE.'
			   DELIMITED BY SIZE
			   INTO INVALID-RECORD-MESSAGE
		   END-STRING
		   
		   DISPLAY INVALID-RECORD-MESSAGE
		   
		   CLOSE  MERGED-PRODUCT-FILE
                  PRODUCT-SUMMARY-REPORT
                  ERROR-FILE
				  UNSORTED-INPUTFILE-NV10
                  UNSORTED-INPUTFILE-CA20
                  UNSORTED-INPUTFILE-WA30
                  SORTED-OUTPUTFILE-NV10
                  SORTED-OUTPUTFILE-CA20
                  SORTED-OUTPUTFILE-WA30

           STOP RUN 

           .
