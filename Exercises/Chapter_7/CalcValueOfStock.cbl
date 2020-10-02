      ******************************************************************
      * Author: Michael Alaniz
      * Date: 10/01/2020
      * Purpose: Experiment with reading from file and calculate.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHAP-7-PROG-1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ProductFile ASSIGN TO "Product.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ProductFile.
       01  ProductRecord.
           88  ProductFileEnd  VALUE HIGH-VALUES.
           *> Contain features for record identification.
           02  IDFeatures.
               03  GadgetID        PIC 9(6).
               03  GadgetName      PIC X(30).

           *> Contain features for determining stock, price, and
           *> potential profit (Qty * price of 1).
           02  PotentialProfitFeatures.
               03  QtyInStock      PIC 9(4).
               03  Price           PIC 9(4)V99.

       WORKING-STORAGE SECTION.
           01 AnInStockValue         PIC 9(10)V99.
           01 TotalInStockValue      PIC 9(10)V99    VALUE ZERO.
           01 PrintAValue            PIC $,$$$,$$$,$$$.99.
           01 PrintTotalValue        PIC $,$$$,$$$,$$$.99    VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT ProductFile
               READ ProductFile AT END SET ProductFileEnd TO TRUE
               END-READ

           PERFORM UNTIL ProductFileEnd
                   MULTIPLY QtyInStock BY Price GIVING AnInStockValue
                   ADD AnInStockValue TO TotalInStockValue
                   MOVE AnInStockValue to PrintAValue

                   DISPLAY GadgetID, ":", SPACE, PrintAValue, "."
                   READ ProductFile AT END SET ProductFileEnd TO TRUE
                   END-READ

           END-PERFORM
           CLOSE ProductFile

           MOVE TotalInStockValue TO PrintTotalValue
           DISPLAY "Stock Total: ", PrintTotalValue, "."

       STOP RUN.

       END PROGRAM CHAP-7-PROG-1.
