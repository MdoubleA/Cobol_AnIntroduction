      ******************************************************************
      * Author: MIchael Alaniz
      * Date: 09/25/2020
      * Purpose: Create the test data needed to complete Chapter 7
      *    Programming Exercise 1.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAKE-TEST-DATA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ProductFile ASSIGN TO "Product.dat"
               ORGANIZATION IS LINE SEQUENTIAL. *> Text editor readable.

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
       01  RecordCounter   PIC 99  VALUE   01.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           *> Get the records from the user.
           *> First get first half of record, it's identifying features.
           *> Then get second half of record, qty and cost of 1 object.
            OPEN OUTPUT ProductFile
               PERFORM GetProductIDFeatures *> Init loop.
               PERFORM UNTIL IDFeatures = SPACES *> Set stop condition.
                   PERFORM GetProductPricingFeatures *> Get 2nd half.
                   WRITE ProductRecord *> Send to file.
                   PERFORM GetProductIDFeatures *> Loop.

               END-PERFORM
            CLOSE ProductFile. *> File clean up.

           *> Print the entered file for visual validation.
            OPEN INPUT ProductFile
              PERFORM DisplayAnEntry *> Init loop.
              PERFORM DisplayAnEntry UNTIL ProductFileEnd *> Loop.
            CLOSE ProductFile *> File clean up.

            STOP RUN. *> End PROGRAM.***********************************

       DisplayAnEntry.
           READ ProductFile AT END SET ProductFileEnd TO TRUE END-READ
           DISPLAY ProductRecord.

       GetProductIDFeatures.
           DISPLAY "Record: ", RecordCounter
           ADD 1 TO RecordCounter GIVING RecordCounter
           DISPLAY "Enter product id and name: 123456Bob The Builder. ",
                   "Enter spaces to terminate."
           ACCEPT IDFeatures.

       GetProductPricingFeatures.
           DISPLAY "Enter 10 digit pricing information."
           ACCEPT PotentialProfitFeatures
           DISPLAY " "
           DISPLAY " ".

       END PROGRAM MAKE-TEST-DATA.
