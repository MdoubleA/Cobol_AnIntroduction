      ******************************************************************
      * Author: Michael Alaniz.
      * Date: 09/19/2020.
      * Purpose: Education.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Chapter5Calculator_FirstProg.
       *> Excepts two numbers and an operation.
       *> Applies operation to numbers.
       *> Allowed operations are -, *, /, and +.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 Num1     PIC 9 VALUE 0.
       01 Num2     PIC 9 VALUE 0.
       01 Result   PIC S99V99 VALUE 0.

       01 ValidOperator    PIC X.
           88 Mult         VALUE '*'.
           88 Divi         VALUE '/'.
           88 Subt         VALUE '-'.
           88 Addi         VALUE '+'.

       01 PrintResult      PIC A(5) VALUE "true".

       PROCEDURE DIVISION.
       CalculateResult.
            *> WITH NO ADVANCING seems to not be working in this set up.
            DISPLAY "Enter an operator: "
            ACCEPT ValidOperator

            IF ValidOperator = 's'
                GO TO ExitCalculator
            END-IF

            DISPLAY "Enter a single digit: "
            ACCEPT Num1

            DISPLAY "Another single digit: "
            ACCEPT Num2

            EVALUATE TRUE
               WHEN Mult MULTIPLY  Num1 BY   Num2 GIVING Result
               WHEN Divi DIVIDE    Num1 BY   Num2 GIVING Result ROUNDED
               WHEN Subt SUBTRACT  Num1 FROM Num2 GIVING Result
               WHEN Addi ADD       Num1 TO   Num2 GIVING Result
               WHEN OTHER          MOVE "false" TO PrintResult
            END-EVALUATE

            IF PrintResult = 'true'
                DISPLAY "Result = ", Result
            ELSE
                DISPLAY "Bad operator, try another one."
            END-IF

            GO TO CalculateResult.

       ExitCalculator.
           EXIT.

       END PROGRAM Chapter5Calculator_FirstProg.
