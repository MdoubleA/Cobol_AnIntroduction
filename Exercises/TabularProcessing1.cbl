      ******************************************************************
      * Author: Michael Alaniz
      * Date: 10/23/2020
      * Purpose: Learn to use COBOL tables in calculations.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Exercise11-7.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CoursesFile ASSIGN TO "Courses.prn" ORGANIZATION IS
             LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD CoursesFile.
       01  aRecord PIC X(140).
         88  CoursesEOF VALUE HIGH-VALUE.


       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN INPUT CoursesFile.
              PERFORM UNTIL CoursesEOF
                PERFORM ReadLine
                DISPLAY aRecord
              END-PERFORM.
            CLOSE CoursesFile.



            STOP RUN.

       ReadLine.
           READ CoursesFile AT END SET CoursesEOF TO TRUE
           END-READ.

       END PROGRAM Exercise11-7.
