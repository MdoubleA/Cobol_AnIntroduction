      ******************************************************************
      * Author: Daniel and Michael
      * Date: 10/15/2020
      * Purpose: Learn to pair program
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PP2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Courses ASSIGN TO "C:\Users\Michael\bin\Courses.prn"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD Courses.
       01  CourseLine.
           02 CourseSubject PIC X(4).
           02 FILLER        PIC X(2).
           02 CourseNumber  PIC X(1).
           02 FILLER        PIC X(130).
           02 Esch          PIC X(3).

       WORKING-STORAGE SECTION.
       77  EndOfCoursesFile PIC 99 VALUE ZERO.
           88 eof VALUE 1.

       01  GradTuition  PIC 999V99 VALUE 496.50.
       01  UGradTuition PIC 999V99 VALUE 238.85.

       01  CourseTotalCosts.
           02 CE PIC


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN INPUT Courses
            PERFORM ReadLine
            PERFORM ReadLine

            DISPLAY CourseLine
            DISPLAY GradTuition, "  ", UGradTuition
            CLOSE Courses
            STOP RUN.

       ReadLine.
           READ Courses
               AT END MOVE 1 TO EndOfCoursesFile
           END-READ.

       END PROGRAM PP2.
