      ******************************************************************
      * Author: Michael & Daniel & Trevor
      * Date: 10/22/2020
      * Purpose: Learn tables.
      * Tectonics: cobc
      * Exercise: Pair Programming 3
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PP3.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Courses ASSIGN TO "Courses.prn"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD Courses.
       01  CourseLine.
           02 CourseSubject PIC X(4).

       WORKING-STORAGE SECTION.
       *>       01 CoursesTable.
       *>           02 Subject PIC X(4) OCCURS 12 TIMES INDEXED BY IDX.
       *>           02 SubjectCount PIC 9(2) OCCURS 12 TIMES VALUES ZERO.

       01 CourseTable.
           02 SubjectList.
               03 FILLER PIC X(4) VALUE "CE".
               03 FILLER PIC X(4) VALUE "CS".
               03 FILLER PIC X(4) VALUE "EAS".
               03 FILLER PIC X(4) VALUE "ECE".
               03 FILLER PIC X(4) VALUE "EE".
               03 FILLER PIC X(4) VALUE "ENVE".
               03 FILLER PIC X(4) VALUE "ESM".
               03 FILLER PIC X(4) VALUE "ETM".
               03 FILLER PIC X(4) VALUE "ME".
               03 FILLER PIC X(4) VALUE "MSE".
               03 FILLER PIC X(4) VALUE "PH".
               03 FILLER PIC X(4) VALUE "SYSE".
               03 FILLER PIC X(4) VALUE "USP".
           02 FILLER REDEFINES SubjectList.
               03 SubjectTable OCCURS 13 TIMES INDEXED BY IDX PIC X(4) .
           02 SubjectCount PIC 9(2) OCCURS 13 TIMES VALUES ZERO.

       01  EndCoursesFile PIC 99.
           88 EOF VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN INPUT Courses
            PERFORM READ-LINE.
            PERFORM READ-LINE UNTIL EOF

            CLOSE Courses
            STOP RUN.

       READ-LINE.
           READ Courses
             AT END MOVE 1 TO EndCoursesFile
           END-READ

           SEARCH SubjectName VARYING IDX
             AT END DISPLAY 'DONE'
             WHEN SubjectName(IDX) EQUAL CourseSubject
             ADD 1 TO SubjectCount(IDX)
           END-SEARCH.

       END PROGRAM PP3.
