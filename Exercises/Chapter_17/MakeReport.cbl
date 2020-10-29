      ******************************************************************
      * Author: Michael Alaniz
      * Date: 10/27/2020
      * Purpose: Create a report on the tuition income broken down by
      *          department, professor, and grad/undergrad level.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TuitionReport.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CoursesFile
             ASSIGN TO "./Exercises/Chapter_17/CoursesProcessed.prn"
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ReportFile
             ASSIGN TO "./Exercises/Chapter_17/Report.txt"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CoursesFile.
       01 CourseFileRecord.
           88 EOCoursesFile VALUE HIGH-VALUES.
           02 CourseSubject PIC X(4).
           02 FILLER        PIC X(2).
           02 CourseNumber  PIC X(3).
           02 FILLER        PIC X(64).
           02 LastName      PIC X(16).
           02 FirstName     PIC X(16).
           02 FILLER        PIC X(32).
           02 Esch          PIC X(3).

       FD ReportFile REPORT IS TuitionReport.

       WORKING-STORAGE SECTION.
       01 FileStatus PIC 9 VALUE 0.
         88 EndOfFile VALUE 1.

       01 FirstRead PIC 9 VALUE 1.

       *> For accumulating department total.
       01 PreviousSubject  PIC X(4)  VALUE SPACES.
       01 DepartmentAccumulator PIC 9(9) VALUE ZERO.

       *> For accumulating professor total.
       01 PreviousLastName PIC X(16) VALUE SPACES.
       01 ProfessorAccumulator PIC 9(9) VALUE ZERO.

       *> For converting Esch from ALPHANUMERIC to NUMERIC.
       01 NumericEschField PIC 999.

       REPORT SECTION.
       RD TuitionReport
         CONTROLS ARE CourseSubject, LastName
           PAGE LIMIT IS 50 LINES.

       01 TYPE IS REPORT HEADING.
         02 Header LINE PLUS 1.
           *> WorkCourseSubject, WorkLastName, WorkCourseNumber
           03 COLUMN 4  PIC X(10) VALUE "DEPARTMENT".
           03 COLUMN 30 PIC X(10) VALUE "PROFESSOR".
           03 COLUMN 55 PIC X(12) VALUE "COURSE LEVEL".

       01 TYPE IS DETAIL LINE PLUS 1.
         02 ReportLine LINE PLUS 1.
           03 COLUMN 4  PIC X(10) SOURCE CourseSubject GROUP INDICATE.
           03 COLUMN 30 PIC X(10) SOURCE LastName      GROUP INDICATE.
           03 COLUMN 55 PIC X(10) SOURCE CourseNumber.

       01 DepartmentSummary TYPE IS CONTROL FOOTING CourseSubject
         NEXT GROUP PLUS 2.
         02 LINE PLUS 1.
           03 COLUMN 55 PIC X(25) VALUE "DEPARTMENT CONTROL CHANGE".

       01 ProfessorSummary TYPE IS CONTROL FOOTING LastName
         NEXT GROUP PLUS 2.
         02 LINE PLUS 1.
           03 COLUMN 55 PIC X(25) VALUE "PROFESSOR CONTROL CHANGE".

       01 Footer TYPE IS REPORT FOOTING.
         02 LINE IS PLUS 1.
           03 COLUMN 30 PIC X(20) VALUE "==== END REPORT ====".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN INPUT CoursesFile.
            OPEN OUTPUT ReportFile.

              INITIATE TuitionReport.
              *>PERFORM ProcessRecord.
              PERFORM ReadLine.
              PERFORM GenerateReport UNTIL EndOfFile.
              TERMINATE TuitionReport.

            CLOSE CoursesFile.
            CLOSE ReportFile.

      *>       OPEN INPUT CoursesFile.
      *>         PERFORM ProcessRecord UNTIL EndOfFile.
      *>       CLOSE CoursesFile.

            STOP RUN.

       GenerateReport.
           GENERATE ReportLine.
           PERFORM ReadLine.

       ReadLine.
           READ CoursesFile AT END SET EndOfFile TO TRUE.
           PERFORM ProcessRecord.

           *>PERFORM ProcessRecord.

       ProcessRecord.
           IF CourseSubject NOT EQUAL TO PreviousSubject AND
               FirstRead EQUAL TO 0 *> 0 as false like C.
               DISPLAY "Control Change" END-IF.
           MOVE CourseSubject TO PreviousSubject.
           MOVE 0 TO FirstRead.
           MOVE Esch TO NumericEschField.
           ADD NumericEschField TO ProfessorAccumulator.


      *>  ProcessRecord.
      *>      READ CoursesFile AT END MOVE 1 TO FileStatus.
      *>      IF FirstRead EQUAL TO 1
      *>      *>DISPLAY "CONTROL break ", CourseSubject.
      *>      IF FileStatus NOT EQUAL 1
      *>        *> Determine if control break.
      *>        IF PreviousSubject NOT EQUAL TO CourseSubject AND
      *>            FirstRead NOT EQUAL TO 1
      *>            DISPLAY "CONTROL break ", CourseSubject
      *>      END-IF

      *>      MOVE CourseSubject TO PreviousSubject.
      *>      MOVE 0 TO FirstRead.

       END PROGRAM TuitionReport.
