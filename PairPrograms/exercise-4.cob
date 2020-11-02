       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCISE-4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO
           "./PairPrograms/SortedCourses.prn"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "Report.txt".
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
         01 INPUT-RECORD.
           03 DEPARTMENT-CODE   PIC X(4).
           03 FILLER            PIC X(17).
           03 COURSE-TITLE      PIC X(36).
           03 FILLER            PIC X(88).
           03 ENROLLMENT        PIC X(4).
       FD REPORT-FILE
           REPORT IS COURSE-REPORT.
       WORKING-STORAGE SECTION.
           77 FILE-STATUS       PIC 99 VALUE 0.
              88 EndOfFile      VALUE 1.
       REPORT SECTION.
       RD COURSE-REPORT
           CONTROLS ARE COURSE-TITLE
           PAGE LIMIT IS 50 LINES.
       01 TYPE IS REPORT HEADING.
           02 FIRST-LINE LINE PLUS 1.
             05 COLUMN 4 PIC X(19) VALUE "DEPARTMENT FINANCES".

       01 REPORT-LINE TYPE IS DETAIL LINE PLUS 1.
         *>02 REPORT-LINE LINE PLUS 1.
           03 COLUMN 4 PIC X(30) SOURCE COURSE-TITLE.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT REPORT-FILE.
           INITIATE COURSE-REPORT.
           READ INPUT-FILE AT END MOVE 1 TO FILE-STATUS.
           PERFORM  GENERATE-REPORT UNTIL EndOfFile.
           TERMINATE COURSE-REPORT.
           CLOSE REPORT-FILE.
           CLOSE INPUT-FILE.
           STOP RUN.

       GENERATE-REPORT.
           GENERATE REPORT-LINE.
           READ INPUT-FILE AT END MOVE 1 TO FILE-STATUS.

       END PROGRAM EXERCISE-4.
