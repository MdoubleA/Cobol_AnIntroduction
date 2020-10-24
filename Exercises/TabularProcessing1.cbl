      ******************************************************************
      * Author: Michael Alaniz
      * Date: 10/23/2020
      * Purpose: Determine section count by department.
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
       01  aRecord.
         88  CoursesEOF VALUE HIGH-VALUE.
         02  SubjectCode PIC X(4).
         02  FILLER       PIC X(136).

       WORKING-STORAGE SECTION.
       01  DeptTable.
         02 Dept OCCURS 13 TIMES INDEXED BY DeptIdx.
           03 DeptName PIC X(4).
           03 DeptCount PIC 9(4).

       01  DefaultTableValue PIC X(104) VALUES
         "CS  0000CE  0000EAS 0000ECE 0000EE  0000ENVE0000ETM 0000ME  00
      -  "00MSE 0000SYSE0000USP 0000PH  0000ESM 0000".

       01  NumDepartments PIC 99 VALUE 13.

       01  Header.
         02 FILLER PIC X(10) VALUE "DEPARTMENT".
         02 FILLER PIC X(5) VALUE SPACES.
         02 FILLER PIC X(13) VALUE "SECTION COUNT".
         02 FormattingDivider PIC X(11) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE DefaultTableValue TO DeptTable.

            OPEN INPUT CoursesFile.
            PERFORM ReadLine. *> Remove header from file.
            PERFORM ReadLine.

              PERFORM UNTIL CoursesEOF
                PERFORM ReadLine
                PERFORM IncrementTablePosition
              END-PERFORM.
            CLOSE CoursesFile.

            PERFORM PrintReport.

            STOP RUN.

       ReadLine.
           READ CoursesFile AT END SET CoursesEOF TO TRUE
           END-READ.

       IncrementTablePosition.
           SET DeptIdx TO 1
           SEARCH Dept
             AT END PERFORM PrintMissedSubject
             WHEN DeptName(DeptIdx) = SubjectCode
               ADD 1 TO DeptCount(DeptIdx)
           END-SEARCH.

       PrintMissedSubject.
           DISPLAY SubjectCode.

       PrintReport.
           DISPLAY Header.
           PERFORM
             VARYING DeptIdx FROM 1 BY 1
             UNTIL DeptIdx = NumDepartments
               DISPLAY DeptName(DeptIdx),
                       FormattingDivider,
                       DeptCount(DeptIdx)
           END-PERFORM.

       END PROGRAM Exercise11-7.
