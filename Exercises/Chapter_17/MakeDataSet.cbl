      ******************************************************************
      * Author: Michael Alaniz
      * Date: 10/27/2020
      * Purpose: To create an ordered data set to use in the creation
      *          of a report detailing tuition revenue by department and
      *          then by professor.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mkdata.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CoursesFile
             ASSIGN TO "./Exercises/Chapter_17/Courses.prn"
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT WorkFile ASSIGN TO "Work.TMP".

           SELECT SortedCoursesFile
             ASSIGN TO "./Exercises/Chapter_17/SortedCourses.prn"
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ProcessedCoursesFile
             ASSIGN TO "./Exercises/Chapter_17/CoursesProcessed.prn"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CoursesFile.
       01 CourseFileRecord PIC X(140).

       SD WorkFile.
       01 WorkRecord.
           02 WorkCourseSubject PIC X(4).
           02 FILLER        PIC X(2).
           02 WorkCourseNumber  PIC X(1).
           02 FILLER        PIC X(66).
           02 WorkLastName      PIC X(16).
           02 FILLER        PIC X(51).

       FD SortedCoursesFile.
       01 SortedCourseFileRecord.
           88 SortedEOCoursesFile VALUE HIGH-VALUES.
           02 SortedCourseSubject PIC X(4).
           02 FILLER        PIC X(2).
           02 SortedCourseNumber  PIC X(1).
           02 FILLER        PIC X(66).
           02 SortedLastName      PIC X(16).
           02 FILLER        PIC X(48).
           02 SortedEsch          PIC X(3).

       FD ProcessedCoursesFile.
       01 ProcessedCourseFileRecord.
           88 ProcessedEOCoursesFile VALUE HIGH-VALUES.
           02 ProcessedCourseSubject PIC X(4).
           02 FILLER                 PIC X(2).
           02 ProcessedCourseNumber  PIC X(1).
           02 FILLER                 PIC X(66).
           02 ProcessedLastName      PIC X(16).
           02 FILLER                 PIC X(51).

       WORKING-STORAGE SECTION.
       01 NumericEschField PIC 999.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           *> WorkCourseSubject, WorkLastName, WorkCourseNumber
            SORT WorkFile ON ASCENDING KEY
                WorkCourseSubject, WorkLastName, WorkCourseNumber
              USING CoursesFile
              GIVING SortedCoursesFile.

            OPEN INPUT SortedCoursesFile.
            OPEN OUTPUT ProcessedCoursesFile.
            PERFORM UNTIL SortedEOCoursesFile
              PERFORM ReadLine

                MOVE SortedEsch TO NumericEschField
                IF NumericEschField GREATER THAN ZERO
                  MOVE SortedCourseFileRecord
                    TO ProcessedCourseFileRecord
                  WRITE ProcessedCourseFileRecord
                END-IF

            END-PERFORM
            CLOSE ProcessedCoursesFile.
            CLOSE SortedCoursesFile.

            STOP RUN.

       ReadLine.
           READ SortedCoursesFile
             AT END SET SortedEOCoursesFile TO TRUE.

       END PROGRAM mkdata.
