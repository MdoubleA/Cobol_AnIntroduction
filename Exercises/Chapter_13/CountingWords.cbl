      ******************************************************************
      * Author: Michael Alaniz
      * Date: 10/24/2020
      * Purpose: Count the number of times a word occurs in a file using
      *          COBOL variable size table.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. vartab.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WordFile
             ASSIGN TO ".\Exercises\Chapter_13\wordlist.prn"
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SaveTableFile
             ASSIGN TO ".\Exercises\Chapter_13\unsortedtable.prn"
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT WorkFile
             ASSIGN TO ".\Exercises\Chapter_13\WORKFILE.TMP"
             ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SortedWordFile
             ASSIGN TO ".\Exercises\Chapter_13\sortedtable.prn"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD WordFile.
         01 WordRecord.
           88 EOWF VALUE HIGH-VALUES.
           02 AWord PIC X(22) VALUE SPACES.

       FD SaveTableFile.
         01 ATableEntry.
           02 FILLER PIC X(22).
           02 FILLER PIC 9(5).

       SD WorkFile.
         01 TempEntry.
           02 FILLER        PIC X(22).
           02 WorkWordCount PIC 9(5).

       FD SortedWordFile.
         01 SortedEntry.
           02 SortedWordCell  PIC X(22).
           02 SortedCountCell PIC 9(5).

       WORKING-STORAGE SECTION.
       *> Pre-allocated memory not used till this set to at least one.
       01 NumberOfWords PIC 9(5) VALUE ZERO.

       01 WordAndCount.
           02 WordCountTable OCCURS 1 TO 50000 TIMES
                             DEPENDING ON NumberOfWords
                             INDEXED BY WCidx.
             03 WordCell PIC X(22) VALUE SPACES.
             03 CountCell PIC 9(5) VALUE ZEROES.

       01 Counter PIC 99 VALUE ZERO.

       01 AReportRecord.
           02 RecordNumber PIC ZZ.
           02 FILLER       PIC XX VALUE ". ".
           02 DisplayWord  PIC X(22).
           02 FILLER       PIC X(3)  VALUE SPACES.
           02 DisplayCount PIC ZZZZZ.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM PopulateTable.
            *> DISPLAY WordAndCount.
            PERFORM WriteUnsortedTableToFile.
            PERFORM SortTableFile.
            PERFORM DisplayReport.

            STOP RUN.

       DisplayReport.
           DISPLAY "Top 10 most common words."
           OPEN INPUT SortedWordFile.
             PERFORM VARYING Counter FROM 1 BY 1 UNTIL Counter = 11
               READ SortedWordFile
               MOVE Counter TO RecordNumber
               MOVE SortedWordCell  TO DisplayWord
               MOVE SortedCountCell TO DisplayCount
               DISPLAY AReportRecord
             END-PERFORM
           CLOSE SortedWordFile.

       SortTableFile.
           SORT WorkFile
             ON DESCENDING KEY WorkWordCount
             USING  SaveTableFile
             GIVING SortedWordFile.

       WriteUnsortedTableToFile.
           OPEN OUTPUT SaveTableFile.
             PERFORM
               VARYING WCidx FROM 1 BY 1 UNTIL WCidx = NumberOfWords
                 MOVE WordCountTable(WCidx) to ATableEntry
                 WRITE ATableEntry
             END-PERFORM.
           CLOSE SaveTableFile.

       PopulateTable.
           OPEN INPUT WordFile.
             PERFORM UNTIL EOWF
               PERFORM ReadLine
               PERFORM IncrementTable
             END-PERFORM.
           CLOSE WordFile.

       IncrementTable.
           SET WCidx TO 1.
           SEARCH WordCountTable
             AT END
               ADD 1 TO NumberOfWords
               MOVE AWord TO WordCell(NumberOfWords)
               MOVE 1 TO CountCell(NumberOfWords)
             WHEN WordCell(WCidx) EQUALS AWord
               ADD 1 TO CountCell(WCidx)
           END-SEARCH.

       ReadLine.
           READ WordFile AT END SET EOWF TO TRUE.

       END PROGRAM vartab.
