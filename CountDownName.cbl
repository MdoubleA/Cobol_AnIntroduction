      ******************************************************************
      * Author: MICHAEL ALANIZ
      * Date: 09/23/2020
      * Purpose: COUNT DOWN TO NAME.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NAME-COUNT-DOWN.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 TargetCount PIC 999 VALUE ZERO.
       01 Counter PIC 999 VALUE ZERO.
       01 RevealName PIC X(30).

       PROCEDURE DIVISION.
       CountDown.
            DISPLAY "Enter a name: "
            ACCEPT RevealName

            DISPLAY "Enter a count down: "
            ACCEPT TargetCount
            DISPLAY "----------------------"

            PERFORM VARYING Counter FROM TargetCount BY -1
            UNTIL Counter = 0
               DISPLAY Counter
            END-PERFORM

            DISPLAY "And it is a ", RevealName.
       END PROGRAM NAME-COUNT-DOWN.
