000000** Character-minimal.cob — minimal correct EIGHTBOL class example
000010*** Copyright © 2026 Interworldly Adventuring, LLC
000020
001000 IDENTIFICATION DIVISION.
001100 CLASS-ID. Character.
001200 AUTHOR. Bruce-Robert Pocock.
001300 DATE-WRITTEN. 18 FEB 2026.
001400
001500 ENVIRONMENT DIVISION.
001600
001700 OBJECT.
001800     DATA DIVISION.
001900         WORKING-STORAGE SECTION.
002000* Declare slots locally so this example needs no external copybooks.
002100* In production, use COPY Phantasia-Globals. to pull in generated slots.
002200         05 HP    PIC 9999 USAGE BINARY.
002300         05 MaxHP PIC 9999 USAGE BINARY.
002400
002500     PROCEDURE DIVISION.
002600
002700         IDENTIFICATION DIVISION.
002800         METHOD-ID. "Think".
002900         PROCEDURE DIVISION.
003000* If HP is zero the character is dead — invoke the Kill method.
003100             IF HP IS EQUAL TO 0 THEN
003200                 INVOKE Self "Kill".
003300             END-IF.
003400             GOBACK.
003500         END METHOD "Think".
003600
003700         IDENTIFICATION DIVISION.
003800         METHOD-ID. "Kill".
003900         PROCEDURE DIVISION.
004000* Set HP to zero and exit.
004100             MOVE 0 TO HP.
004200             EXIT METHOD.
004300         END METHOD "Kill".
004400
004500         IDENTIFICATION DIVISION.
004600         METHOD-ID. "Heal".
004700         PROCEDURE DIVISION.
004800* Add 10 HP, capped at MaxHP.
004900             ADD 10 TO HP.
005000             IF HP > MaxHP THEN
005100                 MOVE MaxHP TO HP.
005200             END-IF.
005300             GOBACK.
005400         END METHOD "Heal".
005500
005600 END OBJECT.
005700
005800 END CLASS Character.
005900
999999
