000000** Character-minimal.cob — minimal correct EIGHTBOL class example
000010*** Copyright © 2026 Interworldly Adventuring, LLC
000020
000030 IDENTIFICATION DIVISION.
000040 CLASS-ID. Character.
000050 AUTHOR. Bruce-Robert Pocock.
000060 DATE-WRITTEN. 18 FEB 2026.
000070
000080 ENVIRONMENT DIVISION.
000090
000100 OBJECT.
000110     DATA DIVISION.
000120         WORKING-STORAGE SECTION.
000130* Declare slots locally so this example needs no external copybooks.
000140* In production, use COPY Phantasia-Globals. to pull in generated slots.
000150         05 HP    PIC 9999 USAGE BINARY.
000160         05 MaxHP PIC 9999 USAGE BINARY.
000170
000180     PROCEDURE DIVISION.
000190
000200         IDENTIFICATION DIVISION.
000210         METHOD-ID. "Think".
000220         PROCEDURE DIVISION.
000230* If HP is zero the character is dead — invoke the Kill method.
000240             IF HP IS EQUAL TO 0 THEN
000250                 INVOKE Self "Kill".
000260             END-IF.
000270             GOBACK.
000280         END METHOD "Think".
000290
000300         IDENTIFICATION DIVISION.
000310         METHOD-ID. "Kill".
000320         PROCEDURE DIVISION.
000330* Set HP to zero and exit.
000340             MOVE 0 TO HP.
000350             EXIT METHOD.
000360         END METHOD "Kill".
000370
000380         IDENTIFICATION DIVISION.
000390         METHOD-ID. "Heal".
000400         PROCEDURE DIVISION.
000410* Add 10 HP, capped at MaxHP.
000420             ADD 10 TO HP.
000430             IF HP > MaxHP THEN
000440                 MOVE MaxHP TO HP.
000450             END-IF.
000460             GOBACK.
000470         END METHOD "Heal".
000480
000490 END OBJECT.
000500
000510 END CLASS Character.
000520
999999
