@echo off
bin\vasmarm_std_win32.exe -L compile.txt -m250 -Fbin -opt-adr -o build\doomfire.bin doom-fire.asm
if %ERRORLEVEL%==0 copy build\doomfire.bin "..\..\Arculator_V2.0_Windows\hostfs\doomfire,ff8"
