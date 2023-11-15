cd C:\Users\fuert\.vscode

@REM C:\cc65\bin\ca65 C:\Users\fuert\.vscode\NESGuide\fondo.s -o C:\Users\fuert\.vscode\NESGuide\fondo.o -t nes
@REM C:\cc65\bin\ld65 C:\Users\fuert\.vscode\NESGuide\fondo.o -o C:\Users\fuert\.vscode\NESGuide\fondo.nes -t nes

@REM C:\cc65\bin\ca65 C:\Users\fuert\.vscode\NESGuide\control.s -o C:\Users\fuert\.vscode\NESGuide\control.o -t nes
@REM C:\cc65\bin\ld65 C:\Users\fuert\.vscode\NESGuide\control.o -o C:\Users\fuert\.vscode\NESGuide\control.nes -t nes

@REM C:\cc65\bin\ca65 C:\Users\fuert\.vscode\NESGuide\personaje.s -o C:\Users\fuert\.vscode\NESGuide\personaje.o -t nes
@REM C:\cc65\bin\ld65 C:\Users\fuert\.vscode\NESGuide\personaje.o -o C:\Users\fuert\.vscode\NESGuide\personaje.nes -t nes

C:\cc65\bin\ca65 C:\Users\fuert\.vscode\NESGuide\animacion.s -o C:\Users\fuert\.vscode\NESGuide\animacion.o -t nes
C:\cc65\bin\ld65 C:\Users\fuert\.vscode\NESGuide\animacion.o -o C:\Users\fuert\.vscode\NESGuide\animacion.nes -t nes

C:\cc65\bin\ca65 C:\Users\fuert\.vscode\NESGuide\animacion2.s -o C:\Users\fuert\.vscode\NESGuide\animacion2.o -t nes
C:\cc65\bin\ld65 C:\Users\fuert\.vscode\NESGuide\animacion2.o -o C:\Users\fuert\.vscode\NESGuide\animacion2.nes -t nes

C:\cc65\bin\ca65 C:\Users\fuert\.vscode\NESGuide\animacion3.s -o C:\Users\fuert\.vscode\NESGuide\animacion3.o -t nes
C:\cc65\bin\ld65 C:\Users\fuert\.vscode\NESGuide\animacion3.o -o C:\Users\fuert\.vscode\NESGuide\animacion3.nes -t nes

@REM   PHP  ; Start by saving registers,
@REM   PHA  ; as usual.
@REM   TXA
@REM   PHA
@REM   TYA
@REM   PHA

@REM PLA ; Done with updates, restore registers
@REM   TAY ; and return to where we called this
@REM   PLA
@REM   TAX
@REM   PLA
@REM   PLP
@REM   RTS