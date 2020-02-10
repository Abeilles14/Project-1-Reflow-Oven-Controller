$MODLP51
org 0000H
ljmp MainProgram
   
CLK  EQU 22118400
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))

CE_ADC EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3 
CE_EE EQU p2.4
CE_RTC EQU p2.5

dseg at 0x30
buffer: ds 30
x:   ds 4
y:   ds 4
bcd: ds 5
LM_Result: ds 2
TC_Result: ds 1
Result: ds 2
temp:ds 2
LM_TEMP: ds 2

bseg
mf: dbit 1


CSEG
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$include(math32.inc)
$include(will.inc)
$include(tempcheck.inc)

MainProgram:
lcall InitSerialPort
lcall INIT_SPI
lcall LCD_4BIT
forever:
lcall ReadTemp
mov x+0,Result+0
mov x+1,Result+0
lcall hex2bcd
Set_cursor (1,1) ;print to the lcd
Display_BCD(bcd+1)
Display_BCD(bcd+0)
WriteData(#'C')
lcall Wait_Sec
sjmp forever
END