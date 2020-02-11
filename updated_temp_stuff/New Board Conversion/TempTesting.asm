$NOLIST
$MOD9351
$LIST
org 0000H
ljmp MainProgram
 
CLK         EQU 14746000  ; Microcontroller system clock frequency in Hz
CCU_RATE    EQU 22050     ; 22050Hz is the sampling rate of the wav file we are playing
CCU_RELOAD  EQU ((65536-((CLK/(2*CCU_RATE)))))
BAUD        EQU 115200
BRVAL       EQU ((CLK/BAUD)-16)

FLASH_CE    EQU P2.4
SOUND       EQU P2.7

; Commands supported by the SPI flash memory according to the datasheet
WRITE_ENABLE     EQU 0x06  ; Address:0 Dummy:0 Num:0
WRITE_DISABLE    EQU 0x04  ; Address:0 Dummy:0 Num:0
READ_STATUS      EQU 0x05  ; Address:0 Dummy:0 Num:1 to infinite
READ_BYTES       EQU 0x03  ; Address:3 Dummy:0 Num:1 to infinite
READ_SILICON_ID  EQU 0xab  ; Address:0 Dummy:3 Num:1 to infinite
FAST_READ        EQU 0x0b  ; Address:3 Dummy:1 Num:1 to infinite
WRITE_STATUS     EQU 0x01  ; Address:0 Dummy:0 Num:1
WRITE_BYTES      EQU 0x02  ; Address:3 Dummy:0 Num:1 to 256
ERASE_ALL        EQU 0xc7  ; Address:0 Dummy:0 Num:0
ERASE_BLOCK      EQU 0xd8  ; Address:3 Dummy:0 Num:0
READ_DEVICE_ID   EQU 0x9f  ; Address:0 Dummy:2 Num:1 to infinite

;CE_ADC EQU P2.0
;MY_MOSI EQU P2.1
;MY_MISO EQU P2.2
;MY_SCLK EQU P2.3 
;CE_EE EQU p2.4
;CE_RTC EQU p2.5

DSEG at 0x30
;Jesus Stuff:
w:   ds 3 ; 24-bit play counter.  Decremented in CCU ISR.


;My Stuff:
buffer: ds 30
x:   ds 4
y:   ds 4
bcd: ds 5
LM_Result: ds 2
TC_Result: ds 2
Result: ds 2
temp:ds 2
LM_TEMP: ds 2

bseg
mf: dbit 1


CSEG
;NEW LCD PINS
LCD_RS equ P0.5
LCD_RW equ P0.6
LCD_E  equ P0.7
LCD_D4 equ P1.2
LCD_D5 equ P1.3
LCD_D6 equ P1.4
LCD_D7 equ P1.6

;OLD LCD PINS
;LCD_RS equ P1.1
;LCD_RW equ P1.2
;LCD_E  equ P1.3
;LCD_D4 equ P3.2
;LCD_D5 equ P3.3
;LCD_D6 equ P3.4
;LCD_D7 equ P3.5

$NOLIST
;$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$include(LCD_4bit_LPC9351.inc)
$include(math32.inc)
$include(will.inc)
$include(tempcheck.inc)
$include(Jesus_stuff.inc)
$LIST


MainProgram:
	lcall InitSerialPort
	lcall Ports_Init ; Default all pins as bidirectional I/O. See Table 42.
    lcall LCD_4BIT
    lcall Double_Clk
	lcall InitSerialPort
	lcall InitADC0 ; Call after 'Ports_Init'

	clr a
	mov x+1,a
	mov x+2,a
	mov x+3,a

forever:

	lcall ReadTemp

	push acc
	lcall Wait_Sec
	pop acc
	
	clr a
	mov x+0,Result+0
	mov x+1,Result+1
	mov x+2,a
	mov x+3,a
	
;PRINTS RESULT TEMPERATURE IN TOP LEFT	
	lcall hex2bcd
	push acc
	Set_cursor (1,1) ;print to the lcd
	pop acc
	lcall LCD_3BCD
	push acc
	WriteData(#'C')
	pop acc
	
		
	clr a
	mov x+0,LM_TEMP+0
	mov x+1,LM_TEMP+1
	mov x+2,a
	mov x+3,a
	
;PRINTS TEMPERATURE FROM LM335 (ROOM TEMP) IN BOTTOM LEFT
	lcall hex2bcd
	push acc
	Set_cursor (2,1) ;print to the lcd
	pop acc
	lcall LCD_3BCD
	push acc
	WriteData(#'C')
	pop acc
	
		
	clr a
	mov x+0,TC_Result+0
	mov x+1,TC_Result+1
	mov x+2,a
	mov x+3,a
;PRINTS THERMOCOUPLE TEMPERATURE ON BOTTOM RIGHT
	lcall hex2bcd
	push acc
	Set_cursor (2,6) ;print to the lcd
	pop acc
	lcall LCD_3BCD
	push acc
	WriteData(#'C')
	pop acc

ljmp forever

END
