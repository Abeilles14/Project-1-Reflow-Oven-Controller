$MODLP51
org 0000H
   ljmp MainProgram

CLK  EQU 22118400
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))


TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

CE_ADC    EQU  P2.0 
MY_MOSI   EQU  P2.1  
MY_MISO   EQU  P2.2 
MY_SCLK   EQU  P2.3 


dseg at 0x30

Result:	ds 2
TerminatingTemp: ds 2
Count1ms: ds 2
x:   ds 4
y:   ds 4
bcd: ds 5

bseg

mf: dbit 1
temporaryFlag: dbit 1
terminateFlag: dbit 1

cseg

Hello_World:
    DB 'TERMINATION', '\r','\n', 0
    
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
$LIST


; These equ must match the wiring between the microcontroller and ADC 

INIT_SPI: 
setb MY_MISO    ; Make MISO an input pin
clr MY_SCLK     ; For mode (0,0) SCLK is zero
Wait_Milli_Seconds(#10)
ret
DO_SPI_G: 
push acc 
mov R1, #0      ; Received byte stored in R1
mov R2, #8		; Loop counter (8-bits)
DO_SPI_G_LOOP: 
mov a, R0       ; Byte to write is in R0
rlc a           ; Carry flag has bit to write
mov R0, a 
mov MY_MOSI, c 
setb MY_SCLK    ; Transmit
Wait_Milli_Seconds(#10)
mov c, MY_MISO  ; Read received bit
mov a, R1       ; Save received bit in R1
rlc a 
mov R1, a 
clr MY_SCLK 
Wait_Milli_Seconds(#10)
djnz R2, DO_SPI_G_LOOP 
pop acc 
ret

; Configure the serial port and baud rate
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
    ret

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret


Delay:
	    Wait_Milli_Seconds(#200)
	        Wait_Milli_Seconds(#200)
	            Wait_Milli_Seconds(#200)
	                Wait_Milli_Seconds(#200)
	                    Wait_Milli_Seconds(#200)
ret


MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    lcall InitSerialPort
    lcall INIT_SPI
    lcall LCD_4BIT
    clr temporaryFlag
    clr terminateFlag
    
    Forever:
    clr CE_ADC
    mov R0, #00000001B	; Start bit:1
    lcall	DO_SPI_G
    mov	R0, #10000000B	; Single ended, read channel 0
    lcall	DO_SPI_G
    mov	a, R1           ; R1 contains bits 8 and 9
    anl	a, #00000011B  ; We need only the two least significant bits
    da a
    mov	Result+1, a    ; Save result high.
    mov	R0, #55H		; It doesn't matter what we transmit...
    lcall	DO_SPI_G
    mov a, R1
    da a
    mov	Result, R1     ; R1 contains bits 0 to 7.  Save result low.

    setb	CE_ADC
	lcall 	Delay
    lcall	Do_Something_With_Result
    ljmp	Forever
    
    Do_Something_With_Result:
	mov x+1, Result+1
	mov x+0, Result
	
	
;-----------------------------------------------------------------;
;	this code should work for allowing 							  ;
;	temperatures > 50 degree celcius							  ;
;	to < 173 degrees celcius.									  ;
;																  ;
;	In the case where temperature < 50 degrees celcius,			  ;
;	terminateFlag is turned on									  ;
;	Updated by Debby Lin 									      ;
;	2020-02-03													  ;
;-----------------------------------------------------------------;
checkHigh:
	clr c			  		  ; clear carry flag
	mov a, Result+1	 		  ; sent in result
	subb a, #high(806)		  ; subtract higherbits of result to #high(806) [50 degree celcius = 8.06 V]
	
; carry flag updated: ;
; 0 if a > val		  ;
; 1 if a <= val		  ;
		  
	cjne a, #0x00, isitsmaller ; if a == val, check lower values, else see if a < val

	clr c			  		  ; clear carry flag
	mov a, Result	 		  ; sent in Reselt
	subb a, #low(806)		  ; subtract lowerbits of result to #low(806) [50 degree celcius = 8.06 V]

isitsmaller:
	mov temporaryFlag, c	  ; move c to a flag so it can be compared
	jb temporaryFlag, flagup  ; if a (higher or lower bits) < val (c = 1), immediately set flag up
	sjmp noflagging
	
flagup:
	setb terminateFlag		  ;set terminate flag to 1

noflagging:
    Load_y(410)
    lcall mul32
    Load_y(1023)
    lcall div32

    
    Load_y(273)
    lcall sub32
    
    lcall hex2bcd
	
	Send_BCD(bcd+0)
	mov dptr, #Hello_World
	lcall SendString
    
    
    ret ; This is equivalent to 'forever: sjmp forever'


    
END


