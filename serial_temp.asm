
$MODLP51
org 0000H
    ljmp MainProgram
    
; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR
    
$NOLIST
$include(math32.inc)
$include(LCD_4bit.inc)
$LIST


; DECLARATIONS

CLK  EQU 22118400
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))

; interrupt

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))

;; PIN IN/OUTS
CE_ADC EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3

SOUND_OUT equ P3.7
CHANGE_MODE equ P0.1	; 0 IF CELCIUS, 1 IF KELVIN

; These register definitions needed by 'math32.inc'
DSEG at 30H
x:   ds 4
y:   ds 4
bcd: ds 5

; TEMPERATURE
SaveT: ds 4
currentTemp: ds 1
SoakTemp: ds 1
ReflTemp: 1
; TIMER COUNTERS
SoakMin: ds 1
SoakSec: ds 1
ReflMin: ds 1
ReflSec: ds 1
; ALARMS
SoakMinAlarm: ds 1
SoakSecAlarm: ds 1
ReflMinAlarm: ds 1
ReflSecAlarm: ds 1

;TODOLETE?
HiTemp: ds 1
LoTemp: ds 1
TEMP_HiTemp: ds 1
TEMP_LoTemp: ds 1

BSEG
mf: dbit 1

CSEG
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5

; LCD and Putty Strings
_Hello_World: DB 'Hello World!', '\r', '\n',0
_New_Line: DB '\r\n', 0
_Soak: DB 'Soak:',0
_Refl: DB 'Refl:',0
_Temperature_LCD: DB 'Temp:',0	
_C:	DB 'C',0
_blank: DB ' ',0
_default: DB '00:00',0

_Warning: DB '!', 0

; INIT SPI

INIT_SPI:
 	setb MY_MISO ; Make MISO an input pin
 	clr MY_SCLK ; For mode (0,0) SCLK is zero
 	ret

DO_SPI_G:
 	push acc
 	mov R1, #0 ; Received byte stored in R1
 	mov R2, #8 ; Loop counter (8-bits)
DO_SPI_G_LOOP:
 	mov a, R0 ; Byte to write is in R0
 	rlc a ; Carry flag has bit to write
 	mov R0, a
 	mov MY_MOSI, c
 	setb MY_SCLK ; Transmit
 	mov c, MY_MISO ; Read received bit
 	mov a, R1 ; Save received bit in R1
 	rlc a
 	mov R1, a
 	clr MY_SCLK
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

;;; Char Stuff ;;;
; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret
    
; Get character using serial port
getchar:
	jnb RI, getchar
	clr RI
	mov a, SBUF
	ret
	
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 		; Clear the bits for timer 0
	orl a, #0x01 		; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  		; Enable timer 0 interrupt
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti
    
; send a string until 0
;SendString:
;    clr A
;    movc A, @A+DPTR
;    jz SendStringDone
;    lcall putchar
;    inc DPTR
;    sjmp SendString
    
;SendStringDone:
;    ret
    
    
; Delay Function
WaitHalfSec:
       mov R2, #89
wait3: mov R1, #250
wait2: mov R0, #166
wait1: djnz R0, wait1 		; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, wait2 			; 22.51519us*250=5.629ms
    djnz R2, wait3 			; 5.629ms*89=0.5s (approximately)
    ret
    

; Alarm Function
TempAlarm:
	lcall bcd2hex
	load_y(35)
	lcall x_gteq_y
	
	; mf = 1 if x>=y, 0 if x<y
	jb mf, AlarmOn
	
	; set timer 0 to 0
	clr TR0
	lcall hex2bcd
	lcall ClearWarning
	ret
	
AlarmOn:
	; enable timer0
	setb TR0
	lcall hex2bcd
	lcall WriteWarning
	ret

; TODELETE	
WriteWarning:
	Set_Cursor(1,16)
	Send_Constant_String(#_Warning)
	Set_Cursor(2,16)
	Send_Constant_String(#_Warning)
	ret
	
ClearWarning:
	Set_Cursor(1,16)
	Send_Constant_String(#_blank)
	Set_Cursor(2,16)
	Send_Constant_String(#_blank)
	ret
	

;NEW HI LO W/ a	
;HiLo:
;    clr c
;    mov a, currentTemp
;    subb a, HiTemp
;    jc keepoldhitemp
;    mov HiTemp, currentTemp
    
;keepoldhitemp:
;    clr c
;    mov a, currentTemp
;    subb a, LoTemp
;    jnc keepoldlotemp
;    mov LoTemp, currentTemp
    
;keepoldlotemp:

;	Set_Cursor(1,13)
;	Display_BCD(HiTemp)
;	Set_Cursor(2,13)
;	Display_BCD(LoTemp)
;	ret
	
			
    
;----------------------;
;    MAIN PROGRAM      ;
;----------------------;
MainProgram:
    mov SP, #7FH
    lcall LCD_4BIT
    ;set constant strings lcd
    Set_Cursor(1,1)
	Send_Constant_String(#_Soak)
	Set_Cursor(1,9)
	Send_Constant_String(#_C)
	Set_Cursor(1,10)
	Send_Constant_String(#_blank)
	Set_Cursor(1,11)
	Send_Constant_String(#_default)
	
	Set_Cursor(2,1)
	Send_Constant_String(#_Refl)
	Set_Cursor(2,9)
	Send_Constant_String(#_C)
	Set_Cursor(2,10)
	Send_Constant_String(#_blank)
	Set_Cursor(2,11)
	Send_Constant_String(#_default)
	
	lcall INIT_SPI
	lcall InitSerialPort

	mov HiTemp, #0x0
	mov LoTemp, #0x99
	
	; enable global interrupts
	lcall Timer0_Init
	setb EA
	
; forever loop interface with putty
Forever:
	cpl P3.7
	clr CE_ADC
	mov R0, #00000001B 		; start at bit 1
	lcall DO_SPI_G
	
	mov R0, #10000000B 		; read channel 0
	lcall DO_SPI_G
	mov a, R1 				; R1 contains bits 8 and 9
	anl a, #00000011B 		; We need only the two least significant bits (AND)
	
	mov SaveT+1, a 		; Save result high.
	
	mov R0, #55H 		; Don't care
	lcall DO_SPI_G
	mov SaveT, R1 		; R1 bits 0 to 7, save result low.
	setb CE_ADC
	lcall WaitHalfSec
	
	; Convert SPI reading into readable temperatures
	lcall GetTemp

	; Do this forever
	sjmp Forever
	
GetTemp:
	mov x, SaveT
	mov x+1, SaveT+1
	mov x+2, #0
	mov x+3, #0
	
	; celcius
	load_y(410)
	lcall mul32
	load_y(1023)
	lcall div32
	load_y(273)
	lcall sub32

	lcall hex2bcd
	lcall Display_TempC
	
	mov currentTemp, bcd

	; Check temperature and sound alarm if ...
	lcall TempAlarm
	
SendCelcius:
	; convert back to celcius, since bcd is in kelvin
	lcall bcd2hex
	load_y(273)
	lcall sub32
	lcall hex2bcd
	Display_BCD(bcd)
	ret
	
;------------------------------------;
;   Sends 10 BCD num in bcd to LCD   ;
;------------------------------------;

Display_TempC:
	Set_Cursor(2, 6)
	Display_BCD(bcd+0)
	ret
	
Read_ADC_Channel:
	clr CE_ADC
	mov R0, #00000001B 	; start at b1
	lcall DO_SPI_G
	mov a, b
	swap a
	anl a, #0F0H
	setb acc.7 		; set onsingle mode (bit 7)
	mov R0, a

	lcall DO_SPI_G
	mov a, R1 			; R1 bits 8 9
	anl a, #00000011B 		; last 2 sig bits
	mov R7, a 				; save high.
	mov R0, #55H 		; don't care
	lcall DO_SPI_G
	mov aR6, R1 		; R1 bits 0-7 save low
	setb CE_ADC
	ret
	
END
