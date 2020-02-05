$NOLIST
$MODLP51
$LIST

; INTERRUPTS
TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

;DECLARATIONS
CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))

; PINS INPUT OUTPUTS
CE_ADC EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3

BOOT_BUTTON equ P4.5
SOUND_OUT equ P3.7

START equ P0.1				; Start Reflow process
STOP equ P0.3				; Stop Reflow process immediately, reset.
MODE_BUTTON equ P4.5		; Switch between Clock, Current Temp, and Soak/Refl Displays

TEMP_BUTTON  equ P0.7		; Inc temperature
ALMIN_BUTTON  equ P4.5		; Inc minutes
ALSEC_BUTTON   equ P2.4		; Inc seconds

; Reset vector
org 0x0000
    ljmp MainProgram
    
; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; These register definitions needed by 'math32.inc'
DSEG at 0x30
x:   ds 4
y:   ds 4
bcd: ds 5

; TEMPERATURE
SaveT: ds 4
currentTemp: ds 1
SoakTemp: ds 1
ReflTemp: ds 1
; TIMER COUNTERS
Count1ms: ds 2 		; Used to determine when (1) second has passed
;BCD_soakMin: ds 1 ;delete?
;BCD_soakSec: ds 1 ;delete?
;BCD_reflMin: ds 1 ;delete?
;BCD_reflSec: ds 1 ;delete?
BCD_counterSec: ds 1
BCD_counterMin: ds 1
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
half_seconds_flag: dbit 1		; Set to 1 in the ISR every time 1000 ms had passed (actually 1 second flag)
start_counter: dbit 1			; Set to 1 once ready to start countdown

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

$NOLIST
$include(math32.inc)
$include(LCD_4bit.inc)
$LIST

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
	
EX1_ISR:
   clr TR2
   reti

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


;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1  
    
    Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counterSec
    jnb ALSEC_BUTTON, Timer2_ISR_decrement
	add a, #0x01
	sjmp Timer2_ISR_da
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counterSec, a
	
Timer2_ISR_done:
	pop psw
	pop acc
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
;TempAlarm:
;	lcall bcd2hex
;	load_y(35)
;	lcall x_gteq_y
	
	; mf = 1 if x>=y, 0 if x<y
;	jb mf, AlarmOn
	
	; set timer 0 to 0
;	clr TR0
;	lcall hex2bcd
;	lcall ClearWarning
;	ret
	
;AlarmOn:
	; enable timer0
;	setb TR0
;	lcall hex2bcd
;	lcall WriteWarning
;	ret

; TODELETE	
;WriteWarning:
;	Set_Cursor(1,16)
;	Send_Constant_String(#_Warning)
;	Set_Cursor(2,16)
;	Send_Constant_String(#_Warning)
;	ret
	
;ClearWarning:
;	Set_Cursor(1,16)
;	Send_Constant_String(#_blank)
;	Set_Cursor(2,16)
;	Send_Constant_String(#_blank)
;	ret
	

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
    
    ; enable global interrupts
    lcall Timer0_Init
    lcall Timer2_Init
   	
    mov SoakTemp, #0x00
   	mov ReflTemp, #0x00
	mov BCD_counterSec, #0x00
	mov BCD_counterMin, #0x00
	mov SoakMinAlarm, #0x00
	mov SoakSecAlarm, #0x00
	mov ReflMinAlarm, #0x00
	mov ReflSecAlarm, #0x00
   	
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

	;mov HiTemp, #0x0	;TODELETE?
	;mov LoTemp, #0x99	;TODELETE?

	setb EA		;counter not running originally
	
	; Set counters
	mov BCD_counterSec, #0x00
	mov BCD_counterMin, #0x00
	mov a, BCD_counterSec 		; number to be displayed placed in accumulator
	Set_Cursor(1, 11)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counterMin); 
	Set_Cursor(2, 11)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counterSec);
	ljmp Setup			; *See in macros, sets up all soak temp, time, refl temp, time before counter start

;-----------------------------;
;	SET SOAK/REFL SETTINGS	  ;
;-----------------------------;
Setup:
	jb TEMP_BUTTON, SetSoakMin ; if 'soak min' button is not pressed, check soak sec
    Wait_Milli_seconds(#50)
    jb TEMP_BUTTON, SetSoakMin
    jnb TEMP_BUTTON, $
    
	; increment Soak temp
	mov a, SoakTemp
	add a, #0x01
	da a
	mov SoakTemp, a
	lcall Display_Soak  
	sjmp Setup	;loops in Setup until Start button pressed

SetSoakMin:
	jb ALMIN_BUTTON, SetSoakSec
    Wait_Milli_seconds(#50)
    jb ALMIN_BUTTON, SetSoakSec
    jnb ALMIN_BUTTON, $
    
	; Now increment Soak temp
	mov a, BCD_counterMin
	add a, #0x01
	da a
	mov BCD_counterMin, a
	lcall Display_Soak
	ljmp Setup
	
SetSoakSec:
	jb ALSEC_BUTTON, Setup
    Wait_Milli_seconds(#50)
    jb ALSEC_BUTTON, Setup
    jnb ALSEC_BUTTON, $
    
	; Now increment Soak temp
	mov a, BCD_counterSec
	add a, #0x01
	da a
	mov BCD_counterSec, a
	lcall Display_Soak
	ljmp Setup
	
SetReflTemp:
	jb ALSEC_BUTTON, StartTimer
    Wait_Milli_seconds(#50)
    jb ALSEC_BUTTON, StartTimer
    jnb ALSEC_BUTTON, $
    
	; Now increment Soak temp
	mov a, BCD_counterSec
	add a, #0x01
	da a
	mov BCD_counterSec, a
	lcall Display_Soak
	ljmp Setup
	
StartTimer:			; pressed to exit settings and start timer
	setb half_seconds_flag
	setb TR2
	setb ET0
	ljmp Forever
 


;----------------------------;
;	 TEMP AND TIME CHECK     ;
;----------------------------;


; forever loop interface with putty
Forever:
	; TEMPERATURE CHECK
	lcall checktemp
	
	; TIME CHECK
	jb BOOT_BUTTON, checktime  ; boot resets display
	Wait_Milli_Seconds(#50)
	jb BOOT_BUTTON, checktime 
	jnb BOOT_BUTTON, $

	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	mov BCD_counterSec, a
	mov a, BCD_counterMin
	add a, #0x01
	da a
	mov BCD_counterMin, a
	setb TR2                ; Start timer 2
	setb ET0
	
	ljmp writenum 

	; Do this forever
	sjmp Forever

checktemp:
	cpl P3.7
	clr CE_ADC
	mov R0, #00000001B 		; start at bit 1
	lcall DO_SPI_G
	
	mov R0, #10000000B 		; read channel 0
	lcall DO_SPI_G
	mov a, R1 				; R1 contains bits 8 and 9
	anl a, #00000011B 		; We need only the two least significant bits (AND)
	
	;mov SaveT+1, a 		; Save result high.
	
	mov R0, #55H 		; Don't care
	lcall DO_SPI_G
	;mov SaveT, R1 		; R1 bits 0 to 7, save result low.
	setb CE_ADC
	lcall WaitHalfSec
	; Convert SPI reading into readable temperatures
	lcall GetTemp
	ret
	
checktime:
    jb STOP, loop_a ; if 'HOUR' button is not pressed, skip
    Wait_Milli_seconds(#50)
    jb STOP, loop_a
    jnb STOP, $
    ;clr TR2                 ; Stop timer 2
	;clr a
;	mov Count1ms+0, a
;	mov Count1ms+1, a
	; add to alarm_countermin(hours)
loop_a:
	jnb half_seconds_flag, forever ;check if 1 second has passed...
loop_b:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    mov a, BCD_counterSec ;check to see if counter is at 60
    cjne a, #0b01100000, writenum  ;jump if not number 61
    mov a, #0b00000000 ;reset to number 0
    mov BCD_counterSec, a
    mov a, BCD_counterMin ;add to minute counter
    add a, #0x01
    da a
    mov BCD_counterMin, a
bigBCDmincheck:   
    cjne a, #0b01100000, writenum ;first compare if BCD_counter2 = 60, if so, clear counter
    clr a
    mov BCD_counterMin, a
writenum:
    Set_Cursor(1, 11)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counterMin); 
	Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counterSec); 
    ljmp forever
    
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
;	lcall TempAlarm

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
