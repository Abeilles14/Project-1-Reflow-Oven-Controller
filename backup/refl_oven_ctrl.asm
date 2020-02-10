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

TEMP_BUTTON  equ P0.0		; Inc temperature
ALMIN_BUTTON  equ P0.3		; Inc minutes
ALSEC_BUTTON   equ P0.6		; Inc seconds

STARTSTOP_BUTTON equ P2.7	; Start/Stop process immediately, Settings
MODE_BUTTON equ P2.4				; Switch Displays between Clock, Current Temp, Settings/timer

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
currentTemp: ds 1	; current temperature from sensor
SoakTemp: ds 1		; set soak temperature
ReflTemp: ds 1		; set refl temperature
; TIMER COUNTERS	; contains counters and timers
Count1ms: ds 2 		; Used to determine when (1) second has passed
BCD_counterSec: ds 1
BCD_counterMin: ds 1
; ALARMS
SoakMinAlarm: ds 1		;contains set time values
SoakSecAlarm: ds 1
ReflMinAlarm: ds 1
ReflSecAlarm: ds 1

BSEG
mf: dbit 1
half_seconds_flag: dbit 1	; Set to 1 in the ISR every time 1000 ms had passed (actually 1 second flag)
start_counter: dbit 1		; Set to 1 once ready to start countdown
refltimer_done: dbit 1		; Set to 1 once refl timer starts
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
_C:	DB '000C',0
_blank: DB ' ',0
_default: DB '00:00',0
_Warning: DB '!', 0

$NOLIST
;$include(math32.inc)
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
	; Decrement the BCD counter
	mov a, BCD_counterSec
	sjmp Timer2_ISR_decrement		; jump to decrement counter
;    jnb ALSEC_BUTTON, Timer2_ISR_decrement
;	add a, #0x01
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
    
;----------------------;
;    MAIN PROGRAM      ;
;----------------------;
MainProgram:
    mov SP, #7FH
    lcall LCD_4BIT
    
    ; enable global interrupts
    lcall Timer0_Init
    lcall Timer2_Init
    
    ; In case you decide to use the pins of P0 configure the port in bidirectional mode:
    mov P0M0, #0	;ESSENTIAL!! BUTTONS WILL GO NUTS
    mov P0M1, #0
   	
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
	Set_Cursor(1,6)
	Send_Constant_String(#_C)
	Set_Cursor(1,10)
	Send_Constant_String(#_blank)
	Set_Cursor(1,11)
	Send_Constant_String(#_default)
	
	Set_Cursor(2,1)
	Send_Constant_String(#_Refl)
	Set_Cursor(2,6)
	Send_Constant_String(#_C)
	Set_Cursor(2,10)
	Send_Constant_String(#_blank)
	Set_Cursor(2,11)
	Send_Constant_String(#_default)
	
	lcall INIT_SPI
	lcall InitSerialPort

	setb EA		;counter not running originally
	
	; Set counters
	mov BCD_counterSec, #0x00
	mov BCD_counterMin, #0x00
	mov a, BCD_counterSec 		; number to be displayed placed in accumulator
	Set_Cursor(1, 11)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counterMin); 
	Set_Cursor(2, 11)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counterSec);
	ljmp SetupSoak			; sets up all soak temp, time, refl temp, time before counter start

;-----------------------------;
;	SET SOAK/REFL SETTINGS	  ;
;-----------------------------;
;--------- SETUP SOAK ---------;
SetupSoak:
	jb BOOT_BUTTON, SetSoakTemp  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, SetSoakTemp  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $
	
	;Make LCD screen blink??
	
	clr a					; clear all settings
	mov SoakTemp, a
	mov SoakMinAlarm, a
	mov SoakSecAlarm, a
	lcall Display_Soak
		
	ljmp SetupSoak	;loops in Setup until Start button pressed

CheckReflSet:			; if startmode button pressed, set refl
	jb STARTSTOP_BUTTON, SetupSoak
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, SetupSoak
    jnb STARTSTOP_BUTTON, $
    ljmp SetupRefl
    	
SetSoakTemp:
	jb TEMP_BUTTON, SetSoakMin ; if 'soak min' button is not pressed, check soak sec
    Wait_Milli_seconds(#50)
    jb TEMP_BUTTON, SetSoakMin
    jnb TEMP_BUTTON, $
    ; increment Soak temp
	mov a, SoakTemp
	add a, #0x01
	da a
	mov SoakTemp, a
	clr a
	lcall Display_Soak
	;lcall Display_Refl
	ljmp SetupSoak
	
SetSoakMin:
	jb ALMIN_BUTTON, SetSoakSec
    Wait_Milli_seconds(#50)
    jb ALMIN_BUTTON, SetSoakSec
    jnb ALMIN_BUTTON, $
    
	; Now increment Soak min
	mov a, SoakMinAlarm
	cjne a, #0x59, incrementSM		;if not equal to 59, add 1
	mov a, #0x00
	da a
	mov SoakMinAlarm, a
	clr a
	lcall Display_Soak
	ljmp SetupSoak	
incrementSM:
	add a, #0x01
	da a
	mov SoakMinAlarm, a
	clr a
	lcall Display_Soak
	ljmp SetupSoak
	
SetSoakSec:
	jb ALSEC_BUTTON, CheckReflSet
    Wait_Milli_seconds(#50)
    jb ALSEC_BUTTON, CheckReflSet
    jnb ALSEC_BUTTON, $
    
	; Now increment Soak sec
	mov a, SoakSecAlarm
	cjne a, #0x59, incrementSS		;if not equal to 59, add 1
	mov a, #0x00
	da a
	mov SoakSecAlarm, a
	clr a
	lcall Display_Soak
	ljmp SetupSoak
incrementSS:
	add a, #0x01
	da a
	mov SoakSecAlarm, a
	clr a
	lcall Display_Soak
	ljmp SetupSoak
    
;--------- SETUP REFLOW	--------;
SetupRefl:
	jb BOOT_BUTTON, SetReflTemp  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, SetReflTemp  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $
	
	;Make LCD screen blink??
	
	clr a
	mov ReflTemp, a
	mov ReflMinAlarm, a
	mov ReflSecAlarm, a
	lcall Display_Refl	
	
	ljmp SetupRefl	;loops in Setup until Start button pressed
		
CheckStartTimer:		; if modestart buttup pressed, start timer and main loop
	jb STARTSTOP_BUTTON, SetupRefl
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, SetupRefl
    jnb STARTSTOP_BUTTON, $
    
	setb half_seconds_flag		; pressed to exit settings and start timer
	setb TR2
	
	mov BCD_counterMin, SoakMinAlarm	; move time settings into counters
	mov BCD_counterSec, SoakSecAlarm
	
	clr refltimer_done; clear timer done flags
	
	ljmp Forever
	
SetReflTemp:
	jb TEMP_BUTTON, SetReflMin ; if 'soak min' button is not pressed, check soak sec
    Wait_Milli_seconds(#50)
    jb TEMP_BUTTON, SetReflMin
    jnb TEMP_BUTTON, $
    ; increment Soak temp
	mov a, ReflTemp
	add a, #0x01
	da a
	mov ReflTemp, a
	clr a
	lcall Display_Refl
	ljmp SetupRefl
	
SetReflMin:
	jb ALMIN_BUTTON, SetReflSec
    Wait_Milli_seconds(#50)
    jb ALMIN_BUTTON, SetReflSec
    jnb ALMIN_BUTTON, $
    
	; Now increment Soak min
	mov a, ReflMinAlarm
	cjne a, #0x59, incrementRM		;if not equal to 59, add 1
	mov a, #0x00
	da a
	mov ReflMinAlarm, a
	clr a
	lcall Display_Refl
	ljmp SetupRefl
incrementRM:
	add a, #0x01
	da a
	mov ReflMinAlarm, a
	clr a
	lcall Display_Refl
	ljmp SetupRefl
	
SetReflSec:
	jb ALSEC_BUTTON, CheckStartTimer
    Wait_Milli_seconds(#50)
    jb ALSEC_BUTTON, CheckStartTimer
    jnb ALSEC_BUTTON, $
    
	; Now increment Soak sec
	mov a, ReflSecAlarm
	cjne a, #0x59, incrementRS		;if not equal to 59, add 1
	mov a, #0x00
	da a
	mov ReflSecAlarm, a
	clr a
	lcall Display_Refl
	ljmp SetupRefl
incrementRS:
	add a, #0x01
	da a
	mov ReflSecAlarm, a
	clr a
	lcall Display_Refl
	ljmp SetupRefl
   
;----------------------------;
;		 MAIN LOOP   		 ;
;----------------------------;

; forever loop interface with putty
Forever:
	; TEMPERATURE CHECK
	;lcall checktemp			;to display current temp later
	
	; TIME CHECK
	jb BOOT_BUTTON, CheckStop  ; buttons to change screen to Clock and Current Temp later
	Wait_Milli_Seconds(#50)
	jb BOOT_BUTTON, CheckStop
	jnb BOOT_BUTTON, $

	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	mov BCD_counterSec, a
	mov BCD_counterMin, a
	setb TR2                ; Start timer 2
	
	ljmp WriteNum 

	; Do this forever
	sjmp Forever

;;;;;;;;;;;;;;;;;;;;;;;;; DEBUG!!!	
CheckStop:
    jb STARTSTOP_BUTTON, loop_a		; if stop button not pressed, go loop and check for 00
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, loop_a
    jnb STARTSTOP_BUTTON, $
    
    clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	mov BCD_counterSec, a
		
	ljmp SetupSoak		; if stop button pressed, go back to setup
	
loop_a:
	jnb half_seconds_flag, Forever ;check if 1 second has passed...
loop_b:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    mov a, BCD_counterSec
    cjne a, #0x99, WriteNum  ;check to see if sec counter is at 00, skip if not 00
   
    mov a, #0x59 			; if sec at 00, reset to number 59
    mov BCD_counterSec, a
    
    clr a
    mov a, BCD_counterMin
    cjne a, #0x00, decrementMin	; check if minutes at 00, if so then stop, else decrement minutes
TimerDone:		; if timer done
	jnb refltimer_done, StartReflTimer		; if reflow timer not done, start reflow timer
	;else if refltimer done, finish process
	clr TR2                 ; Stop timer 2
	clr a	
	ljmp SetupSoak		; go back to settings
	
StartReflTimer:
	setb refltimer_done			; set to indicate final stage in process
	mov BCD_counterMin, ReflMinAlarm
	mov BCD_counterSec, ReflSecAlarm
	ljmp Forever
	
decrementMin:
    add a, #0x99 	;decrement minute counter
    da a
    mov BCD_counterMin, a
    clr a
bigBCDmincheck:   
    cjne a, #0x00, WriteNum ;first compare if BCD_counter2 = 00, if so, clear counter
    clr a
    mov BCD_counterMin, a
WriteNum:
	;check refltimer_done if update soak or refl time
	jnb refltimer_done, Display_SoakTimer	; if in soak stage, update soak display
	jb refltimer_done, Display_ReflTimer		; if in refl stage, update refl display
    ; jumps to Forever after display
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END DEBUG

Display_SoakTimer:
	Set_Cursor(1, 11)
	Display_BCD(BCD_counterMin)
	Set_Cursor(1, 14)
	Display_BCD(BCD_counterSec)
	ljmp Forever

Display_ReflTimer:
	Set_Cursor(2, 11)
	Display_BCD(BCD_counterMin)
	Set_Cursor(2, 14)
	Display_BCD(BCD_counterSec)
	ljmp Forever
	
END