$NOLIST
$MOD9351
$LIST

;DECLARATIONS
CLK         EQU 14746000  ; Microcontroller system clock frequency in Hz
CCU_RATE    EQU 22050     ; 22050Hz is the sampling rate of the wav file we are playing
CCU_RELOAD  EQU ((65536-((CLK/(2*CCU_RATE)))))
BAUD        EQU 115200
BRVAL       EQU ((CLK/BAUD)-16)

TIMER1_RATE   EQU 200     ; 200Hz, for a timer tick of 5ms
TIMER1_RELOAD EQU ((65536-(CLK/(2*TIMER1_RATE))))

toaster_on EQU P0.0

; PINS INPUT OUTPUTS
FLASH_CE EQU P2.4
MY_MOSI EQU P2.2
MY_MISO EQU P2.3
MY_SCLK EQU P2.5

SOUND equ P2.7

;Buttons
BOOT_BUTTON equ P2.6

TEMP_BUTTON  equ P0.2		; Inc temperature
ALMIN_BUTTON  equ P0.3	; Inc minutes
ALSEC_BUTTON   equ P0.1		; Inc seconds

STARTSTOP_BUTTON equ P3.0	; Start/Stop process immediately, Settings
MODE_BUTTON equ P3.1		; Switch Displays between Clock, Current Temp, Settings/timer

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

CSEG

; Reset vector
org 0x0000
    ljmp MainProgram
    
; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	reti

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	ljmp Timer1_ISR

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x005B
	ljmp CCU_ISR

; These register definitions needed by 'math32.inc'
DSEG at 0x30
w:	 ds 3
x:   ds 4
y:   ds 4
bcd: ds 5

; TEMPERATURE
SaveT: ds 4
currentTemp: ds 2	; current temperature from sensor
SoakTemp: ds 4		; set soak temperature
ReflTemp: ds 4		; set refl temperature
Power: ds 2
; TIMER COUNTERS	; contains counters and timers
Count5ms: ds 1
Count1ms: ds 2 		; Used to determine when (1) second has passed
BCD_counterSec: ds 1
BCD_counterMin: ds 1
BCD_runtimeSec: ds 1
BCD_runtimeMin: ds 1
; ALARMS
SoakMinAlarm: ds 1		;contains set time values
SoakSecAlarm: ds 1
ReflMinAlarm: ds 1
ReflSecAlarm: ds 1

BSEG
mf: dbit 1
half_seconds_flag: dbit 1	; Set to 1 in the ISR every time 1000 ms had passed (actually 1 second flag)
seconds_flag: dbit 1
timer_done: dbit 1		; Set to 1 once ready to start countdown
refltimer_done: dbit 1		; Set to 1 once refl timer starts
tempdisplay_flag: dbit 1	; Set to 1 for temp and run time display

CSEG
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P0.5
LCD_RW equ P0.6
LCD_E  equ P0.7
LCD_D4 equ P1.2
LCD_D5 equ P1.3
LCD_D6 equ P1.4
LCD_D7 equ P1.6

; LCD and Putty Strings
_Hello_World: DB 'Hello World!', '\r', '\n',0
_New_Line: DB '\r\n', 0
_Soak: DB 'Soak:',0
_Refl: DB 'Refl:',0
_Temperature_LCD: DB 'Temp:',0
_Power: DB 'Power:%',0	
_C:	DB '000C',0
_blank: DB ' ',0
_default: DB '00:00',0
_clearLCD: DB '                ',0

$NOLIST
$include(math32.inc)
$include(LCD_4bit_LPC9351.inc)
$include(voice_feedback.asm)
$include(Blinkymacro.inc)
;$include (reflproc_FSM.asm)
$LIST

;------------------------------;
; 		PORT INIT/CONFIG	   ;
;------------------------------;

Ports_Init:
    ; Configure all the ports in bidirectional mode:
    mov P0M1, #00H
    mov P0M2, #00H
    mov P1M1, #00H
    mov P1M2, #00H ; WARNING: P1.2 and P1.3 need 1 kohm pull-up resistors if used as outputs!
    mov P2M1, #00H
    mov P2M2, #00H
    mov P3M1, #00H
    mov P3M2, #00H
	ret

;---------------------------------;
; Sends a byte via serial port    ;
;---------------------------------;
putchar:
	jbc	TI,putchar_L1
	sjmp putchar
putchar_L1:
	mov	SBUF,a
	ret

;---------------------------------;
; Receive a byte from serial port ;
;---------------------------------;
getchar:
	jbc	RI,getchar_L1
	sjmp getchar
getchar_L1:
	mov	a,SBUF
	ret

; Configure the serial port and baud rate
InitSerialPort:
	mov	BRGCON,#0x00
	mov	BRGR1,#high(BRVAL)
	mov	BRGR0,#low(BRVAL)
	mov	BRGCON,#0x03 ; Turn-on the baud rate generator
	mov	SCON,#0x52 ; Serial port in mode 1, ren, txrdy, rxempty
	; Make sure that TXD(P1.0) and RXD(P1.1) are configured as bidrectional I/O
	anl	P1M1,#11111100B
	anl	P1M2,#11111100B
	ret

;---------------------------------;
; Initialize ADC1/DAC1 as DAC1.   ;
; Warning, the ADC1/DAC1 can work ;
; only as ADC or DAC, not both.   ;
; The P89LPC9351 has two ADC/DAC  ;
; interfaces.  One can be used as ;
; ADC and the other can be used   ;
; as DAC.  Also configures the    ;
; pin associated with the DAC, in ;
; this case P0.4 as 'Open Drain'. ;
;---------------------------------;
InitDAC1:
    ; Configure pin P0.4 (DAC1 output pin) as open drain
	orl	P0M1,   #00010000B
	orl	P0M2,   #00010000B
    mov ADMODB, #00101000B ; Select main clock/2 for ADC/DAC.  Also enable DAC1 output (Table 25 of reference manual)
	mov	ADCON1, #00000100B ; Enable the converter
	mov AD1DAT3, #0x80     ; Start value is 3.3V/2 (zero reference for AC WAV file)
	ret

;---------------------------------;
; Initialize ADC0/DAC0 as ADC0.   ;
;---------------------------------;
InitADC0:
	; ADC0_0 is connected to P1.7
	; ADC0_1 is connected to P0.0
	; ADC0_2 is connected to P2.1
	; ADC0_3 is connected to P2.0
    ; Configure pins P1.7, P0.0, P2.1, and P2.0 as inputs
    orl P0M1, #00000001b
    anl P0M2, #11111110b
    orl P1M1, #10000000b
    anl P1M2, #01111111b
    orl P2M1, #00000011b
    anl P2M2, #11111100b
	; Setup ADC0
	setb BURST0 ; Autoscan continuos conversion mode
	mov	ADMODB,#0x20 ;ADC0 clock is 7.3728MHz/2
	mov	ADINS,#0x0f ; Select the four channels of ADC0 for conversion
	mov	ADCON0,#0x05 ; Enable the converter and start immediately
	; Wait for first conversion to complete
InitADC0_L1:
	mov	a,ADCON0
	jnb	acc.3,InitADC0_L1
	ret

;---------------------------------;
; Change the internal RC osc. clk ;
; from 7.373MHz to 14.746MHz.     ;
;---------------------------------;
Double_Clk:
    mov dptr, #CLKCON
    movx a, @dptr
    orl a, #00001000B ; double the clock speed to 14.746MHz
    movx @dptr,a
	ret

;---------------------------------;
; Initialize the SPI interface    ;
; and the pins associated to SPI. ;
;---------------------------------;
Init_SPI:
	; Configure MOSI (P2.2), CS* (P2.4), and SPICLK (P2.5) as push-pull outputs (see table 42, page 51)
	anl P2M1, #low(not(00110100B))
	orl P2M2, #00110100B
	; Configure MISO (P2.3) as input (see table 42, page 51)
	orl P2M1, #00001000B
	anl P2M2, #low(not(00001000B)) 
	; Configure SPI
	mov SPCTL, #11010000B ; Ignore /SS, Enable SPI, DORD=0, Master=1, CPOL=0, CPHA=0, clk/4
	ret

;---------------------------------;
; Sends AND receives a byte via   ;
; SPI.                            ;
;---------------------------------;
Send_SPI:
	mov SPDAT, a
Send_SPI_1:
	mov a, SPSTAT 
	jnb acc.7, Send_SPI_1 ; Check SPI Transfer Completion Flag
	mov SPSTAT, a ; Clear SPI Transfer Completion Flag
	mov a, SPDAT ; return received byte via accumulator
	ret

;---------------------------------;
; SPI flash 'write enable'        ;
; instruction.                    ;
;---------------------------------;
Enable_Write:
	clr FLASH_CE
	mov a, #WRITE_ENABLE
	lcall Send_SPI
	setb FLASH_CE
	ret

;---------------------------------;
; This function checks the 'write ;
; in progress' bit of the SPI     ;
; flash memory.                   ;
;---------------------------------;
Check_WIP:
	clr FLASH_CE
	mov a, #READ_STATUS
	lcall Send_SPI
	mov a, #0x55
	lcall Send_SPI
	setb FLASH_CE
	jb acc.0, Check_WIP ;  Check the Write in Progress bit
	ret
	
;---------------------------------;
; CRC-CCITT (XModem) Polynomial:  ;
; x^16 + x^12 + x^5 + 1 (0x1021)  ;
; CRC in [R7,R6].                 ;
; Converted to a macro to remove  ;
; the overhead of 'lcall' and     ;
; 'ret' instructions, since this  ;
; 'routine' may be executed over  ;
; 4 million times!                ;
;---------------------------------;
;crc16:
crc16 mac
	xrl	a, r7			; XOR high of CRC with byte
	mov r0, a			; Save for later use
	mov	dptr, #CRC16_TH ; dptr points to table high
	movc a, @a+dptr		; Get high part from table
	xrl	a, r6			; XOR With low byte of CRC
	mov	r7, a			; Store to high byte of CRC
	mov a, r0			; Retrieve saved accumulator
	mov	dptr, #CRC16_TL	; dptr points to table low	
	movc a, @a+dptr		; Get Low from table
	mov	r6, a			; Store to low byte of CRC
	;ret
endmac

EX1_ISR:
   clr ECCU
   reti


;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 1                     ;
;---------------------------------;
Timer1_Init:
	mov a, TMOD
	anl a, #0x0f ; Clear the bits for timer 1
	orl a, #0x10 ; Configure timer 1 as 16-timer
	mov TMOD, a
	mov TH1, #high(TIMER1_RELOAD)
	mov TL1, #low(TIMER1_RELOAD)
	; Enable the timer and interrupts
    setb ET1  ; Enable timer 1 interrupt
    setb TR1  ; Start timer 1
	ret

;---------------------------------;
; ISR for timer 1                 ;
;---------------------------------;
Timer1_ISR:
	mov TH1, #high(TIMER1_RELOAD)
	mov TL1, #low(TIMER1_RELOAD)
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 8-bit 5-mili-second counter
	inc Count5ms

Inc_Done:
	; Check if half second has passed
	mov a, Count5ms
	cjne a, #200, Timer1_ISR_done ; Warning: this instruction changes the carry flag!
	
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know half second had passed
	; Reset to zero the 5-milli-seconds counter, it is a 8-bit variable
	mov Count5ms, #0
	
	; decrement seconds
	mov a , BCD_counterSec
	add a, #0x99
	da a
	mov BCD_counterSec, a
	cjne a, #0x99, Timer1_ISR_done
	mov BCD_counterSec, #0x59
	;decrement minutes
	mov a , BCD_counterMin
	add a, #0x99
	da a
	mov BCD_counterMin, a
	cjne a, #0x99, Timer1_ISR_done	;If timer minutes at 0, set timerdone flag
	setb timer_done	
	mov BCD_counterMin, #0x00
	
Timer1_ISR_done:
	pop psw
	pop acc
	reti
	
;----------------------;
;    MAIN PROGRAM      ;
;----------------------;
MainProgram:
    mov SP, #0x7F
    
    lcall Ports_Init ; Default all pins as bidirectional I/O. See Table 42.
    lcall LCD_4BIT
    lcall Double_Clk
	lcall InitADC0 ; Call after 'Ports_Init'
	lcall InitDAC1 ; Call after 'Ports_Init'
	lcall CCU_Init
	lcall Init_SPI
	lcall Timer1_Init
	lcall InitSerialPort
	
	clr TR1
	clr TMOD20 ; Stop CCU timer
	clr SOUND ; Turn speaker off
	clr T2S_FSM_Start
	setb EA ; Enable global interrupts.

	; initialize vars
	mov T2S_FSM_state, #0
    mov SoakTemp, #0x00
   	mov ReflTemp, #0x00
   	mov SoakTemp+1, #0x00
   	mov ReflTemp+1, #0x00
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

	ljmp State0_SetupSoak			; sets up all soak temp, time, refl temp, time before counter start

;-------------------------------------;
;	STATE0 SET SOAK/REFL SETTINGS	  ;
;-------------------------------------;
;--------- SETUP SOAK ---------;
State0_SetupSoak:

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
		
	ljmp State0_SetupSoak	;loops in Setup until Start button pressed

CheckReflSet:			; if startmode button pressed, set refl
	jb STARTSTOP_BUTTON, State0_SetupSoak
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, State0_SetupSoak
    jnb STARTSTOP_BUTTON, $
    ljmp State0_SetupRefl
    	
SetSoakTemp:
	jb TEMP_BUTTON, SetSoakMin ; if 'soak min' button is not pressed, check soak sec
    Wait_Milli_seconds(#50)
    jb TEMP_BUTTON, SetSoakMin
    jnb TEMP_BUTTON, $
    
    ; increment Soak temp
	mov a, SoakTemp
	cjne a, #0x99, dontincrementhigherSOAK
incrementhigherSOAK:
	mov a, SoakTemp+1
	add a, #0x01
	da a
	mov SoakTemp+1, a
dontincrementhigherSOAK:
	add a, #0x01
	da a
	mov SoakTemp, a
	clr a
	lcall Display_Soak
	ljmp State0_SetupSoak
	
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
	ljmp State0_SetupSoak	
incrementSM:
	add a, #0x01
	da a
	mov SoakMinAlarm, a
	clr a
	lcall Display_Soak
	ljmp State0_SetupSoak
	
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
	ljmp State0_SetupSoak
incrementSS:
	add a, #0x01
	da a
	mov SoakSecAlarm, a
	clr a
	lcall Display_Soak
	ljmp State0_SetupSoak
    
;--------- SETUP REFLOW	--------;
State0_SetupRefl:
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
	
	ljmp State0_SetupRefl	;loops in Setup until Start button pressed
	
SetReflTemp:
	jb TEMP_BUTTON, SetReflMin ; if 'soak min' button is not pressed, check soak sec
    Wait_Milli_seconds(#50)
    jb TEMP_BUTTON, SetReflMin
    jnb TEMP_BUTTON, $
    ; increment Soak temp
	mov a, ReflTemp
	cjne a, #0x99, dontincrementhigherREFL
incrementhigherREFL:
	mov a, ReflTemp+1
	add a, #0x01
	da a
	mov ReflTemp+1, a
dontincrementhigherREFL:
	add a, #0x01
	da a
	mov ReflTemp, a
	clr a
	lcall Display_Refl
	ljmp State0_SetupRefl
	
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
	ljmp State0_SetupRefl
incrementRM:
	add a, #0x01
	da a
	mov ReflMinAlarm, a
	clr a
	lcall Display_Refl
	ljmp State0_SetupRefl


CheckStartTimer:		; if modestart buttup pressed, start timer and main loop
	jb STARTSTOP_BUTTON, State0_SetupRefl
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, jumpercst 
    jnb STARTSTOP_BUTTON, $
   
   	setb TR1			;Start Timer
	
	mov BCD_counterMin, SoakMinAlarm	; move time settings into counters
	mov BCD_counterSec, SoakSecAlarm
	
	clr refltimer_done; clear timer done flags
	
	;------------------------- TODO ----------------------------;
	; Voice Feedback Soak stage
	; Set oven to Soak heat
	;-----------------------------------------------------------;
	
	ljmp Forever
jumpercst:
	ljmp State0_SetupRefl

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
	ljmp State0_SetupRefl
incrementRS:
	add a, #0x01
	da a
	mov ReflSecAlarm, a
	clr a
	lcall Display_Refl
	ljmp State0_SetupRefl
   
;--------------------------------;
;		STATE1 RAMP SOAK 	     ;
;--------------------------------; 
;State1_RampSoak:
;	jb MODE_BUTTON, SwitchDisplay_S1		; if stop button not pressed, go loop and check for 00
;    Wait_Milli_seconds(#50)
;    jb MODE_BUTTON, SwitchDisplay_S1
;    jnb MODE_BUTTON, $
    
;	jb tempdisplay_flag, TimerDisplayJmp
;	jnb tempdisplay_flag, TempDisplayJmp
	
;	ljmp State1_RampSoak
;SwitchDisplay_S1:
	;------------------------- TODO ----------------------------;
	; Check current temperature
	;-----------------------------------------------------------;
;	mov Power, #1100100B	;power at 100%
;	clr c
;	mov a, currentTemp
;	cjne a, SoakTemp, NOT_EQL_soak	; check if equal to set soak temp, if so, proceed to next state
; compare if greater or equal, proceed
;EQL_soak:
;	ljmp Forever
;NOT_EQL_soak:
;	jc A_LESS_soak
;A_GREATER_soak:
;	ljmp Forever
;A_LESS_soak:
;	ljmp State1_RampSoak

	;------------------------- TODO -------------------------------;
	; Implement safety feature (if Temp < 50C in first 60s, abort) ;
	;--------------------------------------------------------------;
    

;----------------------;
;       JMP FUNCS      ;
;----------------------;
TempDisplayJmp:
	ljmp TempDisplay
TimerDisplayJmp:
	ljmp TimerDisplay
    
;------------------------------------;
;		 STATE2&4 MAIN LOOP   		 ;
;------------------------------------;

; forever loop interface with putty
Forever:
	;------------------------- TODO ----------------------------;
	; Check Temperature
	;-----------------------------------------------------------;
	mov Power, #0x20	;power at 20% for Soak and Refl Stages 2&4
	
	jnb seconds_flag, CheckButtons
	; One second has passed, refresh the LCD with new time
	
	jb timer_done, TimerDone		;check if timer done
	clr seconds_flag
	jb tempdisplay_flag, TempDisplayJmp	; if temp mode button pressed, show temp display
	ljmp WriteNum 

	; Do this forever
	sjmp CheckButtons

CheckButtons:
	; TIME CHECK
	jb BOOT_BUTTON, CheckStop  ; buttons to change screen to Clock and Current Temp later
	Wait_Milli_Seconds(#50)
	jb BOOT_BUTTON, CheckStop
	jnb BOOT_BUTTON, $
	
	clr TR1                 ; Stop timer 2
	clr a
	mov BCD_counterSec, #0x00
	mov BCD_counterMin, #0x00
	lcall Display_Soak
	lcall Display_Refl
	
	ljmp State0_SetupSoak

; add another button for display that will loop to loop_a after
CheckStop:
    jb STARTSTOP_BUTTON, SwitchDisplays		; if stop button not pressed, go loop and display
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, SwitchDisplays
    jnb STARTSTOP_BUTTON, $
    clr TR1                 ; Stop timer 2
    	
	;------------------------- TODO ----------------------------;
	; Turn off power oven
	;-----------------------------------------------------------;	
	ljmp State0_SetupSoak		; if stop button pressed, go back to setup

SwitchDisplays:
	jb MODE_BUTTON, Forever		; if stop button not pressed, go loop and check for 00
    Wait_Milli_seconds(#50)
    jb MODE_BUTTON, Forever
    jnb MODE_BUTTON, $
	
	jb tempdisplay_flag, TimerDisplayJmp
	jnb tempdisplay_flag, TempDisplayJmp
	ljmp Forever

TimerDone:		; if timer done
	jnb refltimer_done, StartReflTimer		; if reflow timer not done, start reflow timer
	;else if refltimer done, finish process
	clr TR1                 ; Stop timer 2
	clr a
	
	; reset all settings
	mov SoakTemp, #0x00
   	mov ReflTemp, #0x00
   	mov SoakTemp+1, #0x00
   	mov ReflTemp+1, #0x00
	mov BCD_counterSec, #0x00
	mov BCD_counterMin, #0x00
	mov SoakMinAlarm, #0x00
	mov SoakSecAlarm, #0x00
	mov ReflMinAlarm, #0x00
	mov ReflSecAlarm, #0x00
	
	lcall Display_Soak
	lcall Display_Refl
	
	;------------------------- TODO ----------------------------;
	; Turn off oven temp
	;-----------------------------------------------------------;
	
	ljmp State0_SetupSoak		; go back to settings

;--------------------------------;
;		STATE3 RAMP REFL 	     ;
;--------------------------------;
;State3_RampRefl:
;	jb MODE_BUTTON, SwitchDisplay_S3		; if stop button not pressed, go loop and check for 00
;    Wait_Milli_seconds(#50)
;    jb MODE_BUTTON, SwitchDisplay_S3
;    jnb MODE_BUTTON, $
    
;	jb tempdisplay_flag, TimerDisplayJmp
;	jnb tempdisplay_flag, TempDisplayJmp
	
;	ljmp State3_RampRefl

;SwitchDisplay_S3:
	;------------------------- TODO ----------------------------;
	; Check current temperature
	;-----------------------------------------------------------;
;	mov Power, #1100100B	;power at 100%
;	clr c
;	mov a, currentTemp
;	cjne a, ReflTemp, NOT_EQL_refl	; check if equal to set soak temp, if so, proceed to next state

	; compare if greater or equal, proceed
;EQL_refl:
;	ljmp StartReflTimer
;NOT_EQL_refl:
;	jc A_LESS_refl
;A_GREATER_refl:
;	ljmp StartReflTimer
;A_LESS_refl:
;	ljmp State3_RampRefl
	
;---------------------------;
;		STATE4 REFL 	    ;
;---------------------------; 

StartReflTimer:
	clr timer_done
	setb refltimer_done		; set to indicate final stage in process
	mov BCD_counterMin, ReflMinAlarm
	mov BCD_counterSec, ReflSecAlarm
	
	;------------------------- TODO --------------------------------------;
	; Change oven temperature to Reflow
	;---------------------------------------------------------------------;
	
	ljmp Forever

END