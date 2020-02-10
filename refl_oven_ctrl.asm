$NOLIST
$MOD9351
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
CE_EE EQU P2.4
CE_RTC EQU P2.5

BOOT_BUTTON equ P4.5
SOUND_OUT equ P3.7

TEMP_BUTTON  equ P0.0		; Inc temperature
ALMIN_BUTTON  equ P0.3		; Inc minutes
ALSEC_BUTTON   equ P0.6		; Inc seconds

STARTSTOP_BUTTON equ P2.7	; Start/Stop process immediately, Settings
MODE_BUTTON equ P2.4				; Switch Displays between Clock, Current Temp, Settings/timer

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
w: 	 ds 3
x:   ds 4
y:   ds 4
bcd: ds 5
buffer: ds 30

; THERMOCOUPLE&SENSOR
LM_Result: ds 2
TC_Result: ds 1
temp:ds 2
LM_TEMP: ds 2

; TEMPERATURE
SaveT: ds 4
currentTemp: ds 2	; current temperature from sensor
SoakTemp: ds 2		; set soak temperature
ReflTemp: ds 2		; set refl temperature
; TIMER COUNTERS	; contains counters and timers
Count1ms: ds 2 		; Used to determine when (1) second has passed
Count5ms: ds 1
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
tempdisplay_flag: dbit 1	; Set to 1 for temp and run time display

CSEG
; These 'equ' must match the wiring between the microcontroller and the LCD!
					;OLD PIN CONFIG FOR AT89LP:
LCD_RS equ P0.5		;P1.1
LCD_RW equ P0.6		;P1.2
LCD_E  equ P0.7		;P1.3
LCD_D4 equ P1.2		;P3.2
LCD_D5 equ P1.3		;P3.3
LCD_D6 equ P1.4		;P3.4
LCD_D7 equ P1.6		;P3.5

; LCD and Putty Strings
_Hello_World: DB 'Hello World!', '\r', '\n',0
_New_Line: DB '\r\n', 0
_Soak: DB 'Soak:',0
_Refl: DB 'Refl:',0
_Temperature_LCD: DB 'Temp:',0	
_C:	DB '000C',0
_blank: DB ' ',0
_default: DB '00:00',0
_clearLCD: DB '                '

$NOLIST
$include(math32.inc)
$include(LCD_4bit_LPC9351.inc)
;$include(LCD_4bit.inc)
;$include (reflproc_FSM.asm)
;$include(will.inc)
;$include(tempcheck.inc)
$LIST

;---------------------------------;
; Initial configuration of ports. ;
; After reset the default for the ;
; pins is 'Open Drain'.  This     ;
; routine changes them pins to    ;
; Quasi-bidirectional like in the ;
; original 8051.                  ;
; Notice that P1.2 and P1.3 are   ;
; always 'Open Drain'. If those   ;
; pins are to be used as output   ;
; they need a pull-up resistor.   ;
;---------------------------------;
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
	clr tempdisplay_flag
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
	add a, #0x01
	da a
	mov SoakTemp, a
	clr a
	lcall Display_Soak
	;lcall Display_Refl
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
		
CheckStartTimer:		; if modestart buttup pressed, start timer and main loop
	jb STARTSTOP_BUTTON, State0_SetupRefl
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, State0_SetupRefl
    jnb STARTSTOP_BUTTON, $
    
	setb half_seconds_flag		; pressed to exit settings and start timer
	setb TR2
	
	mov BCD_counterMin, SoakMinAlarm	; move time settings into counters
	mov BCD_counterSec, SoakSecAlarm
	
	clr refltimer_done; clear timer done flags to indicate Soak stage
	
	;------------------------- TODO ----------------------------;
	; Voice Feedback Soak stage
	; Set oven to Soak heat
	;-----------------------------------------------------------;
	
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
State1_RampSoak:
	;------------------------- TODO ----------------------------;
	; Check current temperature
	;-----------------------------------------------------------;
	clr c
	mov a, currentTemp
	cjne a, SoakTemp, NOT_EQL_soak	; check if equal to set soak temp, if so, proceed to next state
; compare if greater or equal, proceed
EQL_soak:
	ljmp Forever
NOT_EQL_soak:
	jc A_LESS_soak
A_GREATER_soak:
	ljmp Forever
A_LESS_soak:
	ljmp State1_RampSoak

	;------------------------- TODO -------------------------------;
	; Implement safety feature (if Temp < 50C in first 60s, abort) ;
	;--------------------------------------------------------------;
 
;------------------------------------;
;   	STATE2&4 SOAK AND REFLOW   	 ;
;------------------------------------;
; forever loop interface with putty
Forever:
	
	;------------------------- TODO ----------------------------;
	; Check Temperature
	;-----------------------------------------------------------;

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
	
	;switch displays instead of loop_a
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
	
	;------------------------- TODO ----------------------------;
	; Turn off oven
	; Voice feed back Turn off oven
	;-----------------------------------------------------------;	
	ljmp State0_SetupSoak		; if stop button pressed, go back to setup
	
;SwitchDisplays:
;	jb MODE_BUTTON, loop_a		; if stop button not pressed, go loop and check for 00
;    Wait_Milli_seconds(#50)
;    jb MODE_BUTTON, loop_a
;    jnb MODE_BUTTON, $
	
;	jb tempdisplay_flag, TimerDisplay
;	jnb tempdisplay_flag, TempDisplay

	ljmp loop_a
	
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
	; reset all settings
	mov SoakTemp, #0x00
   	mov ReflTemp, #0x00
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
	; Voice feedback Reflow process over
	;-----------------------------------------------------------;
	ljmp State0_SetupSoak		; go back to settings

;--------------------------------;
;		STATE3 RAMP REFL 	     ;
;--------------------------------; 
State3_RampRefl:
	;------------------------- TODO ----------------------------;
	; Check current temperature
	;-----------------------------------------------------------;
	clr c
	mov a, currentTemp
	cjne a, ReflTemp, NOT_EQL_refl	; check if equal to set soak temp, if so, proceed to next state

	; compare if greater or equal, proceed
EQL_refl:
	ljmp StartReflTimer
NOT_EQL_refl:
	jc A_LESS_refl
A_GREATER_refl:
	ljmp StartReflTimer
A_LESS_refl:
	ljmp State3_RampRefl
	
;---------------------------;
;		STATE4 REFL 	    ;
;---------------------------; 
StartReflTimer:
	setb refltimer_done			; set to indicate final stage in process
	mov BCD_counterMin, ReflMinAlarm
	mov BCD_counterSec, ReflSecAlarm
	
	;------------------------- TODO --------------------------------------;
	; Change oven temperature to Reflow
	; Voice feedback Soak stage over, start Reflow
	;---------------------------------------------------------------------;
	
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
	
;---------------------------;
;		STATE5 COOLING 	    ;
;---------------------------; 
State5_Cool:
	;------------------------- TODO ----------------------------;
	; Check current temperature
	;-----------------------------------------------------------;
	clr c
	mov a, currentTemp
	cjne a, #60 , NOT_EQL_cool	; check if equal to set soak temp, if so, proceed to next state

	; compare if greater or equal, proceed
EQL_cool:
	ljmp State5_Cool
NOT_EQL_cool:
	jc A_LESS_cool
A_GREATER_cool:
	ljmp State5_Cool
A_LESS_cool:
	; reset all settings
	mov SoakTemp, #0x00
   	mov ReflTemp, #0x00
	mov BCD_counterSec, #0x00
	mov BCD_counterMin, #0x00
	mov SoakMinAlarm, #0x00
	mov SoakSecAlarm, #0x00
	mov ReflMinAlarm, #0x00
	mov ReflSecAlarm, #0x00
	
	lcall Display_Soak
	lcall Display_Refl
	
	ljmp State0_SetupSoak	
	
END
