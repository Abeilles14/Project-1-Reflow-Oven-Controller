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
buffer: ds 30

; THERMOCOUPLE
LM_Result: ds 2
TC_Result: ds 2
Result: ds 2
LM_TEMP: ds 2
; TEMPERATURE
SaveT: ds 4
currentTemp: ds 2	; current temperature from sensor
SoakTemp: ds 2		; set soak temperature
ReflTemp: ds 2		; set refl temperature
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
;POWER
pwm_on:	   	ds 2 ;Oven controller
pwm_off:   	ds 2

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
$include(LCD_4bit_LPC9351.inc)
$include(math32.inc)
$include(voice_feedback.asm)
;$include (reflproc_FSM.asm)
$include(will.inc)
$include(tempcheck.inc)
$include(Jesus_stuff.inc)
$LIST

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
    
    lcall InitSerialPort
    lcall Ports_Init ; Default all pins as bidirectional I/O. See Table 42.
    lcall LCD_4BIT
    lcall Double_Clk
	lcall InitADC0 ; Call after 'Ports_Init'
	lcall CCU_Init	; voice feedback interrupt
	lcall Timer1_Init
	
	; set/clear interrupts
	clr TR1
	clr TMOD20 ; Stop CCU timer
	clr SOUND ; Turn speaker off
	clr T2S_FSM_Start
	setb EA ; Enable global interrupts.

	; initialize vars
	;mov T2S_FSM_state, #0
    mov SoakTemp, #0x00
   	mov ReflTemp, #0x00
	mov BCD_counterSec, #0x00
	mov BCD_counterMin, #0x00
	mov SoakMinAlarm, #0x00
	mov SoakSecAlarm, #0x00
	mov ReflMinAlarm, #0x00
	mov ReflSecAlarm, #0x00
	mov seconds, #0x00
	mov minutes, #0x00
   	
   	mov pwm_on, #0
    mov pwm_on+1, #0
   	
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
    jb STARTSTOP_BUTTON, State0_SetupRefl
    jnb STARTSTOP_BUTTON, $

	;------------------------- TODO ----------------------------;
	; Voice Feedback Soak stage
	; Set oven to Soak heat
	;-----------------------------------------------------------;
		
	; temp stuff, clear bits
	clr a
	mov x+1,a
	mov x+2,a
	mov x+3,a

	
	;------------------------- TODO ----------------------------;
	; Change display to ramp soak?
	;-----------------------------------------------------------;
	
	ljmp State1_RampSoak

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
 ; 100% power
    mov pwm_on+1, #high(350) ;PWM of 100 is a diagonal line on the diagram (increasing temp)
	mov pwm_on+0, #low(350) ; goes on at 0

	lcall ReadTemp
	jb MODE_BUTTON, SwitchDisplay_S1		; if stop button not pressed, go loop and check for 00
    Wait_Milli_seconds(#50)
    jb MODE_BUTTON, SwitchDisplay_S1
    jnb MODE_BUTTON, $
    
	jb tempdisplay_flag, TimerDisplayJmp2
	jnb tempdisplay_flag, TempDisplayJmp2
	
SwitchDisplay_S1:
	lcall ReadTemp
	mov Power, #1100100B	;power at 100%

; Compare upper byte
CompareUpperB_S1:
	mov a, SoakTemp+1
	clr c
	subb a, Result+1	;Soak-Temp
	jnc CompareLowerB_S1		; if SoakTemp>Result UB, check LB, else end state
	ljmp End_S1
CompareLowerB_S1:
	mov a, SoakTemp+0
	clr c
	subb a, Result+0
	jnc State1_RampSoak ; if SoakTemp<Result LB, loop, else end state
; If Soak Temp reached, proceed
End_S1:
	mov BCD_counterMin, SoakMinAlarm	; move time settings into counters
	mov BCD_counterSec, SoakSecAlarm
	clr timer_done
	clr refltimer_done; clear timer done flags
	setb TR1			;Start Timer
	ljmp Forever
	;------------------------- TODO -------------------------------;
	; Implement safety feature (if Temp < 50C in first 60s, abort) ;
	;--------------------------------------------------------------;

;----------------------;
;       JMP FUNCS      ;
;----------------------;
TempDisplayJmp2:
	ljmp TempDisplayJmp
TimerDisplayJmp2:
	ljmp TimerDisplayJmp
	
;------------------------------------;
;		 STATE2&4 MAIN LOOP   		 ;
;------------------------------------;

; forever loop interface with putty
Forever:
	 ; 20% pwm for soak and refl
    mov pwm_on+1, #high(775) ;PWM of 20 is a horizontal line on the diagram (const temp)
	mov pwm_on, #low(775)
	
	; check temperature
	lcall ReadTemp	

	;------------------------- TODO ----------------------------;
	; Check Temperature
;	 lcall ReadTemp
;	 mov currentTemp, Result
	;-----------------------------------------------------------;
	; Voice Feedback
	;lcall T2S_FSM		; Run the state machine that plays minutes:seconds

	mov Power, #0x20	;power at 20% for Soak and Refl Stages 2&4
	
	jnb seconds_flag, CheckButtons
	; One second has passed, refresh the LCD with new time
	
	jb timer_done, TimerDoneJmp		;check if timer done
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


;----------------------;
;       JMP FUNCS      ;
;----------------------;
TempDisplayJmp:
	ljmp TempDisplay
TimerDisplayJmp:
	ljmp TimerDisplay
ForeverJmp:
	ljmp Forever
TimerDoneJmp:
	ljmp TimerDone
	
; add another button for display that will loop to loop_a after
CheckStop:
    jb STARTSTOP_BUTTON, VoiceFeedback		; if stop button not pressed, go loop and display
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, VoiceFeedback
    jnb STARTSTOP_BUTTON, $
    clr TR1                 ; Stop timer 2
    	
	;------------------------- TODO ----------------------------;
	; Turn off power oven
	;-----------------------------------------------------------;	
	ljmp State0_SetupSoak		; if stop button pressed, go back to setup
		
SwitchDisplays:
	jb MODE_BUTTON, ForeverJmp		; if stop button not pressed, go loop and check for 00
    Wait_Milli_seconds(#50)
    jb MODE_BUTTON, ForeverJmp
    jnb MODE_BUTTON, $
	
	jb tempdisplay_flag, TimerDisplayJmp
	jnb tempdisplay_flag, TempDisplayJmp
	ljmp Forever

VoiceFeedback:	
	; Voice Feedback
	jb TEMP_BUTTON, SwitchDisplays		; if stop button not pressed, go loop and display
    Wait_Milli_seconds(#50)
    jb TEMP_BUTTON, SwitchDisplays

    mov seconds, BCD_counterSec
    mov minutes, BCD_counterMin
	setb T2S_FSM_Start	; This plays the current minutes:seconds by making the state machine get out of state zero.
	ljmp Forever

TimerDone:		; if timer done
	clr TR1                 ; Stop timer 2
	clr a
	
	jnb refltimer_done, State3_RampRefl		; if reflow timer not done, start reflow timer
	;else if refltimer done, finish process

	ljmp State5_Cool		; go to Cool state
;----------------------;
;       JMP FUNCS      ;
;----------------------;
TempDisplayJmp3:
	ljmp TempDisplay
TimerDisplayJmp3:
	ljmp TimerDisplay
	
;--------------------------------;
;		STATE3 RAMP REFL 	     ;
;--------------------------------;
State3_RampRefl:
 	; 100% power
    mov pwm_on+1, #high(350) ;PWM of 100 is a diagonal line on the diagram (increasing temp)
	mov pwm_on+0, #low(350) ; goes on at 0
	
	lcall ReadTemp
	
	jb MODE_BUTTON, SwitchDisplay_S3		; if stop button not pressed, go loop and check for 00
    Wait_Milli_seconds(#50)
    jb MODE_BUTTON, SwitchDisplay_S3
    jnb MODE_BUTTON, $
    
	jb tempdisplay_flag, TimerDisplayJmp3
	jnb tempdisplay_flag, TempDisplayJmp3
	
SwitchDisplay_S3:
	lcall ReadTemp
	mov Power, #1100100B	;power at 100%

; Compare upper byte
CompareUpperB_S3:
	mov a, ReflTemp+1
	clr c
	subb a, Result+1	;Soak-Temp
	jnc CompareLowerB_S3		; if SoakTemp>Result UB, check LB, else end state
	ljmp End_S3
CompareLowerB_S3:
	mov a, ReflTemp+0
	clr c
	subb a, Result+0
	jnc State3_RampRefl ; if SoakTemp<Result LB, loop, else end state
; If Soak Temp reached, proceed
;---------------------------;
;		STATE4 REFL 	    ;
;---------------------------; 
End_S3:
	clr timer_done
	setb refltimer_done		; set to indicate final stage in process
	mov BCD_counterMin, ReflMinAlarm	; move time settings into counters
	mov BCD_counterSec, ReflSecAlarm
	clr timer_done
	setb refltimer_done; clear timer done flags
	setb TR1			;Start Timer
	ljmp Forever
	
;---------------------------;
;		STATE5 COOLING 	    ;
;---------------------------; 
State5_Cool:
;	pwn 0%
	 mov pwm_on+0, #low(1000)
	 mov pwm_on+1, #high(1000)

	lcall ReadTemp
	
	jb MODE_BUTTON, SwitchDisplay_S5		; if stop button not pressed, go loop and check for 00
    Wait_Milli_seconds(#50)
    jb MODE_BUTTON, SwitchDisplay_S5
    jnb MODE_BUTTON, $
    
	jb tempdisplay_flag, TimerDisplayJmp3
	jnb tempdisplay_flag, TempDisplayJmp3
	
SwitchDisplay_S5:
	lcall ReadTemp
	mov Power, #0x00	;power at 0%
; Compare upper byte
CompareUpperB_S5:
	mov a, Result+1
	clr c
	subb a, #0x00	;Soak-Temp
	jnc CompareLowerB_S5		; if SoakTemp>Result UB, check LB, else end state
	ljmp End_S5
CompareLowerB_S5:
	mov a, Result+0		;change to 0x60 later
	clr c
	subb a, #0x24
	jnc State5_Cool ; if SoakTemp<Result LB, loop, else end state
; If Cooling temp reached, proceed
;---------------------------;
;		STATE4 REFL 	    ;
;---------------------------; 
End_S5:
	clr TR1
	clr timer_done
	clr refltimer_done		; set to indicate final stage in process
	;reset all settings
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