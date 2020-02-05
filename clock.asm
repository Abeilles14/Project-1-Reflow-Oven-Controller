; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P3.7
UPDOWN        equ P0.0
HOUR          equ P2.0
MINUTE        equ P2.2
ALARMH        equ P2.3
ALARMM        equ P2.1

; Reset vector
org 0x0000
    ljmp main

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

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
BCD_minute:   ds 1
BCD_hour:     ds 1
alarm_minute: ds 1
alarm_hour:   ds 1

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
one_second_flag: dbit 1 ; Set to one in the ISR every time 1000 ms had passed
am_pm: dbit 1

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    clr ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
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
	; Check if one second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb one_second_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	sjmp Timer2_ISR_da
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
; These custom characters copied from https://cdn.instructables.com/ORIG/FGY/5J1E/GYFYDR5L/FGY5J1EGYFYDR5L.txt
Custom_Characters:
	WriteCommand(#40h) ; Custom characters are stored starting at address 40h
; Custom made character 0
	WriteData(#00111B)
	WriteData(#01111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 1
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
; Custom made character 2
	WriteData(#11100B)
	WriteData(#11110B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 3
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#01111B)
	WriteData(#00111B)
; Custom made character 4
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 5
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11110B)
	WriteData(#11100B)
; Custom made character 6
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 7
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	ret

; For all the big numbers, the starting column is passed in register R1
Draw_big_0:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)  
	WriteData(#1) 
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)  
	WriteData(#4)  
	WriteData(#5)
	WriteData(#' ')
	ret
	
Draw_big_1:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#1)
	WriteData(#2)
	WriteData(#' ')
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#4)
	WriteData(#255)
	WriteData(#4)
	WriteData(#' ')
	ret

Draw_big_2:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#6)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#7)
	WriteData(#' ')
	ret

Draw_big_3:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#6)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#7)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_4:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#4)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#255)
	WriteData(#' ')
	ret

Draw_big_5:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#255)
	WriteData(#6)
	WriteData(#6)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#7)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_6:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#6)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_7:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#1)
	WriteData(#1)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#0)
	WriteData(#' ')
	ret

Draw_big_8:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_9:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#255)
	WriteData(#' ')
	ret

; The number to display is passed in accumulator.  The column where to display the
; number is passed in R1. This works only for numbers 0 to 9.
Display_big_number:
	; We need to multiply the accumulator by 3 because the jump table below uses 3 bytes
	; for each 'ljmp' instruction.
	mov b, #3
	mul ab
	mov dptr, #Jump_table
	jmp @A+dptr
Jump_table:
	ljmp Draw_big_0 ; This instruction uses 3 bytes
	ljmp Draw_big_1
	ljmp Draw_big_2
	ljmp Draw_big_3
	ljmp Draw_big_4
	ljmp Draw_big_5
	ljmp Draw_big_6
	ljmp Draw_big_7
	ljmp Draw_big_8
	ljmp Draw_big_9
; No 'ret' needed because we are counting of on the 'ret' provided by the Draw_big_x functions above

; Takes a BCD 2-digit number passed in the accumulator and displays it at position passed in R0
Display_Big_BCD:
	push acc
	; Display the most significant decimal digit
	mov b, R0
	mov R1, b
	swap a
	anl a, #0x0f
	lcall Display_big_number
	
	; Display the least significant decimal digit, which starts 4 columns to the right of the most significant digit
	mov a, R0
	add a, #3
	mov R1, a
	pop acc
	anl a, #0x0f
	lcall Display_big_number
	
	ret

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0 configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    lcall Custom_Characters ; Custom characters are needed to display big numbers.  This call generates them.
    setb one_second_flag
	mov BCD_counter, #0x50
	mov BCD_minute, #0x59
	mov BCD_hour, #0x11
	mov alarm_minute, #0x00
	mov alarm_hour, #0x00
	mov r4, #0 ;am or pm
	mov r3, #0 ;button press (setting) 
	mov r5, #0 ;am or pm for alarm

	; After initialization the program stays in this 'forever' loop
loop:

 
    cjne r3, #0, alarm_hour_change
    ljmp mode_change
    
alarm_hour_change:    
    jb ALARMH, alarm_minute_change  
	Wait_Milli_Seconds(#50)	
	jb ALARMH, alarm_minute_change  
	jnb ALARMH, $
    
    mov a, alarm_hour 
    add a, #0x01
    da a
    mov alarm_hour, a
    
    mov a, alarm_hour 
    cjne a, #0x12, hour_reset3
    cjne r5,#1, jump3
    mov r5, #0
    sjmp hour_reset3
    
jump3:
   mov r5, #1
   
hour_reset3:
    mov alarm_hour, a
    cjne a, #0x13, alarm_minute_change
    mov a, #0x01
    mov alarm_hour, a


alarm_minute_change:    
    jb ALARMM, minute_change  
	Wait_Milli_Seconds(#50)	
	jb ALARMM, minute_change  
	jnb ALARMM, $
    
    mov a, alarm_minute
    add a, #0x01
    da a
    mov alarm_minute, a
    cjne a, #0x60, minute_change
    mov a, #0x01
    mov alarm_minute, a

minute_change:    
    jb MINUTE, hour_change  
	Wait_Milli_Seconds(#50)	
	jb MINUTE, hour_change  
	jnb MINUTE, $
    
    mov a, BCD_minute ;BCD_hour++
    add a, #0x01
    da a
    mov BCD_minute, a
    cjne a, #0x60, hour_change
    mov a, #0x01
    mov BCD_minute, a
    
hour_change:
    jb HOUR, mode_change  
	Wait_Milli_Seconds(#50)	
	jb HOUR, mode_change  
	jnb HOUR, $		
	
	mov a, BCD_hour ;BCD_hour++
    add a, #0x01
    da a
    mov BCD_hour, a
    
    mov a, BCD_hour 
    cjne a, #0x12, hour_reset2
    cjne r4,#1, jump2
    mov r4, #0
    sjmp hour_reset2
    
jump2:
   mov r4, #1
   
hour_reset2:
    mov BCD_hour, a
    cjne a, #0x13, mode_change
    mov a, #0x01
    mov BCD_hour, a
    
	
mode_change:	
	jb BOOT_BUTTON, trigger  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, trigger  ; if the 'BOOT' button is not pressed skip	
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
        cjne r3,#0, leave
    
enter:
    mov r3,#1
    sjmp trigger
    
leave:
    mov r3,#0

trigger:
    mov a, BCD_hour
    mov b, alarm_hour
    cjne a, b, off
    
    mov a, BCD_minute
    mov b, alarm_minute
    cjne a, b, off
    
    mov a, r4
    mov b, r5
    cjne a, b, off
    
    setb ET0
    sjmp loop_a

off:
    clr ET0

loop_a:
	jb one_second_flag, loop_b
	ljmp loop
	 
    
loop_b:
    clr one_second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	
    mov a, BCD_counter
    cjne a, #0x60, display
    mov a, #0x00
    mov BCD_counter, a
    
    mov a, BCD_minute ;BCD_minute++
    add a, #0x01
    da a
    mov BCD_minute, a
    
    cjne a, #0x60, display ;BCD_minute reset
    mov a, #0x00
    mov BCD_minute, a
    
    mov a, BCD_hour ;BCD_hour++
    add a, #0x01
    da a
    mov BCD_hour, a
    
    
    mov a, BCD_hour 
    cjne a, #0x12, next
    cjne r4,#1, jump
    mov r4, #0

    
jump:
   mov r4, #1
    
    
next:
    mov a, BCD_hour ;BCD_hour reset
    cjne a, #0x13, display
    mov a, #0x01
    mov BCD_hour, a
      
  
    
display:
    cjne r3, #0, setting_display
    mov a, #0x8F
    lcall ?WriteCommand
    mov a, #'M'
    lcall ?WriteData   
    
normal_display:    
    Set_Cursor(2, 15)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter)
    
    mov R0, #7 ; Column where to display the big font 2-digit number
    mov a, BCD_minute ; The number to display using big font
	lcall Display_Big_BCD
	
	mov R0, #0 ; Column where to display the big font 2-digit number
    mov a, BCD_hour ; The number to display using big font
	lcall Display_Big_BCD
    
	
check_reg:
    cjne r4, #1, display_am 
    sjmp display_pm
  
  
display_am: 
    mov a, #0x8E
    lcall ?WriteCommand
    mov a, #'A'
    lcall ?WriteData
    sjmp last
    
display_pm: 
    mov a, #0x8E
    lcall ?WriteCommand
    mov a, #'P'
    lcall ?WriteData

last:    
    ljmp loop
    
    
setting_display:
	Set_Cursor(1, 1)
	Display_Char(#'C')
	
	Set_Cursor(1, 2)
	Display_Char(#'L')
	
	Set_Cursor(1, 3)
	Display_Char(#'O')
	
	Set_Cursor(1, 4)
	Display_Char(#'C')
	
	Set_Cursor(1, 5)
	Display_Char(#'K')
	
	Set_Cursor(1, 6)
	Display_Char(#' ')
	
	Set_Cursor(1, 7)     
	Display_BCD(BCD_hour)
	
	Set_Cursor(1, 9)     
	Display_BCD(BCD_minute)
	
	Set_Cursor(1, 11)
	Display_Char(#' ')
	
	Set_Cursor(1, 12)     
	Display_BCD(BCD_counter)
	
check_reg2:
    cjne r4, #1, display_am2 
    sjmp display_pm2
  
  
display_am2: 
    Set_Cursor(1, 15)
	Display_Char(#'A')
    sjmp draw_m
    
display_pm2: 
    Set_Cursor(1, 15)
	Display_Char(#'P')
	
draw_m:
	Set_Cursor(1, 16)
	Display_Char(#'M')
	
	

	
second_row:
	Set_Cursor(2, 1)
	Display_Char(#'A')
	
	Set_Cursor(2, 2)
	Display_Char(#'L')
	
	Set_Cursor(2, 3)
	Display_Char(#'A')
	
	Set_Cursor(2, 4)
	Display_Char(#'R')
	
	Set_Cursor(2, 5)
	Display_Char(#'M')
	
	Set_Cursor(2, 6)
	Display_Char(#' ')
	
	Set_Cursor(2, 7)     
	Display_BCD(alarm_hour)
	
	Set_Cursor(2, 9)     
	Display_BCD(alarm_minute)
	
	Set_Cursor(2, 11)
	Display_Char(#' ')
	
	Set_Cursor(2, 12)     
	Display_Char(#' ')
	
	Set_Cursor(2, 13)     
	Display_Char(#' ')

check_reg3:
    cjne r5, #1, display_am3
    sjmp display_pm3
  
  
display_am3: 
    Set_Cursor(2, 15)
	Display_Char(#'A')
    sjmp draw_m2
    
display_pm3: 
    Set_Cursor(2, 15)
	Display_Char(#'P')
	
draw_m2:
	Set_Cursor(2, 16)
	Display_Char(#'M')
	ljmp last
	
END
