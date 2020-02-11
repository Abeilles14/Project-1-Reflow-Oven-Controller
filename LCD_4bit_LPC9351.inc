cseg

; When using a 7.373MHz clock
; one cycle takes 2.0/7.373MHz = 271.26 ns

;---------------------------------;
; Wait 40 microseconds            ;
;---------------------------------;
Wait40uSec:
	push AR0
	mov R0, #74
L0: 
	djnz R0, L0 ; 2 cycles->2*271.26ns*74=40us
	pop AR0
    ret

;---------------------------------;
; Wait 'R2' milliseconds          ;
;---------------------------------;
Wait_Milli_Seconds mac
	push AR2
	mov R2, %0
	lcall ?Wait_Milli_Seconds
	pop AR2
endmac

?Wait_Milli_Seconds:
	push AR0
	push AR1
L3: mov R1, #10
L2: mov R0, #184
L1: djnz R0, L1 ; 2 cycles->2*271.26ns*184=100us
    djnz R1, L2 ; 100us*10=1ms
    djnz R2, L3 ; number of millisecons to wait passed in R2
    pop AR1
    pop AR0
    ret

;---------------------------------;
; Wait Half Second	          ;
;---------------------------------;

WaitHalfSec:
    mov R2, #20
L6: mov R1, #250
L5: mov R0, #184
L4: djnz R0, L4 ; 2 machine cycles-> 2*0.27126us*184=100us
    djnz R1, L5 ; 100us*250=0.025s
    djnz R2, L6 ; 0.025s*20=0.5s
    ret
	
;---------------------------------;
; Toggles the 'E' pin in the LCD  ;
;---------------------------------;
LCD_pulse:
	setb LCD_E
	lcall Wait40uSec
	clr LCD_E
    lcall Wait40uSec
    ret

;---------------------------------;
; Writes acc to LCD in 4-bit mode ;
;---------------------------------;
LCD_byte:
	; Write high 4 bits first
	mov c, ACC.7
	mov LCD_D7, c
	mov c, ACC.6
	mov LCD_D6, c
	mov c, ACC.5
	mov LCD_D5, c
	mov c, ACC.4
	mov LCD_D4, c
    lcall LCD_pulse
	; Write low 4 bits next
	mov c, ACC.3
	mov LCD_D7, c
	mov c, ACC.2
	mov LCD_D6, c
	mov c, ACC.1
	mov LCD_D5, c
	mov c, ACC.0
	mov LCD_D4, c
    lcall LCD_pulse
	ret

;---------------------------------;
; Write data to LCD               ;
;---------------------------------;
WriteData mac
	mov a, %0
	lcall ?WriteData
endmac
	
?WriteData:
	setb LCD_RS
	ljmp LCD_byte

;---------------------------------;
; Write command to LCD            ;
;---------------------------------;
WriteCommand mac
	mov a, %0
	lcall ?WriteCommand
endmac

?WriteCommand:
	clr LCD_RS
	ljmp LCD_byte

;---------------------------------;
; Configure LCD in 4-bit mode     ;
;---------------------------------;
LCD_4BIT:
	clr LCD_E   ; Resting state of LCD's enable pin is zero
	clr LCD_RW  ; We are only writing to the LCD in this program
	
	; After power on, let the LCD start up before initializing
	Wait_Milli_Seconds(#40)
	
	; First make sure the LCD is in 8-bit mode and then change to 4-bit mode
	WriteCommand(#0x33)
	WriteCommand(#0x33)
	WriteCommand(#0x32) ; change to 4-bit mode

	; Configure the LCD
	WriteCommand(#0x28)
	WriteCommand(#0x0c)
	WriteCommand(#0x01) ;  Clear screen command (takes some time)

    ;Wait for the clear screen command to finish.
	Wait_Milli_Seconds(#2)
    ret

;---------------------------------;
; Send a constant string to LCD   ;
;---------------------------------;
Send_Constant_String mac
	push dph
	push dpl
	push acc
	mov dptr, %0
	lcall ?Send_Constant_String
	pop acc
	pop dpl
	pop dph
endmac

?Send_Constant_String:
    clr a
    movc a, @a+dptr
    jz ?Send_Constant_String_Done
    lcall ?WriteData
    inc dptr
    sjmp ?Send_Constant_String
?Send_Constant_String_Done:
    ret	

;---------------------------------;
; Set LCD cursor at row, column   ;
;---------------------------------;
Set_Cursor mac
	push acc
	mov a, #%1
	dec a
	lcall ?Set_Cursor_%0 ; Select column and row
	pop acc
endmac

?Set_Cursor_2:
	orl a, #01000000B
?Set_Cursor_1:
	orl a, #10000000B
	ljmp ?WriteCommand ; Select column and row

;---------------------------------;
; Display a BCD number in the LCD ;
;---------------------------------;
Display_BCD mac
	push ar0
	mov r0, %0
	lcall ?Display_BCD
	pop ar0
endmac

?Display_BCD:
	push acc
	; Write most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall ?WriteData
	; write least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall ?WriteData
	pop acc
	ret

;------------------------------------;
; Display a char in the LCD          ;
;------------------------------------;
Display_char mac
	push acc
	mov a, %0
	lcall ?WriteData
	pop acc
endmac


;------------------------------------;
;   	  Display on LCD             ;
;------------------------------------;
Display_Soak:
	Set_Cursor(1, 7)
	Display_BCD(SoakTemp)
	Set_Cursor(1, 11)
	Display_BCD(SoakMinAlarm)
	Set_Cursor(1, 14)
	Display_BCD(SoakSecAlarm)
	ret

Display_Refl:
	Set_Cursor(2, 7)
	Display_BCD(ReflTemp)
	Set_Cursor(2, 11)
	Display_BCD(ReflMinAlarm)
	Set_Cursor(2, 14)
	Display_BCD(ReflSecAlarm)
	ret
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

WriteNum:
	;check refltimer_done if update soak or refl time
	jnb refltimer_done, Display_SoakTimer	; if in soak stage, update soak display
	jb refltimer_done, Display_ReflTimer	; if in refl stage, update refl display	
    ; jumps to Forever after display


;------------------------------------;
;   			Displays	 		 ;
;------------------------------------;
;------- show current temperature --------;
TempDisplay:
	setb tempdisplay_flag
	
	Set_Cursor(1,1)
	Send_Constant_String(#_clearLCD)
	Set_Cursor(2,1)
	Send_Constant_String(#_clearLCD)

	Set_Cursor(1,1)
	Send_Constant_String(#_Temperature_LCD)
	;Set_Cursor(1,6)
	;Display_BCD(currentTemp)
	Set_Cursor(2,1)
	Send_Constant_String(#_Power)
	Set_Cursor(2,8)
	Display_BCD(Power)

;----------- DISPLAY TEMP --------------;

	push acc
	lcall Wait_Sec
	pop acc
	
;	clr a
;	mov x+0,Result+0
;	mov x+1,Result+1
;	mov x+2,a
;	mov x+3,a
	
;PRINTS RESULT TEMPERATURE IN TOP LEFT	
	lcall hex2bcd
	push acc
	Set_cursor (1,6) ;print to the lcd
	pop acc
	lcall LCD_3BCD
	push acc
	WriteData(#'C')
	pop acc
	
		
;	clr a
;	mov x+0,LM_TEMP+0
;	mov x+1,LM_TEMP+1
;	mov x+2,a
;	mov x+3,a
	
;PRINTS TEMPERATURE FROM LM335 (ROOM TEMP) IN BOTTOM LEFT
	lcall hex2bcd
	push acc
	Set_cursor (1,12) ;print to the lcd
	pop acc
	lcall LCD_3BCD
	push acc
	WriteData(#'C')
	pop acc
	
		
;	clr a
;	mov x+0,TC_Result+0
;	mov x+1,TC_Result+1
;	mov x+2,a
;	mov x+3,a

	
;PRINTS THERMOCOUPLE TEMPERATURE ON BOTTOM RIGHT
;	lcall hex2bcd
;	push acc
;	Set_cursor (2,6) ;print to the lcd
;	pop acc
;	lcall LCD_3BCD
;	push acc
;	WriteData(#'C')
;	pop acc
	
	ljmp Forever
;-------------- show timers ---------------;
TimerDisplay:
	clr tempdisplay_flag

	Set_Cursor(1,1)
	Send_Constant_String(#_clearLCD)
	Set_Cursor(1,2)
	Send_Constant_String(#_clearLCD)

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

	lcall Display_Soak
	lcall Display_Refl
	
	ljmp Forever

	;ret