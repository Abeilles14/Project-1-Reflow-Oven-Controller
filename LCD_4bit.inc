cseg

; When using a 22.1184MHz crystal in fast mode
; one cycle takes 1.0/22.1184MHz = 45.21123 ns

;---------------------------------;
; Wait 40 microseconds            ;
;---------------------------------;
Wait40uSec:
	push AR0
	mov R0, #177
L0: 
	nop
	nop
	djnz R0, L0 ; 1+1+3 cycles->5*45.21123ns*177=40us
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
L3: mov R1, #45
L2: mov R0, #166
L1: djnz R0, L1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, L2 ; 22.51519us*45=1.013ms
    djnz R2, L3 ; number of millisecons to wait passed in R2
    pop AR1
    pop AR0
    ret

;---------------------------------;
; 	  Wait HalfSecond         ;
;---------------------------------;

WaitHalfSec:
    mov R2, #89
L6: mov R1, #250
L5: mov R0, #166
L4: djnz R0, L6 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, L5 ; 22.51519us*250=5.629ms
    djnz R2, L4 ; 5.629ms*89=0.5s (approximately)
    ret
	
;---------------------------------;
; Toggles the 'E' pin in the LCD  ;
;---------------------------------;
LCD_pulse:
	setb LCD_E
	lcall Wait40uSec
	clr LCD_E
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
    lcall Wait40uSec
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
	; NOTE: the preprogrammed power-on delay of 16 ms on the AT89LP52
	; seems to be enough.  That is why this line is commented out.
	; Wait_Milli_Seconds(#40)
	
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

;------------------------------------;
;   		Displays	     ;
;------------------------------------;
;--------- show current temperature ----------;
TempDisplay:


;--------- show current soak/refl process ---------;
TimerDisplay:

;----------------------------------------;
;   	Checking Equal or greater	 ;
;----------------------------------------;
;------------- Soak Temp ----------------;
Compare_SoakTemp_GE mac
	mov a, %0		; compare current temp with SoakTemp
	lcall ?Compare_GE mac
endmac

?Compare_SoakTemp_GE:
	clr c
	cjne a, SoakTemp, NOT_EQ	; current temp and soak temp
EQL:				; proceed
	sjmp END_CMP

NOT_EQ:			;if not equal, check if less
	jc A_LESS

A_GREATER:		; proceed
	sjmp END_CMP

A_LESS:		; loop
	ret

END_CMP:	;Finished
	ljmp Forever
	
;------------- Refl Temp ----------------;

;------------------------------------;
;   	Temperature Stuff	     ;
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
	; lcall TempAlarm

SendCelcius:
	; convert back to celcius, since bcd is in kelvin
	lcall bcd2hex
	load_y(273)
	lcall sub32
	lcall hex2bcd
	Display_BCD(bcd)
	ret


















 
	