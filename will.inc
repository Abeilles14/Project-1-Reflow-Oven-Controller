$NOLIST


CSEG
Wait10us:
	mov R0,#18
	djnz R0,$
	ret;



	
Read_ADC_Channel MAC
mov b, #%0
lcall ?Read_ADC_Channel
ENDMAC

?Read_ADC_Channel:
clr CE_ADC
mov R0, #00000001B ; Start bit:1
lcall DO_SPI_G
mov a, b
swap a
anl a, #0F0H
setb acc.7 ; Single mode (bit 7).
mov R0,a
lcall DO_SPI_G
mov a, R1 ; R1 contains bits 8 and 9
anl a, #00000011B ; We need only the two least significant bits
mov R7, a ; Save result high.
mov R0, #55H ; It doesn't matter what we transmit...
lcall DO_SPI_G
mov a, R1 ; 
mov R6, a ; R1 contains bits 0 to 7. Save result low.
setb CE_ADC
ret

Average_Channel1:
	load_x(0)
	mov R5,#100
Sum_Loop1:
	mov y+3,#0
	mov y+2,#0
	mov y+1,#0
	Read_ADC_Channel(1)
	mov y+0,R6
	lcall add32
	lcall Wait10us
	djnz R5, Sum_Loop1
	load_y(100)
	lcall div32
	ret

INIT_SPI:
 setb MY_MISO ; Make MISO an input pin
 clr MY_SCLK ; For mode (0,0) SCLK is zero
 setb CE_ADC
setb CE_EE
clr CE_RTC ; RTC CE is active high
 ret

 
GeString:
	mov R0, #buffer
	mov R1, #15D
GSLoop:
	lcall getchar
	push acc
	clr C
	subb a, #10H
	pop acc
	jc GSDone
	MOV @R0, A
	inc R0
	djnz R1, GSSkip
mov a, #0xFF 
mov buffer+0,a
mov buffer+1,a
mov buffer+2,a
mov buffer+3,a
mov buffer+4,a
	
	sjmp GSDone
GSSkip:
	SJMP GSLoop
GSDone:
	clr a
	mov @R0, a
	ret

getchar:
	jnb RI, getchar
	clr RI
	mov a, SBUF
	ret

;---------------------------------;
; Send a BCD number to PuTTY      ;
;---------------------------------;
Send_BCD mac
	push ar0
	mov r0, %0
	lcall ?Send_BCD
	pop ar0
endmac

?Send_BCD:
	push acc
	; Write most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	; write least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	pop acc
	ret

Wait_Sec:

	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
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
 
New_Line:
    DB   '\n', 0

$LIST
