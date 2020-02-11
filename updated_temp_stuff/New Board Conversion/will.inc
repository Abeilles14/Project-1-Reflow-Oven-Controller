$NOLIST


CSEG
Wait10us:
	mov R0,#18
	djnz R0,$
	ret;
		

Average_AD0DAT3 :
Load_x(0)
mov R5, #100
Sum_loop0:
mov y+3, #0
mov y+2, #0
mov y+1, #0
mov y+0, AD0DAT3
lcall add32
lcall Wait10us
djnz R5, Sum_loop0
load_y(100)
lcall div32
ret

;INIT_SPI_w:
 ;setb MY_MISO ; Make MISO an input pin
 ;clr MY_SCLK ; For mode (0,0) SCLK is zero
 ;setb CE_ADC
 ;setb CE_EE
 ;clr CE_RTC ; RTC CE is active high
; ret



;R0 carries data to transmit, on return R1 holds recieved data
;DO_SPI_G_w:		
 ;push acc
 ;mov R1, #0 ; Received byte stored in R1
 ;mov R2, #8 ; Loop counter (8-bits)
;DO_SPI_G_LOOP:
; mov a, R0 ; Byte to write is in R0
; rlc a ; Carry flag has bit to write
; mov R0, a
; mov MY_MOSI, c
; setb MY_SCLK ; Transmit
; mov c, MY_MISO ; Read received bit
; mov a, R1 ; Save received bit in R1
; rlc a
 ;mov R1, a
 ;clr MY_SCLK
 ;djnz R2, DO_SPI_G_LOOP
 ;pop acc
 ;ret
 
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

getchar_w:
	jnb RI, getchar_w
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
;InitSerialPort_w:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
   ; mov R1, #222
   ; mov R0, #166
   ; djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
   ; djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	;orl	PCON,#0x80
	;mov	SCON,#0x52
	;mov	BDRCON,#0x00
	;mov	BRL,#BRG_VAL
	;mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
   ; ret

; Send a character using the serial port
putchar_w:
    jnb TI, putchar_w
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
