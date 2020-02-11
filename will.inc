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
