cseg

;---------------------------------;
; Wait 'R2' microseconds          ;
;---------------------------------;
Wait_Micro_Seconds mac
	push AR2
	mov R2, %0
	lcall ?Wait_Micro_Seconds
	pop AR2
endmac

?Wait_Micro_Seconds:
	push AR0
	push AR1
L13: mov R1, #1
L12: mov R0, #2
L11: djnz R0, L11 ; 3 cycles->3*166.666ns*2
    djnz R1, L12 ; 
    djnz R2, L13 ; number of millisecons to wait passed in R2
    pop AR1
    pop AR0
    ret
