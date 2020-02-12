dseg at 30H
minutes:       ds 1
seconds:       ds 1
T2S_FSM_state: ds 1

BSEG
T2S_FSM_start: dbit 1

CSEG
;---------------------------------;
; Routine to initialize the CCU.  ;
; We are using the CCU timer in a ;
; manner similar to the timer 2   ;
; available in other 8051s        ;
;---------------------------------;
CCU_Init:
	mov TH2, #high(CCU_RELOAD)
	mov TL2, #low(CCU_RELOAD)
	mov TOR2H, #high(CCU_RELOAD)
	mov TOR2L, #low(CCU_RELOAD)
	mov TCR21, #10000000b ; Latch the reload value
	mov TICR2, #10000000b ; Enable CCU Timer Overflow Interrupt
	setb ECCU ; Enable CCU interrupt
	setb TMOD20 ; Start CCU timer
	ret

;---------------------------------;
; ISR for CCU.  Used to playback  ;
; the WAV file stored in the SPI  ;
; flash memory.                   ;
;---------------------------------;
CCU_ISR:
	mov TIFR2, #0 ; Clear CCU Timer Overflow Interrupt Flag bit. Actually, it clears all the bits!
	
	; The registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Check if the play counter is zero.  If so, stop playing sound.
	mov a, w+0
	orl a, w+1
	orl a, w+2
	jz stop_playing
	
	; Decrement play counter 'w'.  In this implementation 'w' is a 24-bit counter.
	mov a, #0xff
	dec w+0
	cjne a, w+0, keep_playing
	dec w+1
	cjne a, w+1, keep_playing
	dec w+2
	
keep_playing:

	lcall Send_SPI ; Read the next byte from the SPI Flash...
	mov AD1DAT3, a ; and send it to the DAC
	
	sjmp CCU_ISR_Done

stop_playing:
	clr TMOD20 ; Stop CCU timer
	setb FLASH_CE  ; Disable SPI Flash
	clr SOUND ; Turn speaker off

CCU_ISR_Done:	
	pop psw
	pop acc
	reti



; Sounds we need in the SPI flash: 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 30; 40; 50; minutes; seconds;
; Approximate index of sounds in file 'stop_watch.wav'
; This was generated using: computer_sender -Asw_index.asm -S2000 stop_watch.wav
sound_index:
    db 0x00, 0x00, 0x2d ; 0 
    db 0x00, 0x31, 0x07 ; 1 
    db 0x00, 0x70, 0x07 ; 2 
    db 0x00, 0xad, 0xb9 ; 3 
    db 0x00, 0xf2, 0x66 ; 4 
    db 0x01, 0x35, 0xd5 ; 5 
    db 0x01, 0x7d, 0x33 ; 6 
    db 0x01, 0xc7, 0x61 ; 7 
    db 0x02, 0x12, 0x79 ; 8 
    db 0x02, 0x49, 0xc1 ; 9 
    db 0x02, 0x8f, 0x7a ; 10 
    db 0x02, 0xd0, 0x63 ; 11 
    db 0x03, 0x1b, 0x87 ; 12 
    db 0x03, 0x63, 0x0e ; 13 
    db 0x03, 0xb9, 0x5f ; 14 
    db 0x04, 0x11, 0x3a ; 15 
    db 0x04, 0x66, 0xc4 ; 16 
    db 0x04, 0xc0, 0x12 ; 17 
    db 0x05, 0x26, 0x98 ; 18 
    db 0x05, 0x74, 0xe9 ; 19 
    db 0x05, 0xd2, 0x8e ; 20 
    db 0x06, 0x1d, 0x83 ; 21 -> 30 
    db 0x06, 0x63, 0x42 ; 22 -> 40 
    db 0x06, 0xaa, 0xb9 ; 23 -> 50 
    db 0x06, 0xf3, 0xd6 ; 24 -> Minutes 
    db 0x07, 0x3f, 0x02 ; 25 -> Seconds 

; Size of each sound in 'sound_index'
; Generated using: computer_sender -Asw_index.asm -S2000 stop_watch.wav
Size_Length:
    db 0x00, 0x30, 0xda ; 0 
    db 0x00, 0x3f, 0x00 ; 1 
    db 0x00, 0x3d, 0xb2 ; 2 
    db 0x00, 0x44, 0xad ; 3 
    db 0x00, 0x43, 0x6f ; 4 
    db 0x00, 0x47, 0x5e ; 5 
    db 0x00, 0x4a, 0x2e ; 6 
    db 0x00, 0x4b, 0x18 ; 7 
    db 0x00, 0x37, 0x48 ; 8 
    db 0x00, 0x45, 0xb9 ; 9 
    db 0x00, 0x40, 0xe9 ; 10 
    db 0x00, 0x4b, 0x24 ; 11 
    db 0x00, 0x47, 0x87 ; 12 
    db 0x00, 0x56, 0x51 ; 13 
    db 0x00, 0x57, 0xdb ; 14 
    db 0x00, 0x55, 0x8a ; 15 
    db 0x00, 0x59, 0x4e ; 16 
    db 0x00, 0x66, 0x86 ; 17 
    db 0x00, 0x4e, 0x51 ; 18 
    db 0x00, 0x5d, 0xa5 ; 19 
    db 0x00, 0x4a, 0xf5 ; 20 
    db 0x00, 0x45, 0xbf ; 21 -> 30
    db 0x00, 0x47, 0x77 ; 22 -> 40
    db 0x00, 0x49, 0x1d ; 23 -> 50
    db 0x00, 0x4b, 0x2c ; 24 -> minutes
    db 0x00, 0x5c, 0x87 ; 25 -> seconds

; The sound and its length from the two tables above is passed in the accumulator.
Play_Sound_Using_Index:
	setb SOUND ; Turn speaker on
	clr TMOD20 ; Stop the CCU from playing previous request
	setb FLASH_CE
	
	; There are three bytes per row in our tables, so multiply index by three
	mov b, #3
	mul ab
	mov R0, a ; Make a copy of the index*3
	
	clr FLASH_CE ; Enable SPI Flash
	mov a, #READ_BYTES
	lcall Send_SPI
	; Set the initial position in memory of where to start playing
	mov dptr, #sound_index
	mov a, R0
	movc a, @a+dptr
	lcall Send_SPI
	inc dptr
	mov a, R0
	movc a, @a+dptr
	lcall Send_SPI
	inc dptr
	mov a, R0
	movc a, @a+dptr
	lcall Send_SPI
	; Now set how many bytes to play
	mov dptr, #Size_Length
	mov a, R0
	movc a, @a+dptr
	mov w+2, a
	inc dptr
	mov a, R0
	movc a, @a+dptr
	mov w+1, a
	inc dptr
	mov a, R0
	movc a, @a+dptr
	mov w+0, a
	
	mov a, #0x00 ; Request first byte to send to DAC
	lcall Send_SPI
	
	setb TMOD20 ; Start playback by enabling CCU timer

	ret

;---------------------------------------------------------------------------------;
; This is the FSM that plays minutes and seconds after the STOP button is pressed ;
; The state diagram of this FSM is available as 'Stop_Watch_FSM.pdf'              ;
;---------------------------------------------------------------------------------;
T2S_FSM:
	mov a, T2S_FSM_state

T2S_FSM_State0: ; Checks for the start signal (T2S_FSM_Start==1)
	cjne a, #0, T2S_FSM_State1
	jnb T2S_FSM_Start, T2S_FSM_State0_Done
	; Check if minutes is larger than 19
	clr c
	mov a, minutes
	subb a, #20
	jnc minutes_gt_19
	mov T2S_FSM_state, #1
	sjmp T2S_FSM_State0_Done
minutes_gt_19:
	mov T2S_FSM_state, #3
T2S_FSM_State0_Done:
	ret
	
T2S_FSM_State1: ; Plays minutes when minutes is less than 20
	cjne a, #1, T2S_FSM_State2
	mov a, minutes
	lcall Play_Sound_Using_Index
	mov T2S_FSM_State, #2
	ret 

T2S_FSM_State2: ; Stay in this state until sound finishes playing
	cjne a, #2, T2S_FSM_State3
	jb TMOD20, T2S_FSM_State2_Done 
	mov T2S_FSM_State, #6
T2S_FSM_State2_Done:
	ret

T2S_FSM_State3: ; Plays the tens when minutes is larger than 19, for example for 42 minutes, it plays 'forty'
	cjne a, #3, T2S_FSM_State4
	mov a, minutes
	mov b, #10
	div ab
	add a, #18
	lcall Play_Sound_Using_Index
	mov T2S_FSM_State, #4
	ret

T2S_FSM_State4: ; Stay in this state until sound finishes playing
	cjne a, #4, T2S_FSM_State5
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #5
T2S_FSM_State4_Done:
    ret

T2S_FSM_State5: ; Plays the units when minutes is larger than 19, for example for 42 minutes, it plays 'two'
	cjne a, #5, T2S_FSM_State6
	mov a, minutes
	mov b, #10
	div ab
	mov a, b
	jz T2S_FSM_State5_Done ; Prevents from playing something like 'forty zero'
	lcall Play_Sound_Using_Index
T2S_FSM_State5_Done:
	mov T2S_FSM_State, #2
	ret

T2S_FSM_State6: ; Plays the word 'minutes'
	cjne a, #6, T2S_FSM_State7
	mov a, #24 ; Index 24 has the word 'minutes'
	lcall Play_Sound_Using_Index
	mov T2S_FSM_State, #7
	ret

T2S_FSM_State7: ; Stay in this state until sound finishes playing
	cjne a, #7, T2S_FSM_State8
	jb TMOD20, T2S_FSM_State7_Done 
	; Done playing previous sound, check if seconds is larger than 19
	clr c
	mov a, seconds
	subb a, #20
	jnc seconds_gt_19
	mov T2S_FSM_state, #8
	sjmp T2S_FSM_State0_Done
seconds_gt_19:
	mov T2S_FSM_state, #10
T2S_FSM_State7_Done:
    ret

T2S_FSM_State8: ; Play the seconds when seconds is less than 20.
	cjne a, #8, T2S_FSM_State9
	mov a, seconds
	lcall Play_Sound_Using_Index
	mov T2S_FSM_state, #9
	ret

T2S_FSM_State9: ; Stay in this state until sound finishes playing
	cjne a, #9, T2S_FSM_State10
	jb TMOD20, T2S_FSM_State9_Done 
	mov T2S_FSM_State, #13
T2S_FSM_State9_Done:
	ret

T2S_FSM_State10:  ; Plays the tens when seconds is larger than 19, for example for 35 seconds, it plays 'thirty'
	cjne a, #10, T2S_FSM_State11
	mov a, seconds
	mov b, #10
	div ab
	add a, #18
	lcall Play_Sound_Using_Index
	mov T2S_FSM_state, #11
	ret

T2S_FSM_State11: ; Stay in this state until sound finishes playing
	cjne a, #11, T2S_FSM_State12
	jb TMOD20, T2S_FSM_State11_Done 
	mov T2S_FSM_State, #12
T2S_FSM_State11_Done:
	ret

T2S_FSM_State12: ; Plays the units when seconds is larger than 19, for example for 35 seconds, it plays 'five'
	cjne a, #12, T2S_FSM_State13
	mov a, seconds
	mov b, #10
	div ab
	mov a, b
	jz T2S_FSM_State12_Done ; Prevents from saying something like 'thirty zero'
	lcall Play_Sound_Using_Index
T2S_FSM_State12_Done:
	mov T2S_FSM_State, #9
	ret

T2S_FSM_State13: ; Plays the word 'seconds'
	cjne a, #13, T2S_FSM_State14
	mov a, #25 ; Index 25 has the word 'seconds'
	lcall Play_Sound_Using_Index
	mov T2S_FSM_State, #14
	ret

T2S_FSM_State14: ; Stay in this state until sound finishes playing
	cjne a, #14, T2S_FSM_Error
	jb TMOD20, T2S_FSM_State14_Done 
	clr T2S_FSM_Start 
	mov T2S_FSM_State, #0
T2S_FSM_State14_Done:
	ret

T2S_FSM_Error: ; If we got to this point, there is an error in the finite state machine.  Restart it.
	mov T2S_FSM_state, #0
	clr T2S_FSM_Start
	ret
; End of FMS that plays minutes and seconds


