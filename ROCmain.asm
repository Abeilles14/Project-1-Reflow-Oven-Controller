$NOLIST
$MOD9351
$LIST

;DECLARATIONS
CLK         EQU 14746000  ; Microcontroller system clock frequency in Hz
CCU_RATE    EQU 22050     ; 22050Hz is the sampling rate of the wav file we are playing
CCU_RELOAD  EQU ((65536-((CLK/(2*CCU_RATE)))))
BAUD        EQU 115200
BRVAL       EQU ((CLK/BAUD)-16)

; PINS INPUT OUTPUTS
FLASH_CE EQU P2.4
MY_MOSI EQU P2.2
MY_MISO EQU P2.3
MY_SCLK EQU P2.5

SOUND_OUT equ P2.7

;Buttons
BOOT_BUTTON equ P2.0

TEMP_BUTTON  equ P0.1		; Inc temperature
ALMIN_BUTTON  equ P0.3	; Inc minutes
ALSEC_BUTTON   equ P0.2		; Inc seconds

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
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; These register definitions needed by 'math32.inc'
DSEG at 0x30
w:	 ds 3
x:   ds 4
y:   ds 4
bcd: ds 5

; TEMPERATURE
SaveT: ds 4
currentTemp: ds 2	; current temperature from sensor
SoakTemp: ds 2		; set soak temperature
ReflTemp: ds 2		; set refl temperature
; TIMER COUNTERS	; contains counters and timers
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

BSEG
mf: dbit 1
half_seconds_flag: dbit 1	; Set to 1 in the ISR every time 1000 ms had passed (actually 1 second flag)
start_counter: dbit 1		; Set to 1 once ready to start countdown
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
_C:	DB '000C',0
_blank: DB ' ',0
_default: DB '00:00',0
_Warning: DB '!', 0

$NOLIST
$include(math32.inc)
$include(LCD_4bit_LPC9351.inc)
;$include (reflproc_FSM.asm)
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

;---------------------------------;
; High constants for CRC-CCITT    ;
; (XModem) Polynomial:            ;
; x^16 + x^12 + x^5 + 1 (0x1021)  ;
;---------------------------------;
CRC16_TH:
	db	000h, 010h, 020h, 030h, 040h, 050h, 060h, 070h
	db	081h, 091h, 0A1h, 0B1h, 0C1h, 0D1h, 0E1h, 0F1h
	db	012h, 002h, 032h, 022h, 052h, 042h, 072h, 062h
	db	093h, 083h, 0B3h, 0A3h, 0D3h, 0C3h, 0F3h, 0E3h
	db	024h, 034h, 004h, 014h, 064h, 074h, 044h, 054h
	db	0A5h, 0B5h, 085h, 095h, 0E5h, 0F5h, 0C5h, 0D5h
	db	036h, 026h, 016h, 006h, 076h, 066h, 056h, 046h
	db	0B7h, 0A7h, 097h, 087h, 0F7h, 0E7h, 0D7h, 0C7h
	db	048h, 058h, 068h, 078h, 008h, 018h, 028h, 038h
	db	0C9h, 0D9h, 0E9h, 0F9h, 089h, 099h, 0A9h, 0B9h
	db	05Ah, 04Ah, 07Ah, 06Ah, 01Ah, 00Ah, 03Ah, 02Ah
	db	0DBh, 0CBh, 0FBh, 0EBh, 09Bh, 08Bh, 0BBh, 0ABh
	db	06Ch, 07Ch, 04Ch, 05Ch, 02Ch, 03Ch, 00Ch, 01Ch
	db	0EDh, 0FDh, 0CDh, 0DDh, 0ADh, 0BDh, 08Dh, 09Dh
	db	07Eh, 06Eh, 05Eh, 04Eh, 03Eh, 02Eh, 01Eh, 00Eh
	db	0FFh, 0EFh, 0DFh, 0CFh, 0BFh, 0AFh, 09Fh, 08Fh
	db	091h, 081h, 0B1h, 0A1h, 0D1h, 0C1h, 0F1h, 0E1h
	db	010h, 000h, 030h, 020h, 050h, 040h, 070h, 060h
	db	083h, 093h, 0A3h, 0B3h, 0C3h, 0D3h, 0E3h, 0F3h
	db	002h, 012h, 022h, 032h, 042h, 052h, 062h, 072h
	db	0B5h, 0A5h, 095h, 085h, 0F5h, 0E5h, 0D5h, 0C5h
	db	034h, 024h, 014h, 004h, 074h, 064h, 054h, 044h
	db	0A7h, 0B7h, 087h, 097h, 0E7h, 0F7h, 0C7h, 0D7h
	db	026h, 036h, 006h, 016h, 066h, 076h, 046h, 056h
	db	0D9h, 0C9h, 0F9h, 0E9h, 099h, 089h, 0B9h, 0A9h
	db	058h, 048h, 078h, 068h, 018h, 008h, 038h, 028h
	db	0CBh, 0DBh, 0EBh, 0FBh, 08Bh, 09Bh, 0ABh, 0BBh
	db	04Ah, 05Ah, 06Ah, 07Ah, 00Ah, 01Ah, 02Ah, 03Ah
	db	0FDh, 0EDh, 0DDh, 0CDh, 0BDh, 0ADh, 09Dh, 08Dh
	db	07Ch, 06Ch, 05Ch, 04Ch, 03Ch, 02Ch, 01Ch, 00Ch
	db	0EFh, 0FFh, 0CFh, 0DFh, 0AFh, 0BFh, 08Fh, 09Fh
	db	06Eh, 07Eh, 04Eh, 05Eh, 02Eh, 03Eh, 00Eh, 01Eh

;---------------------------------;
; Low constants for CRC-CCITT     ;
; (XModem) Polynomial:            ;
; x^16 + x^12 + x^5 + 1 (0x1021)  ;
;---------------------------------;
CRC16_TL:
	db	000h, 021h, 042h, 063h, 084h, 0A5h, 0C6h, 0E7h
	db	008h, 029h, 04Ah, 06Bh, 08Ch, 0ADh, 0CEh, 0EFh
	db	031h, 010h, 073h, 052h, 0B5h, 094h, 0F7h, 0D6h
	db	039h, 018h, 07Bh, 05Ah, 0BDh, 09Ch, 0FFh, 0DEh
	db	062h, 043h, 020h, 001h, 0E6h, 0C7h, 0A4h, 085h
	db	06Ah, 04Bh, 028h, 009h, 0EEh, 0CFh, 0ACh, 08Dh
	db	053h, 072h, 011h, 030h, 0D7h, 0F6h, 095h, 0B4h
	db	05Bh, 07Ah, 019h, 038h, 0DFh, 0FEh, 09Dh, 0BCh
	db	0C4h, 0E5h, 086h, 0A7h, 040h, 061h, 002h, 023h
	db	0CCh, 0EDh, 08Eh, 0AFh, 048h, 069h, 00Ah, 02Bh
	db	0F5h, 0D4h, 0B7h, 096h, 071h, 050h, 033h, 012h
	db	0FDh, 0DCh, 0BFh, 09Eh, 079h, 058h, 03Bh, 01Ah
	db	0A6h, 087h, 0E4h, 0C5h, 022h, 003h, 060h, 041h
	db	0AEh, 08Fh, 0ECh, 0CDh, 02Ah, 00Bh, 068h, 049h
	db	097h, 0B6h, 0D5h, 0F4h, 013h, 032h, 051h, 070h
	db	09Fh, 0BEh, 0DDh, 0FCh, 01Bh, 03Ah, 059h, 078h
	db	088h, 0A9h, 0CAh, 0EBh, 00Ch, 02Dh, 04Eh, 06Fh
	db	080h, 0A1h, 0C2h, 0E3h, 004h, 025h, 046h, 067h
	db	0B9h, 098h, 0FBh, 0DAh, 03Dh, 01Ch, 07Fh, 05Eh
	db	0B1h, 090h, 0F3h, 0D2h, 035h, 014h, 077h, 056h
	db	0EAh, 0CBh, 0A8h, 089h, 06Eh, 04Fh, 02Ch, 00Dh
	db	0E2h, 0C3h, 0A0h, 081h, 066h, 047h, 024h, 005h
	db	0DBh, 0FAh, 099h, 0B8h, 05Fh, 07Eh, 01Dh, 03Ch
	db	0D3h, 0F2h, 091h, 0B0h, 057h, 076h, 015h, 034h
	db	04Ch, 06Dh, 00Eh, 02Fh, 0C8h, 0E9h, 08Ah, 0ABh
	db	044h, 065h, 006h, 027h, 0C0h, 0E1h, 082h, 0A3h
	db	07Dh, 05Ch, 03Fh, 01Eh, 0F9h, 0D8h, 0BBh, 09Ah
	db	075h, 054h, 037h, 016h, 0F1h, 0D0h, 0B3h, 092h
	db	02Eh, 00Fh, 06Ch, 04Dh, 0AAh, 08Bh, 0E8h, 0C9h
	db	026h, 007h, 064h, 045h, 0A2h, 083h, 0E0h, 0C1h
	db	01Fh, 03Eh, 05Dh, 07Ch, 09Bh, 0BAh, 0D9h, 0F8h
	db	017h, 036h, 055h, 074h, 093h, 0B2h, 0D1h, 0F0h


EX1_ISR:
   clr ECCU
   reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for the CCU                     ;
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
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	mov TIFR2, #0 ; Clear CCU Timer Overflow Interrupt Flag bit. Actually, it clears all the bits!
	setb P2.6
	
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
;    jnb ALSEC_BUTTON, Timer2_ISR_decrement
;	add a, #0x01
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
    mov SP, #0x7F
    
    lcall Ports_Init ; Default all pins as bidirectional I/O. See Table 42.
    lcall LCD_4BIT
    lcall Double_Clk
	lcall InitSerialPort
	lcall InitADC0 ; Call after 'Ports_Init'
	lcall InitDAC1 ; Call after 'Ports_Init'
	lcall CCU_Init
	lcall Init_SPI
	
	clr TMOD20 ; Stop CCU timer
	setb EA ; Enable global interrupts.
	
	clr SOUND_OUT ; Turn speaker off

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
	
	; Set counters
	mov BCD_counterSec, #0x00
	mov BCD_counterMin, #0x00
	mov a, BCD_counterSec 		; number to be displayed placed in accumulator
	Set_Cursor(1, 11)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counterMin); 
	Set_Cursor(2, 11)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counterSec);
	ljmp SetupSoak			; sets up all soak temp, time, refl temp, time before counter start

;-----------------------------;
;	SET SOAK/REFL SETTINGS	  ;
;-----------------------------;
;--------- SETUP SOAK ---------;
SetupSoak:
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
		
	ljmp SetupSoak	;loops in Setup until Start button pressed

CheckReflSet:			; if startmode button pressed, set refl
	jb STARTSTOP_BUTTON, SetupSoak
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, SetupSoak
    jnb STARTSTOP_BUTTON, $
    ljmp SetupRefl
    	
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
	ljmp SetupSoak
	
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
	ljmp SetupSoak	
incrementSM:
	add a, #0x01
	da a
	mov SoakMinAlarm, a
	clr a
	lcall Display_Soak
	ljmp SetupSoak
	
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
	ljmp SetupSoak
incrementSS:
	add a, #0x01
	da a
	mov SoakSecAlarm, a
	clr a
	lcall Display_Soak
	ljmp SetupSoak
    
;--------- SETUP REFLOW	--------;
SetupRefl:
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
	
	ljmp SetupRefl	;loops in Setup until Start button pressed
		
CheckStartTimer:		; if modestart buttup pressed, start timer and main loop
	jb STARTSTOP_BUTTON, SetupRefl
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, SetupRefl
    jnb STARTSTOP_BUTTON, $
    
	setb half_seconds_flag		; pressed to exit settings and start timer
	setb ECCU
	
	mov BCD_counterMin, SoakMinAlarm	; move time settings into counters
	mov BCD_counterSec, SoakSecAlarm
	
	clr refltimer_done; clear timer done flags
	
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
	ljmp SetupRefl
	
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
	ljmp SetupRefl
incrementRM:
	add a, #0x01
	da a
	mov ReflMinAlarm, a
	clr a
	lcall Display_Refl
	ljmp SetupRefl
	
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
	ljmp SetupRefl
incrementRS:
	add a, #0x01
	da a
	mov ReflSecAlarm, a
	clr a
	lcall Display_Refl
	ljmp SetupRefl
   
;----------------------------;
;		 MAIN LOOP   		 ;
;----------------------------;

; forever loop interface with putty
Forever:
	; TEMPERATURE CHECK
	;lcall checktemp			;to display current temp later
	
	; TIME CHECK
	jb BOOT_BUTTON, CheckStop  ; buttons to change screen to Clock and Current Temp later
	Wait_Milli_Seconds(#50)
	jb BOOT_BUTTON, CheckStop
	jnb BOOT_BUTTON, $

	clr ECCU                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	mov BCD_counterSec, a
	mov BCD_counterMin, a
	setb ECCU                ; Start timer 2
	
	ljmp WriteNum 

	; Do this forever
	sjmp Forever

;;;;;;;;;;;;;;;;;;;;;;;;; DEBUG!!!	
CheckStop:
    jb STARTSTOP_BUTTON, loop_a		; if stop button not pressed, go loop and check for 00
    Wait_Milli_seconds(#50)
    jb STARTSTOP_BUTTON, loop_a
    jnb STARTSTOP_BUTTON, $
    
    clr ECCU                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	mov BCD_counterSec, a
		
	ljmp SetupSoak		; if stop button pressed, go back to setup
	
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
	clr ECCU                 ; Stop timer 2
	clr a	
	ljmp SetupSoak		; go back to settings
	
StartReflTimer:
	setb refltimer_done			; set to indicate final stage in process
	mov BCD_counterMin, ReflMinAlarm
	mov BCD_counterSec, ReflSecAlarm
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
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END DEBUG

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
	
END