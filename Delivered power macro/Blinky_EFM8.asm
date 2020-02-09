
$NOLIST
$MOD9351
$LIST

toaster_on EQU P0.0 

org 0000H
    ljmp main
org 001BH
	ljmp 1803H

$NOLIST
$INCLUDE(Blinkymacro.inc)
$LIST



main:

    mov SP, #7FH
    ; Enable crossbar and weak pull-ups
	mov	P0M1,#0x00
	mov	P0M2,#0x00
    clr toaster_on               ; set pin to zero to beign with
M0:
    clr toaster_on ; Led on
    Wait_Micro_Seconds(#99)
    setb toaster_on ; led off

    Wait_Micro_Seconds(#1)



    sjmp M0
end
