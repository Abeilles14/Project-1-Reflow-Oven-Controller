$NOLIST


CSEG

	
Read_ADC_Channel mac
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
mov R0, a
lcall DO_SPI_G
mov a, R1 ; R1 contains bits 8 and 9
anl a, #00000011B ; We need only the two least significant bits
mov R7, a ; Save result high.
mov R0, #55H ; It doesn't matter what we transmit...
lcall DO_SPI_G
mov R6, R1 ; R1 contains bits 0 to 7. Save result low.
setb CE_ADC
ret
$LIST
