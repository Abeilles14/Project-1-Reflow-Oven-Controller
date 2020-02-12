$NOLIST

CSEG

ReadTemp:
	lcall ReadVolts

	ret
ReadVolts:
	mov LM_Result+0, AD0DAT2 ;read cj temp
	lcall Average_AD0DAT3 ;Read average of voltage from ThermoCouple
	mov TC_Result+0,x+0	;Move Result into TC_Result
	mov TC_Result+1,x+1
	lcall Do_Something_With_Result
	
	ret


Do_Something_With_Result:

push acc
clr a
mov buffer+0,a
mov buffer+1,a
mov buffer+2,a
mov buffer+3,a
mov buffer+4,a

clr a
mov x+0,TC_Result+0 
mov x+1,TC_Result+1
mov x+2,a
mov x+3,a

lcall hex2bcd
Send_BCD(bcd+2);Send ThermoCouple Voltage to Python
Send_BCD(bcd+1)
Send_BCD(bcd+0)
	
mov DPTR, #New_Line;Send Newline so python knows its the end of transmission
lcall SendString

mov x+0,LM_Result+0 ;Perform math on LM335 Voltage to convert to Celcius

clr a
mov x+1,a
mov x+2,a
mov x+3,a
Load_y(3300)
lcall mul32
Load_y(255)
lcall div32
Load_y(2730)
lcall sub32
Load_y(10)
lcall div32
mov LM_TEMP+0,x+0
mov LM_TEMP+1,x+1

lcall GeString ;Recieve converted ThermoCouple Temperature from Python

mov a, buffer+0 ;convert 4 digits from ascii to bcd
anl a, #0x0F
swap a
mov r0, a
mov a, buffer+1
anl a, #0x0F
orl a, r0
mov bcd+1,a

mov a, buffer+2
anl a, #0x0F
swap a
mov r0, a
mov a,buffer+3
anl a, #0x0F
orl a, r0
mov bcd+0,a
clr a	;clear remaining bits of bcd
mov bcd+2,a
mov bcd+3,a
mov bcd+4,a


lcall bcd2hex ;convert to hex so we can add Thermocouple temp and Cold junction temp together
mov TC_Result+0,x+0
mov TC_Result+1,x+1


clr a

mov y+0,LM_TEMP+0
mov y+1,LM_TEMP+1
mov y+2,a
mov y+3,a

lcall add32
mov Result+0, x+0
mov Result+1, x+1

pop acc


ret
  
END