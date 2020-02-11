;audjust oven power by comparing current Temp and goal Temp
CheckPower:
	push acc
CompareUpperB_P:
	mov a, goalTemp+1
	clr c
	subb a, Result+1	;goal-temp
	jnc PowerON		; if temp<goal UB, power on, else compare lower byte
CompareLowerB_P:
	mov a, SoakTemp+0
	clr c
	subb a, Result+0	;goal-temp
	jnc PowerON		; temp<goal LB, power on, else power off
	ljmp PowerOFF
	pop acc
	ret
PowerOFF:
	clr POWER
	pop acc
	ret
PowerON:
	setb POWER
	pop acc
	ret
END
