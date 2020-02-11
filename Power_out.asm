;audjust oven power by comparing current Temp and goal Temp
CheckPower:
	push acc
	mov a, goalTemp+1
	cjne a, Result+1, TempCarry
	mov a, goalTemp+0
	cjne a, Result+0, TempCarry
	pop acc
	ret
TempCarry:
	jc PowerOFF
	ljmp PowerON
PowerOFF:
	clr POWER
	pop acc
	ret
PowerON:
	setb POWER
	pop acc
	ret
END