state_done: dbit 1

Timer_done: dbit 1
goal_temp: ds 2 ;desired period for current state
current_state_time: ds 1 ;time elapsed for current state
current_temp: ds 2



;mov goal_temp, 100

FSMstate(goal_temp, goal_time, current_state_time) ;example macro call


FSMstate mac

skip%Mb:

	;time check
	mov a, goal_time
	clr c
	subb a, current_state_time
	jb c, skip%Ma			;if goal_time < current_state_time, end the state

	;temp check
	lcall GetTemp
	; resulting current temp saved in current_temp+1 to current_temp+0
	
	;compare higher byte temperature values
	mov a, %0 +1
	clr c
	subb a, current_temp+1
	jb c, skip%Ma 	;c = 1 if goal_temp-high < current_temp-high, end the state
	cjne a, #0x00, skip%Mb	;if gial_temp-high = current_temp-high, check current_temp-low
					
	;compare lower byte temperature values
	mov a, %0
	clr c
	subb a, current_temp
	jb c, skip%Ma	;c = 1 if goal_temp-low < current_temp-low, end the state
	ljmp skip%Mb
	
skip%Ma:

endmac