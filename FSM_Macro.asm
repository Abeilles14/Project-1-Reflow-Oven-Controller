;----------------------------;
;FSM Macro combining logics  ;
;calling example:	     ;
;FSMstate(goaltemp, goaltime);
;inputs are of decimal values;
;----------------------------;
FSMstate mac

skip%Mb:

	;time check
	mov a, %1
	clr c
	subb a, BCD_counterSec
	jb c, skip%Ma			;if goal_time < current_state_time, end the state

	;temp check
	lcall Read_ADC_Channel
	lcall GetTemp
	; resulting current temp saved in currentTemp (decimal value)
	
	;compare lower byte temperature values
	mov a, %0
	clr c
	subb a, currentTemp
	jb c, skip%Ma	;c = 1 if goal_temp < current_temp, end the state
	ljmp skip%Mb
	
skip%Ma:

endmac
