;--------------------------------------;
;		REFLOW OVEN CONTROLLER FSM	   ;
;--------------------------------------;
;STATE 0: Settings (until START)
;STATE 1: Ramp to Soak (until Soak temp)
;STATE 2: Soak (until Soak timer done)
;STATE 3: Ramp to Refl (until Refl timer done)
;STATE 4: Refl (until Refl timer done)
;STATE 5: Cooling (until Temp < 60C)
;---------------------------------------;

DSEG
state: ds 1
Temp_soak: ds 1
Time_soak: ds 1
Temp_refl: ds 1
Time_refl: ds 1


; in forever loop: mov a, state
State0_Settings:			;loop in settings until start
	cjne a, #0, State1_RampSoak
	mov pwm, #0
	; TODO
	; check for start flag/button to jump to state0_done, if not, loop in Settings
	; If pressed, 
	mov state, #1
state0_done:		; when state0 done, loop to?
	ljmp forever

State1_RampSoak:
	cjne a, #1, State2_Soak
	mov pwm, #100
	mov sec, #0
	mov a, temp_soak
	clr c
	subb a, temp
	jnc state1_done
	; TODO
	; if Soak temp reached, start soak timer
	mov state, #2
state1_done:		; when state1 done, loop to?
	ljmp forever

State2_Soak:
	cjne a, #2, State3_RampRefl
	mov pwm, #20
	mov a, time_soak
	clr c
	subb a, sec
	jnc state2_done
	mov state, #3
state2_done:
	ljmp forever

State3_RampRefl:


State4_Refl:

State5_Cooling:




