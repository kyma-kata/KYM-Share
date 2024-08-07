!DSP56000ProgramOrganizer readProgramSegmentFor: 'class ReadMemory' editDate: '(14 April 2000 10:02:23 am )'!ReadMemory	classDef	2,paramPtr,inputPtr
;
; Use the input to lookup a value from the table.
; The left input is scaled and offset to do the indexing.
;
;	0	scale		offset
;	1	leftInput	rightInput
;		
;
	move			l:(paramPtr),x					; x := scale | offset
	tfr		x0,a	x:(inputPtr),x0					; x0 := input sample value
	mac		x1,x0,a	outputPtr,inputPtr				; a := target address
@	move			a,r0							; r0 := target address
@	move			x:(r0),a						; a := tbl[phs]
@	move			a,x:(inputPtr)	a,y:(outputPtr)	; output both channels
	rts

	endClass	(Miss + 14),0
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class WriteMemory' editDate: '(14 April 2000 10:03:45 am )'!WriteMemory	classDef	3,paramPtr,inputPtr
;
; Use the first input to write a value (the second input) into the table.
; The left channel of the first input is scaled and offset to do the indexing.
; The left channel of the second input is written into the table.
;
;	0	scale		offset
;	1	leftInput	rightInput		; value (we assume left and right values are the same)
;	2	leftInput	rightInput		; index
;		
;
	move			l:(paramPtr),x					; x := scale | offset
	tfr		x0,a	x:(inputPtr)-,x0				; x0 := input sample value
	mac		x1,x0,a	l:(inputPtr),b					; a := target address, b := value to write
@	move			a,r0							; r0 := target address
@	move			b,x:(r0)						; tbl[phs] := b
@	move			b,l:(outputPtr)					; output both channels
	rts

	endClass	(Miss + 14),0
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class ExponentialDecay' editDate: '(4 December 2002 7:50:45 pm )'!ExponentialDecay classDef	2,paramPtr
		move	X:(paramPtr)+,x0	;x0 := amplitude
		move	X:(paramPtr)-,y0	;y0 := decay factor
		mpyr	x0,y0,a		;a := amplitude + decay factor
		move	a,X:(paramPtr)	;save amplitude in Microsound for next time
		move	x0,X:(outputPtr)	;output left
		move	x0,Y:(outputPtr)	;output right
		rts
		endClass! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class SafeRCL' editDate: '(11 December 2002 1:30:00 pm )'!SafeRCL classDef	4,paramPtr,inputPtr
	move	X:(inputPtr),a		Y:(paramPtr),y0 				;a := Input and y0 :=LastV
	sub		y0,a		X:(paramPtr)+,x0 						;x0 is Rfactor
	move	a,y1

	;move	a,X:(outputPtr)		;TEST
	;rts							;TEST
	;endClass					;TEST




	mpyr	x0,y1,a		X:(paramPtr),x1 					 	;Inew now in a :: x1 is Lfactor 




	move	Y:(paramPtr)+,y1 									;y1 is LastI
	mpyr	x1,y0,b		
	add		y1,b		X:(paramPtr)-,x0						;b is NewLcurrent ::x0 is now Cfactor
	move	b,Y:(paramPtr)-									;save new current in YParam

	sub		b,a	
	move	a,y1												;y1 is new total current
	mpyr	x0,y1,b												
	add		y0,b												;b is new voltage
	;move	y1,Y:(paramPtr)-									;save new current in YParam
	move	b,X:(outputPtr)										;volts goes to left output
	move	b,Y:(paramPtr)										;save new volts in y param
	move	b,Y:(outputPtr)										;volts goes to right output

	;move	X:(inputPtr),a		;TEST
	;move	a,X:(outputPtr)		;TEST

	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class TrackAndHold' editDate: '(18 December 2002 9:28:32 pm )'!TrackAndHold classDef	3,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 .
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	
			move	Y:(paramPtr),x1
			jset	#0,X:(paramPtr),TAHhere1
			move	X:(inputPtr),x1
			move	x1,Y:(paramPtr)
TAHhere1	move	x1,X:(outputPtr)								;volts goes to left output
			move	x1,Y:(outputPtr)		




			bclr	#0,X:(paramPtr)
			move	X:-(inputPtr),a		
		
			tst		a
		
			jle		TAHhere
			bset	#0,X:(paramPtr)
	
TAHhere		;move	y1,X:(outputPtr)								;volts goes to left output
			;move	y1,Y:(outputPtr)								;volts goes to right output
	
	
			rts
			endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class GateToTrigger' editDate: '(30 December 2002 5:21:10 pm )'!GateToTrigger classDef	3,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 ..
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	
	
		
		
		move	X:(inputPtr),a		Y:(paramPtr)+,y1				;load input and place o/p value for false in y1
		clr		b					X:(paramPtr)-,x1	

		tst		a					
		jle		gtthere												;jump if zero or less
																	;this stuff only done if input is true
		not		b					x1,a
																
		;move	x1,a											;load up last state
		tst		a
		jne		gtthere
		move	X:(paramPtr),y1									;place o/p value for true in y1
	
gtthere	move	y1,X:(outputPtr)									;to left output
		move	y1,Y:(outputPtr)									;to right output
		move	X:(paramPtr)+,x0									;dummy move to cause incrument
		move	b,X:(paramPtr)										;save last state
		
	
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class duffMixerWithRap' editDate: '(2 January 2003 5:08:47 pm )'!duffMixerWithWrap classDef	30,paramPtr,inputPtr
	
	;this is a mono	mixer with wrap around so that 1.1 becomes -0.9
	
	
	move	#$00,x1											;clear out MSB of X to allow and of the LSW
	clr		a				X:(inputPtr)-,x0				;clear all of a so that inputs can be added to its LSW
	rep		Y:(paramPtr)									;repeat next instruction by No of inputs(stored in yparam 0
	add		x,a		X:(inputPtr)-,x0						;Do add and set up next add

	move	a0,X:(outputPtr)								;goes to left output
	move	a0,Y:(outputPtr)								;goes to right output


	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class MixerWithWrap' editDate: '(2 January 2003 5:42:07 pm )'!MixerWithWrap classDef	30,paramPtr,inputPtr
	
	;this is a mono	mixer with wrap around so that 1.1 becomes -0.9
	
	
	;move	#$00,x1											;clear out MSB of X to allow and of the LSW
	clr		a				X:(inputPtr)-,x0				;clear all of a so that inputs can be added to its LSW
	rep		Y:(paramPtr)									;repeat next instruction by No of inputs(stored in yparam 0
	add		x0,a		X:(inputPtr)-,x0						;Do add and set up next add

	move	a1,X:(outputPtr)								;goes to left output
	move	a1,Y:(outputPtr)								;goes to right output


	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class RampGenerator' editDate: '(2 January 2003 7:06:57 pm )'!RampGenerator classDef	2,paramPtr
		



		move	X:(paramPtr),a						;a := amplitude
		move	#1,y1
		add		y1,a		Y:(paramPtr)+,y0		;y0 := MaxNumber
		cmp		y0,a		X:(paramPtr)-,x1		;x1 := Starting Number
		tgt		x1,a
		move	a,X:(paramPtr)						;save amplitude in Microsound for next time
		move	a,X:(outputPtr)						;output left
		move	a,Y:(outputPtr)						;output right
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class MixerAndGainWithWrap' editDate: '(8 January 2003 6:44:04 pm )'!MixerAndGainWithWrap classDef	30,paramPtr,inputPtr
	
	;this is a mono	mixer with wrap around so that 1.1 becomes -0.9
	
	
	clr		a				X:(inputPtr)-,x0	Y:(paramPtr)+,y0	;clear all of a so that inputs can be added to it
																	;y0 is Int part of gain

	rep		Y:(paramPtr)											;repeat next instruc by No of inputs(stored in yparam 0
	add		x0,a			X:(inputPtr)-,x0						;Do add and set up next add

	move	a1,x0													
	mpy		y0,x0,a			X:-(paramPtr),y1						;y1 is Fractional part of gain
	asr		a
	move	a0,x1

	mpy		y1,x0,a
	add		x1,a

	move	a1,X:(outputPtr)										;goes to left output
	move	a1,Y:(outputPtr)										;goes to right output


	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class Divide' editDate: '(10 January 2003 5:36:41 pm )'!Divide classDef	3,paramPtr,inputPtr
	
	
	
	
		
			move	X:(inputPtr)-,x1		Y:(paramPtr),y1			;2; first input x1 is input and y1 is scale control
			mpyr	x1,y1,a					X:(inputPtr),x0			;2; a is input Scaled   x0 is DevideBy input
			abs		a												;2;
			
			jne		Divhere											;6; if input is zero make op zero even if divided by zero
			clr		a												;not in longest path;
			jmp		DivEnd											;not in longest path;
			

Divhere		andi	#$fe,CCR 										;2; clear carry bit
			rep		#$18											;4;
			div		x0,a											;48;
			add		x0,a					x1,b					;2;
			
			eor		x0,b											;2;
			jclr	#23,b1,DivEnd									;8;
			neg		a												;2;

DivEnd		move	a0,X:(outputPtr)								;2; volts goes to left output
			move	a0,Y:(outputPtr)								;2; volts goes to right output
		
		
			rts														;4;
			endClass				(88)							;88;

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class Divide320' editDate: '(13 January 2003 7:50:01 pm )'!Divide classDef	3,paramPtr,inputPtr
	
	
	
	
		
			move	X:(inputPtr)-,x1		Y:(paramPtr),y1			;2; first input x1 is input and y1 is scale control
			mpyr	x1,y1,a					X:(inputPtr),x0			;2; a is input Scaled   x0 is DevideBy input
			abs		a												;2;
			
			bne		Divhere320										;6; if input is zero make op zero even if divided by zero
			clr		a												;not in longest path;
			bra		DivEnd320										;not in longest path;
			

Divhere320	andi	#$fe,CCR 										;2; clear carry bit
			rep		#$18											;4;
			div		x0,a											;48;
			add		x0,a					x1,b					;2;
			
			mpy		x0,x1,b
			;eor		x0,b											;2;
			;brclr	#23,b1,DivEnd320								;8;
			neg		a				ifmi								;2;

DivEnd320	move	a0,X:(outputPtr)								;2; volts goes to left output
			move	a0,Y:(outputPtr)								;2; volts goes to right output
		
		
			rts														;4;
			endClass												;88;

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class RCL' editDate: '(14 January 2003 11:17:24 am )'!RCL classDef	4,paramPtr,inputPtr
	
	;Below is the code to perform Three formulas as follows

	;  (Input - LastV) * Rfactor = InputCurrent
	;  (LastV * Lfactor) + LastI = NewCurrent
	;  ((InputCurrent - NewCurrent) * Cfactor) + LastV = NewV (and output)
	
	
	move	X:(inputPtr),a		Y:(paramPtr),y0 		;a is Input :: y0 is LastV
	sub		y0,a		X:(paramPtr)+,x0 				;x0 is Rfactor
	move	a,y1										;y1 is Input-LastV
	
	mpyr	x0,y1,a		X:(paramPtr),x1 				;a is InputCurrent :: x1 is Lfactor 

		
	move	Y:(paramPtr)+,b 							;b is LastI
	macr	x1,y0,b		X:(paramPtr)-,x0				;b is NewCurrent ::x0 is now Cfactor


	sub		b,a				;b,Y:(paramPtr)-	
	move	a,y1										;y1 is new total current
	mpyr	x0,y1,b			b,Y:(paramPtr)-				;Multi total cur by Cfactor also save NewCurrent as b was
	add		y0,b										;b is new voltage

	move	b,X:(outputPtr)								;volts goes to left output
	move	b,Y:(paramPtr)								;save new volts in y param
	move	b,Y:(outputPtr)								;volts goes to right output


	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class TryOut' editDate: '(21 January 2003 6:09:27 pm )'!TryOut classDef	10,paramPtr,inputPtr
	
	
		
		move	X:(inputPtr)+,x1		Y:(paramPtr)+,y1
		mpy		x1,y1,a


		lsl		a						X:(paramPtr),x1
		asr		a						Y:(paramPtr),x0
		move	a1,y1				
		move	a0,y0
		
		mpy		x0,y1,a
		mac		y0,x1,a
		asl		a
		move	a2,b
		move	a1,b0
		mac		x1,y1,b					#127,y0
		move	b1,a
		cmpm	y0,b					#128,y1
		tge		y1,a
		cmp		y0,b
		tge		y0,a

		asr		b						a1,a2
		move	b0,a1
		move	a,X:(outputPtr)								;volts goes to left output
		move	a,Y:(outputPtr)								;volts goes to right output
	
		rts
		endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class MultiplyWithGain' editDate: '(21 January 2003 7:00:28 pm )'!MultiplyWithGain classDef	3,paramPtr,inputPtr
	
	
		
		move	X:(inputPtr)-,x1		
		move	X:(inputPtr),y1		
		mpy		x1,y1,a


		lsl		a						X:(paramPtr),x1
		asr		a						Y:(paramPtr),x0
		move	a1,y1				
		move	a0,y0
		
		mpy		x0,y1,a
		mac		y0,x1,a
		asl		a
		move	a2,b
		move	a1,b0
		mac		x1,y1,b					#127,y0
		move	b1,a
		cmpm	y0,b					#128,y1
		tge		y1,a
		cmp		y0,b
		tge		y0,a

		asr		b						a1,a2
		move	b0,a1
		move	a,X:(outputPtr)								;volts goes to left output
		move	a,Y:(outputPtr)								;volts goes to right output
	
		rts
		endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class RCL48' editDate: '(22 January 2003 5:28:01 pm )'!RCL48 classDef	6,paramPtr,inputPtr
	
	;Below is the code to perform Three formulas as follows

	;  (Input - LastV) * Rfactor = InputCurrent
	;  (LastV * Lfactor) + LastI = NewCurrent
	;  ((InputCurrent - NewCurrent) * Cfactor) + LastV = NewV (and output)
	
	
	move	X:(inputPtr),a		Y:(paramPtr)+,y0 		;a is Input ;; y0 is Rfactor
	move	L:(paramPtr)-,b 							;b is LastV
	sub		b,a					X:(paramPtr)+,x0		;a is (Input - LastV) ;; x0 is now Cfactor
	move	a,y1										;y1 is (Input-LastV)	;; dummy to ind param
	
	move	X:(paramPtr)+,x1								; dummy to ind param


	mpy		y0,y1,a		X:(paramPtr)+,x1 				;a is InputCurrent :: x1 is Lfactor 

	move	b,y0	
	move	L:(paramPtr),b 								;b is LastI
	mac		x1,y0,b										;b is NewCurrent :: 
	
	move	b,L:(paramPtr)-	

	sub		b,a				;b,L:(paramPtr)-				;a is (InputCurrent - NewCurrent);; save NewCurrent
	move	a,x1			Y:(paramPtr)-,y0			;x1 is new total current ;; dummy move to dec paramPtr
	
	mpy		x0,x1,b			L:(paramPtr),a				;Multi total cur by Cfactor also Reload lastV in a
	add		b,a											;a is new voltage
	move	a,L:(paramPtr)+		
	
		lsl		a						Y:(paramPtr)+,x1	;x1 is gain Int
		asr		a						X:(paramPtr)+,x0	;dummy to up param
		move	a1,y1					
		move	a0,y0
		
		mpy		x1,y0,a					X:(paramPtr),x0		;x1 is gain Fraction
		mac		y1,x0,a
		asl		a
		move	a2,b
		move	a1,b0
		mac		x1,y1,b					#127,y0
		move	b1,a
		cmpm	y0,b					#128,y1
		tge		y1,a
		cmp		y0,b
		tge		y0,a

		asr		b						a1,a2
		move	b0,a1
		move	a,X:(outputPtr)								;volts goes to left output
		move	a,Y:(outputPtr)								;volts goes to right output
	
		rts
		endClass


! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class MixerWithFlip' editDate: '(27 January 2003 5:28:49 pm )'!MixerWithFlip classDef	30,paramPtr,inputPtr
	
	;this is a mono	mixer with wrap around so that 1.1 becomes -0.9
	
	
		;move	#$00,x1											;clear out MSB of X to allow and of the LSW
		clr		a				X:(inputPtr)-,x1				;clear all of a so that inputs can be added to its LSW
		
		rep		Y:(paramPtr)									;repeat next instruction by No of inputs(stored in yparam 0
		add		x1,a		X:(inputPtr)-,x1					;Do add and set up next add

		jset	#23,a1,here1
		bchg	#$0,a2
here1	jclr	#$0,a2,here2
		move	#1,y0
		add		y0,a
		neg a

here2	move	a1,X:(outputPtr)								;goes to left output
		move	a1,Y:(outputPtr)								;goes to right output

	
		rts
		endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class Ramp2' editDate: '(4 February 2003 4:14:00 pm )'!Ramp2 classDef	3,paramPtr
		



		move	Y:(paramPtr),a								;a := amplitude
		move	#1,y1				
		add		y1,a				X:(paramPtr)+,y0		;y0 := MaxNumber
		cmp		y0,a				L:(paramPtr)-,x			;x0 := Gain and x1 := Starting Number
		tgt		x1,a
		move	a,x1				a,Y:(paramPtr)+			;save amplitude in Microsound for next time

		mpy		x1,x0,a				X:(paramPtr)+,y1		;dummy inc
		asr		a					X:(paramPtr)+,y1		;y1 has error compensation
		move	a0,b						
		add		y1,b

		;move	a,X:(outputPtr)								;output left
		;move	a,Y:(outputPtr)								;output right

		move	b,X:(outputPtr)								;output left
		move	b,Y:(outputPtr)								;output right
		rts
		endClass


! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class StepWriter' editDate: '(6 February 2003 8:13:45 pm )'!StepWriter	classDef	6,paramPtr,inputPtr
;
;
; Use the first input to write a value (the second input) into the table.
; The left channel of the first input is scaled and offset to do the indexing.
; The left channel of the second input is written into the table.
;
;	0	scale			offset
;	1	DeadAddress		1st Samp Address
;	2	ExtraAddress		
;	3	leftInput		rightInput		; value (we assume left and right values are the same)
;	4	leftInput		rightInput		; Gate Write
;	5	leftInput		rightInput		; index	
;
		move			l:(paramPtr)+,x					; x := scale | offset
		tfr		x0,a	x:(inputPtr)-,x0				; x0 := input index value
		mac		x1,x0,a	x:(inputPtr)-,b					; a := target address, b := Write YN
		tst		b		l:(paramPtr)+,y					; y1=Dead Address | y0=1st samp Address+1
		tle		y1,a									; over Write address with dead address
	
@		move	a,r0									; r0 := target address
		move	l:(inputPtr),b	
@		move	b,x:(r0)								; tbl[phs] := b
@		move	b,l:(outputPtr)							; output both channels
		;move	#0,a0
		cmp		y0,a	x:(paramPtr),x0					; x0=1 above top address
		bge		SWEnd
		move	x0,r0
		move	b,x:(r0)
SWEnd	rts

		endClass	(Miss + 28),0
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class WriteMemoryTest' editDate: '(6 February 2003 8:15:08 pm )'!WriteMemoryTest	classDef	6,paramPtr,inputPtr
;
;   OBSOLEETE !!!!!!
;
; Use the first input to write a value (the second input) into the table.
; The left channel of the first input is scaled and offset to do the indexing.
; The left channel of the second input is written into the table.
;
;	0	scale			offset
;	1	DeadAddress		1st Samp Address
;	2	ExtraAddress		
;	3	leftInput		rightInput		; value (we assume left and right values are the same)
;	4	leftInput		rightInput		; Gate Write
;	5	leftInput		rightInput		; index	
;
		move			l:(paramPtr)+,x					; x := scale | offset
		tfr		x0,a	x:(inputPtr)-,x0				; x0 := input index value
		mac		x1,x0,a	x:(inputPtr)-,b					; a := target address, b := Write YN
		tst		b		l:(paramPtr)+,y					; y1=Dead Address | y0=1st samp Address+1
		tle		y1,a									; over Write address with dead address
	
@		move	a,r0									; r0 := target address
		move	l:(inputPtr),b	
@		move	b,x:(r0)								; tbl[phs] := b
@		move	b,l:(outputPtr)							; output both channels
		;move	#0,a0
		cmp		y0,a	x:(paramPtr),x0					; x0=1 above top address
		bge		WMTEnd
		move	x0,r0
		move	b,x:(r0)
WMTEnd	rts

		endClass	(Miss + 28),0
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class Ramp3' editDate: '(18 February 2003 7:01:24 pm )'!Ramp3 classDef	4,paramPtr
		



		move	Y:(paramPtr),a								;a := amplitude
		move	#1,y1				
		add		y1,a				X:(paramPtr)+,y0		;y0 := MaxNumber
		cmp		y0,a				L:(paramPtr)-,x			;x0 := Gain and x1 := Starting Number
		tgt		x1,a
		move	a,x1				a,Y:(paramPtr)+			;save amplitude in Microsound for next time

		mpy		x1,x0,a				X:(paramPtr)+,y1		;dummy inc
		asr		a					L:(paramPtr),y			;y1 has error compensation ,y0has LoGain
		move	a0,b
		macr		x1,y0,b
		add		y1,b

		;move	a,X:(outputPtr)								;output left
		;move	a,Y:(outputPtr)								;output right

		move	b1,X:(outputPtr)								;output left
		move	b1,Y:(outputPtr)								;output right
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class Negate' editDate: '(24 February 2003 2:01:27 pm )'!Negate classDef	2,paramPtr,inputPtr
	
		
	
		
		
		move	X:(inputPtr),a		
		
		neg		a

		;move	a1,X:(paramPtr)									;test only


		move	a1,X:(outputPtr)								;to left output
		move	a1,Y:(outputPtr)								;to right output
	
	
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class Switch' editDate: '(24 February 2003 4:30:51 pm )'!Switch classDef	3,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 .
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	
	
		
		
		move	X:(inputPtr)-,a		Y:(paramPtr),y1				;a is switch control , y1 is off value
		
		tst		a					X:(inputPtr)-,b				; b is input
		
		tle		y1,b
		;move	X:(paramPtr),y1	
	
		move	b,X:(outputPtr)								;to left output
		move	b,Y:(outputPtr)								;to right output
	
	
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class OneSamp' editDate: '(27 February 2003 2:03:39 pm )'!OneSamp classDef	2,paramPtr,inputPtr
	
		;One Sample Delay in Sterio
		;
		;
	
		
		move	l:(paramPtr),y
		move	l:(inputPtr)+,x		
		move	y,l:(outputPtr)
		move	x,l:(paramPtr)
			
	
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class TrackAndPreHold' editDate: '(28 March 2003 2:23:55 pm )'!TrackAndPreHold classDef	3,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 .
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	


			move	X:(inputPtr)-,x0	Y:(paramPtr),b	
			move	X:(inputPtr),a		
		
			tst		a
			tle		x0,b
			
			move	b,Y:(paramPtr)
			move	b,X:(outputPtr)								;volts goes to left output
			move	b,Y:(outputPtr)								;volts goes to right output
	
			rts
			endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class AsLogic' editDate: '(28 March 2003 3:15:09 pm )'!AsLogic classDef	2,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 ..
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	
	
		
		
		move	X:(inputPtr),a		Y:(paramPtr),y1
		
		tst		a					X:(paramPtr),b
		tle		y1,b
		
		move	b,X:(outputPtr)								;to left output
		move	b,Y:(outputPtr)								;to right output
	
	
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class CounterRamp' editDate: '(1 April 2003 5:12:22 pm )'!CounterRamp classDef	9,paramPtr,inputPtr
		


		clr		b					L:(paramPtr)+,x			;x1 is IncSteps and x0 is dec Steps
		move	X:(inputPtr)-,a								;a is countup Y/N
		tst		a
		tgt		x1,b
		move	X:(inputPtr)-,a								;a is countDown Y/N
		tst		a					L:(paramPtr)+,y			;y1:= ResToMax y0 := MaxNumber
		tgt		x0,b
		move	Y:(paramPtr),a								;a := amplitude
		add		b,a					X:(paramPtr)+,x0		;x0 := ResToMin   x1 := Starting Number
		

		cmp		y0,a				X:(paramPtr),x1			;x0 := Gain and x1 := Starting Number
		tgt		x0,a

		cmp		x1,a				X:(inputPtr),b			;b is reset Y/N
		tlt		y1,a

		tst		b					Y:(paramPtr)-,x0		;x0 := Gain
		tgt		x1,a


		move	a,x1				a,Y:(paramPtr)+			;save amplitude in Microsound for next time

		mpy		x1,x0,a				X:(paramPtr)+,y1		;dummy inc
		asr		a					L:(paramPtr),y			;y1 has error compensation ,y0has LoGain
		move	a0,b
		macr		x1,y0,b
		add		y1,b


		move	b1,X:(outputPtr)								;output left
		move	b1,Y:(outputPtr)								;output right
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class AndGate' editDate: '(2 April 2003 7:32:47 pm )'!AndGate classDef	32,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 ..
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	
	
		
		
			move	X:(paramPtr),b

			
			move	X:(inputPtr)-,a		Y:(paramPtr)+,y0
			do		X:(paramPtr),AGHere
			tst		a					X:(inputPtr)-,a
			tle		y0,b
			
AGHere		move	b,X:(outputPtr)								;to left output
			move	b,Y:(outputPtr)								;to right output
		
		
			rts
			endClass


! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class OrGate' editDate: '(2 April 2003 8:05:47 pm )'!OrGate classDef	32,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 ..
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	
	
		
		
			move	X:(paramPtr),y0

			
			move	X:(inputPtr)-,a		Y:(paramPtr)+,b
			do		X:(paramPtr),OGHere
			tst		a					X:(inputPtr)-,a
			tgt		y0,b
			

OGHere		move	b,X:(outputPtr)								;to left output
			move	b,Y:(outputPtr)								;to right output
		
		
			rts
			endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class ExOrGate' editDate: '(3 April 2003 2:35:20 pm )'!ExOrGate classDef	32,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 .
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	
	
		
		
			move	L:(paramPtr)+,x

			
			move	X:(inputPtr)-,a		Y:(paramPtr),b
			do		X:(paramPtr),EOGHere
			neg		b					b,y0	
			tst		a					X:(inputPtr)-,a
			tle		y0,b
			

EOGHere		tst		b					x0,a
			tgt		x1,a
			move	a,X:(outputPtr)								;to left output
			move	a,Y:(outputPtr)								;to right output
		
		
			rts
			endClass


! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class SRFlipFlop' editDate: '(3 April 2003 6:05:07 pm )'!SRFlipFlop classDef	4,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 .
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	
	
		
		
		
		move	X:(inputPtr)-,a		Y:(paramPtr)+,b
		tst		a					L:(paramPtr)-,y
		tgt		y1,b				
		move	X:(inputPtr),a
		tst		a					
		tgt		y0,b				
		move	b,Y:(paramPtr)		

		move	b,X:(outputPtr)									;to left output
		move	b,Y:(outputPtr)									;to right output
		
	
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class Toggle' editDate: '(4 April 2003 7:46:21 pm )'!Toggle classDef	4,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 .
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	
	
			clr		a		X:(inputPtr),b
			not		a		a,y0				
			
							
			tst		b		L:(paramPtr),x
			tgt		y0,a									;a has 0 if input is hi and -max if input is lo	
			not		a		a,X:(paramPtr)+					;save not of input
			and		x1,a	L:(paramPtr)-,y					;And with not of last input  load hi/lo oplevels
			eor		x0,a	y1,b							;ExOr with last opState ref
			;lsl		a		a,Y:(paramPtr)
			teq		y0,b	
			move	a,Y:(paramPtr)								
			move	b,X:(outputPtr)								;to left output
			move	b,Y:(outputPtr)								;to right output
		
			rts
			endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class MultiTapDelay' editDate: '(10 April 2003 7:06:54 pm )'!MultiTapDelay	classDef	108,paramPtr,inputPtr
;
;				X					Y
;	0	MaxNoOfSamples  		current offset
;	1	1st Samp Address		FeedBack
;	2	FeedbackLastSamp		FeedbackOutput	
;	3	FeedbackLastTap			NoOfTaps (+1)
;	4	Offset1
;	5	DummyLevela				DummyLevelb			
;	6	Offset2		
;	7	level1left				1-level1left
;	8	offset3
;	9	level2left				1-level2left	
;
;
;
;	X	leftInput		rightInput		; 
;	

		move	paramPtr,r5									;copy paramptr to r5
		move	L:(paramPtr)+,x								;x1 = MaxNoOfSamps  ; x0 = current offset
		move	#1,a										;to inc record point
		add		x0,a			L:(paramPtr)+,y				;y1 = 1st samp addr ; y0 = Feedback
		move	y0,b
		move	x:(inputPtr),y0								;get input
		add		y0,b			y1,r0						;b contains input sample and feedback

		
		cmp		x1,a			r0,r1

		blt		MTDHere										;only do this next bit if we are at max sample
		move	a,n0
		move	b,X:(r0+n0)									;Record extra sample for interpolation
		sub		x1,a			

MTDHere	move	a,n0
		move	a,n1			
		move	a,x0			a,Y:(r5)+					;save new current offset
		move	b,X:(r0+n0)									;Record input sample with feedback added.
		move	x:(r1)+,b									;Dummy move to inc r1
		move	X:(r1+n1),b
		clr		b				b,Y:(r5)-					;save last sample in table feedback

		move	L:(paramPtr)+,y	


		do		Y:(paramPtr)+,MTDLoop						;first time round is not a tap and is muted
		move	X:(paramPtr)+,a								;a is tap offset
		add		x0,a			X:(r5),x1					;x0 is current offset(write position) ; Put MaxSamps in x1
		cmp		x1,a			L:(paramPtr)+,y				;y1,y0 are levels of tap interpolation pair
		sub		x1,a			ifge						;If tap is out of range put it back in range
		move	X:(r0+n0),x1								; x1 is tap sample from wavetable
		mac		x1,y1,b			a,n0
		move	X:(r1+n1),x1								; x1 is next tap sample for interpolation
		mac		x1,y0,b			a,n1			
MTDLoop														; last time round sets up addr for extra feedback tap
	
		
		
		move	Y:(r5)+,x1									;Dummy to inc r5
		move	Y:(r5)+,x1									;last sample in table feedback is in x1
		move	L:(r5)+,y									;y1 is last samp feedback level ; y0 is O/P feedback level
		mpy		x1,y1,a			b,x0
		mac		x0,y0,a			X:(r1+n1),x1
		
		
		move	X:(r5)-,x0									;x0 is extra tap feedback level
		mac		x1,x0,a			X:(r5)-,x0					; dummy to dec r5
		move	a,Y:(r5)									;save feedback total for adding to recorder next time round

		move	Y:(paramPtr),x1								;x1 is level of dry input
		move	X:(inputPtr),x0
		mac		x0,x1,b			


		move	b,X:(outputPtr)	
		move	b,Y:(outputPtr)	

		

		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class SRRamp' editDate: '(15 April 2003 8:26:37 pm )'!SRRamp classDef	3,paramPtr,inputPtr
	
		;Ramp frequency controled by module at sample rate
		;
		;
	
		
		move	X:(inputPtr)-,x1	y:(paramPtr),a
		add		x1,a				x:(inputPtr),b
		tst		b					a1,y1
		tgt		y1,a
		move	a,y:(paramPtr)
		
		move	a,X:(outputPtr)
		move	a,Y:(outputPtr)
		
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class PulseStretcherFreq' editDate: '(22 April 2003 3:44:27 pm )'!PulseStretcherFreq classDef 5,paramPtr,inputPtr
	
		;Ramp frequency controled by module at sample rate
		;
		;		    X						Y
		;0	 	TrigPercent				RampValue  
		;1		Percent					O/P LoValue
		;2		O/P HiValue				
		;
		;
		

		clr		b					L:(paramPtr)+,x
		move	X:(inputPtr)-,a
		asr		a
		;rnd		a
		add		x0,a				L:(paramPtr)-,y
		move	a,x0										;a and x0 has new rampvalue
		move	X:(inputPtr),a


		tst		a					x0,a
		tle		x0,b										;b is clr if trig is hi and RampValue if trig is Lo
		cmp		x1,a				y0,a
		tlt		x0,b										;
		move	b,Y:(paramPtr)+
		move	X:(paramPtr)+,x0							;Dummy Inc
		cmp		y1,b				X:(paramPtr),x0			;x0 is rampvalue if trig lo or count lessthan TrigPer else 00
		tle		x0,a

		
		
		move	a,X:(outputPtr)
		move	a,Y:(outputPtr)
		
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class PulseStretcherTime' editDate: '(23 April 2003 3:08:22 pm )'!PulseStretcherTime classDef 5,paramPtr,inputPtr
	
		;Ramp frequency controled by module at sample rate
		;
		;		    X						Y
		;0	 	RampInc					RampValue  
		;1		O/P LoValue				TrigMode FFFFFFF
		;2		O/P HiValue				
		;
		;
		

		clr		b					L:(paramPtr)+,x
		move	x1,a				
		add		x0,a				X:(inputPtr)-,y1		;y1 is InputPercent
		move	a,x0										;x0 has new rampvalue
		move	X:(inputPtr),a		Y:(paramPtr)-,y0		;a is GateInput		;y0 is TrigMode


		tst		a					y1,a					;a has newRampValue
		tle		x0,b										;b is clr if trig is hi and RampValue if trig is Lo
		and		y0,a										;flush percent if trigmode
		cmp		x0,a				
		tgt		x0,b										;
		move	b,Y:(paramPtr)+								;Save RampValue
		move	X:(paramPtr)+,a								;Dummy Inc
		cmp		y1,b				X:(paramPtr),x0			;x0 is rampvalue if trig lo or count lessthan TrigPer else 00
		tlt		x0,a

		
		
		move	a,X:(outputPtr)
		move	a,Y:(outputPtr)
		
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class DisableGate' editDate: '(23 April 2003 6:01:53 pm )'!DisableGate classDef	3,paramPtr,inputPtr
	
		;Note the decision as to what two values will be out put are placed in xparam0 and yparam0 .
		;therefore it can be used as an invertor or as output 0 1 or -1 1 without changing this code and changed by hot 		;paramittors.
	
	
		
		move	X:(paramPtr),x1		
		move	X:(inputPtr)-,a		Y:(paramPtr),b				
		
		tst		a					X:(inputPtr),a				
		tle		x1,b
		tst		a
		tgt		x1,b
		
		move	b,X:(outputPtr)								;to left output
		move	b,Y:(outputPtr)								;to right output
	
	
		rts
		endClass

! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class SlewRateLimiter' editDate: '(2 May 2003 7:54:31 pm )'!SlewRateLimiter classDef	3,paramPtr,inputPtr
		



			move	X:(inputPtr),x0			Y:(paramPtr)+,a				;a := amplitude and x0 is Input
			cmp		x0,a					Y:(paramPtr),y1				;y1 is UpRate							
			jgt		SRLHere1											;Jump if not going in up direction
			add		y1,a
			cmp		x0,a
			tgt		x0,a												;if amplit has over shot replace with input
			jmp		SRLHere2
			
SRLHere1	move	X:(paramPtr),y1										;y1 is DownRate
			sub		y1,a				
			cmp		x0,a				
			tlt		x0,a


SRLHere2	move	a,Y:-(paramPtr)								;save amplitude in Microsound for next time
			move	a,X:(outputPtr)								;output left
			move	a,Y:(outputPtr)								;output right
			rts
			endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class ALog2' editDate: '(10 May 2003 11:58:05 pm )'!;
; Convert Left input to y = 2^(32*x)
;   -1 < x < 0, 0 < y < 1
;
; DM/MCFA 05/01
;
ALog2	classDef	4,paramPtr,inputPtr
;
; Input comes typically from Log2Norm which computes y = Log2(x)/32.
; This routine expects negative values which it scales by 32 in computing their exponential.
; Positive input values get pinned to an output value of 1.0.
;
; Memory:   X:              Y:
;   0       ---			a1
;   1   	---			a2
;	2		---			a0
;   3       L Input		R Input
;

	move	l:(inputPtr)-,ab
	neg		a		outputPtr,inputPtr

	clr		b				; clip input to negative values
	max		a,b

	aslm	#6,b,b			; get integer exponent value
	move	b2,y1

	move	#0,b2			; input was negative, neg inp is positive
	asr		b				; plant new sign bit (0) and shift back into mantissa
	neg		b				; recreate negative value between -1 and 0
	move	b,x0			; save into x0 for polynomial approximation
	;
	; Step 2 - Calculate EXP2 by polynomial approximation.
	; 8-bit accuracy
	;
	mpyr	x0,x0,a  			y:(paramPtr)+,y0	;x**2 get a1
	mpy		x0,y0,a		a,x1	y:(paramPtr)+,y0	;x1*x, mv x**2, get a2
	mac		x1,y0,a				y:(paramPtr)+,y0	;a2* x**2, get a0
	add		y0,a									;add a0
	asrm	y1,a,a									;shift down by integer exponent
	rnd		a										;round for ouput
	move	a,x:(outputPtr)		a,y:(inputPtr)		;send to both left and right channels
	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class DBConvert' editDate: '(10 May 2003 11:58:43 pm )'!;
; Convert Left and Right amplitudes to quadrature Log2(x^2 + y^2)/64
;   y = log2(x^2)/64 = x(dB)/192.7 = x(dB)/(10*log10(2)*64)
;
; DM/MCFA 05/01
;
DBConvert	classDef	4,paramPtr,inputPtr
;
; Compute y = Log2(left**2 + right**2)/64 for 2**(-23) <= Abs(left,right) < 1.0
; result y sent to Left and Right channel, -46/64 <= y < 0.0
;
; Memory:   X:              Y:
;   0       ---			a1
;   1   	---			a2
;	2		---			a0
;   4       L Input		R Input
;
	move	l:(inputPtr),ab
	move	a,x0
	mpy		x0,x0,a		b,y0
	mpy		y0,y0,b		outputPtr,inputPtr
	add		b,a
	bne		dbcnzero
	move	#-1.0,a
	bra		dbcout
dbcnzero
	;
	; Step 1 - Normalize A to get value between 0.5 and 1.0
	;
	clb		a,b
	normf	b1,a
	move	a,x0									;normalized number in x0
	;
	; Step 2 - Calculate LOG2 by polynomial approximation.
	; 8-bit accuracy
	;
	mpyr	x0,x0,a  			y:(paramPtr)+,y0	;x**2 get a1
	mpy		x0,y0,a		a,x1	y:(paramPtr)+,y0	;x1*x, mv x**2, get a2
	mac		x1,y0,a				y:(paramPtr)+,y0	;a2* x**2, get a0
	add		y0,a									;add a0
	;
	; Step 3 - Divide result by 64
	;
	aslm	#3,a,a
	move	b1,b0
	dec		b
	move	b0,a2									;new sign = characteristic
	asrm	#7,a,a
	rnd		a										;round result
dbcout
	;
	; Send result to both output channels
	;
	move	a,x:(inputPtr)		a,y:(outputPtr)
	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class DirectIIR' editDate: '(10 May 2003 11:59:16 pm )'!;
; MultiSection IIR Filter in Direct Form
;
; DM/MCFA 03/01
;
; set for max of 8 sections (= 2 + (5*nsections))
DirectIIR	classDef	42,paramPtr,inputPtr
;
; Filter the left input with a 2-pole Biquad IIR Filter.
; The left and right input values are summed to a mono signal.
; Uses the Direct Form to minimize rounding artifacts.
;
; Direct Form Difference Equation:
;  y[n] = 2*(b0/2 x[n] + b1/2 x[n-1] + b2/2 x[n-2] + a1/2 y[n-1] + a2/2 y[n-2])
;
; corresponding to the transfer function
;
; H(z) = (b0 + b1/z + b2/z^2)/(1 - a1/z - a2/z^2)
;
; As implemented here, following the guidelines in the Motorola App Notes,
; we use half-scaled coefficients with internal scale by *2 to avoid
; overflows in internal nodes of the filter.
;
; Memory:   X:              Y:
;   0       ---			nsections
;   1       x[n-1]		b0/2 ------- section 1
;	2		x[n-2]		b1/2
;	3       y[n-1]		b2/2
;   4       y[n-2]		a1/2
;   5		y2[n-1]		a2/2
;   6       y2[n-2]		b0/2 ------- section 2
;   7       y3[n-1]		b1/2 
;   ...
;	41		left input	right input
;
	move	l:(inputPtr),ab
	add		b,a		paramPtr,inputPtr							; mono sum of input samples, setup inputPtr for access to x[n]
	asr		a		x:(inputPtr)+,x1	y:(paramPtr)+,y1		; mono sum / 2, x[1] -> x1, nSections -> y1
	asr		a		x:(inputPtr)-,x0	y:(paramPtr)+,y0		; mono sum / 4 (for *2 scale mode), B0/2 -> y0

	ori		#$08,MR												; turn on scaling *2 mode

	do		y1,endOfLoopDirectIIR
	move	a,y1
	mpy		y1,y0,a		a,x:(inputPtr)+		y:(paramPtr)+,y0	; b0/2 * x[0] -> B, x[0] -> x[1], b1/2 -> y0
	mac		x1,y0,a		x1,x:(inputPtr)+	y:(paramPtr)+,y0	; B + b1/2 * x[1] -> B, x[2] -> x0, b2/2 -> y0

	mac		x0,y0,a		x:(inputPtr)+,x1	y:(paramPtr)+,y0	; B + b2/2 * x[2] -> B, y[1] -> x1, a1/2 -> y0
	mac		x1,y0,a		x:(inputPtr)-,x0	y:(paramPtr)+,y0	; B + a1/2 * y[1] -> B, y[2] -> x0, a2/2 -> y0
	macr	x0,y0,a							y:(paramPtr)+,y0	; B + a2/2 * y[2] -> B,             b0/2 -> y0
endOfLoopDirectIIR
	move	a,x:(inputPtr)+										; y[n] -> y[n-1]
	move	x1,x:(inputPtr)										; y[n-1] -> y[n-2]
	move	outputPtr,inputPtr
	move	a,x:(inputPtr)		a,y:(outputPtr)
	andi	#$0F3,MR											; turn off scaling *2 mode
	rts															; all done
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class FBDirectIIR' editDate: '(10 May 2003 11:59:47 pm )'!;
; MultiSection IIR Filter in Direct Form
;
; DM/MCFA 03/01
;
; set for max of 8 sections (= 2 + (5*nsections) + 1)
FBDirectIIR	classDef	43,paramPtr,inputPtr
;
; Filter the left input with a 2-pole Biquad IIR Filter.
; The left and right input values are summed to a mono signal.
; Uses the Direct Form to minimize rounding artifacts.
;
; Direct Form Difference Equation:
;  y[n] = 2*(b0/2 x[n] + b1/2 x[n-1] + b2/2 x[n-2] + a1/2 y[n-1] + a2/2 y[n-2])
;
; As implemented here, following the guidelines in the Motorola App Notes,
; we use half-scaled coefficients with internal scale by *2 to avoid
; overflows in internal nodes of the filter.
;
; Memory:   X:              Y:
;   0       prevOut		feedBack
;   1       x[n-1]		nsections
;   1       x[n-2]		b0/2 ------- section 1
;	2		y[n-1]		b1/2
;	3       y[n-2]		b2/2
;   4       y2[n-1]		a1/2
;   5		y2[n-2]		a2/2
;   6       y3[n-1]		b0/2 ------- section 2
;   7       y3[n-2]		b1/2 
;   ...
;	41		left input	right input
;
	move	l:(inputPtr),ab
	move	paramPtr,r0
	add		b,a		paramPtr,inputPtr							; mono sum of input samples, setup inputPtr for access to x[n]
	asr		a		x:(inputPtr)+,x1	y:(paramPtr)+,y1		; mono sum/2, yprev -> x1, feedback -> y1
	macr	x1,y1,a	x:(inputPtr)+,x1	y:(paramPtr)+,y1		; A + fb * y[-1] -> A, x[1] -> x1, nSections -> y1
	asr		a		x:(inputPtr)-,x0	y:(paramPtr)+,y0		; mono sum / 4 (for *2 scale mode), B0/2 -> y0

	ori		#$08,MR												; turn on scaling *2 mode

	do		y1,endOfLoopFBDirectIIR
	move	a,y1
	mpy		y1,y0,a		a,x:(inputPtr)+		y:(paramPtr)+,y0	; b0/2 * x[0] -> A, x[0] -> x[1], b1/2 -> y0
	mac		x1,y0,a		x1,x:(inputPtr)+	y:(paramPtr)+,y0	; A + b1/2 * x[1] -> A, x[2] -> x0, b2/2 -> y0

	mac		x0,y0,a		x:(inputPtr)+,x1	y:(paramPtr)+,y0	; A + b2/2 * x[2] -> A, y[1] -> x1, a1/2 -> y0
	mac		x1,y0,a		x:(inputPtr)-,x0	y:(paramPtr)+,y0	; A + a1/2 * y[1] -> A, y[2] -> x0, a2/2 -> y0
	macr	x0,y0,a							y:(paramPtr)+,y0	; A + a2/2 * y[2] -> A,             b0/2 -> y0
endOfLoopFBDirectIIR
	move	a,x:(inputPtr)+										; y[n] -> y[n-1]
	move	x1,x:(inputPtr)										; y[n-1] -> y[n-2]
	move	outputPtr,inputPtr
	move	a,x:(inputPtr)		a,y:(outputPtr)
	move	a,x:(r0)											; save feedback output
	andi	#$0F3,MR											; turn off scaling *2 mode
	rts															; all done
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class HWRect' editDate: '(11 May 2003 12:00:11 am )'!;
; Limit Input to positive values = Half-Wave Rectifier
;
; DM/MCFA 07/01
;
HWRect	classDef	2,paramPtr,inputPtr
;
; Treat both left and right channels.
;
; Memory:   X:              Y:
;   0       L Input		R Input
;
	move	l:(inputPtr),ab
	move	b,x0

	clr		b
	max		a,b
	move	b,y0

	move	x0,a
	clr		b
	max		a,b

	move	y0,a
	move	ab,l:(outputPtr)
	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class Log2Norm' editDate: '(11 May 2003 12:00:42 am )'!;
; Normalized Base 2 Logarithm of Left Input
;
; DM/MCFA 05/01
;
Log2Norm	classDef	4,paramPtr,inputPtr
;
; Compute y = Log2(x)/32 for x in Left channel, 2**(-23) <= x < 1.0
; result y sent to Left channel, -23/32 <= y < 0.0
;
; Memory:   X:              Y:
;   0       ---			a1 = 0.9981958
;   1   	---			a2 = -0.3372223
;	2		---			a0 = -0.6626105
;   4       L Input		R Input
;
	move	l:(inputPtr),ab
	;
	; Step 1 - Normalize A to get value between 0.5 and 1.0
	;
	clb		a,b
	normf	b1,a
	move	a,x0									;normalized number in x0
	;
	; Step 2 - Calculate LOG2 by polynomial approximation.
	; 8-bit accuracy
	;
	mpyr	x0,x0,a  			y:(paramPtr)+,y0	;x**2 get a1
	mpy		x0,y0,a		a,x1	y:(paramPtr)+,y0	;x1*x, mv x**2, get a2
	mac		x1,y0,a				y:(paramPtr)+,y0	;a2* x**2, get a0
	add		y0,a									;add a0
	;
	; Step 3 - Divide result by 32
	;
	aslm	#3,a,a
	move	b1,b0
	dec		b
	move	b0,a2									;new sign = characteristic
	asrm	#6,a,a
	rnd		a			outputPtr,inputPtr			;round result
	;
	; Send result to both output channels
	;
	move	a,x:(inputPtr)		a,y:(outputPtr)
	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class LRAbsMax' editDate: '(11 May 2003 12:01:06 am )'!;
; Left/Right Channel Abs Max
;
; DM/MCFA 04/01
;
LRAbsMax	classDef	2,paramPtr,inputPtr
;
; Compute the maximum of the absolute values of left and right inputs.
; Output same result to both left and right output channels.
;
; Memory:   X:              Y:
;   0       ---			---
;   1       L Input		R Input
;
	move	l:(inputPtr),ab
	abs		a		outputPtr,inputPtr
	abs		b
	max		a,b
	move	b,x:(inputPtr)	b,y:(outputPtr)
	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class MLimit' editDate: '(11 May 2003 12:01:41 am )'!;
; Limit Input to values above some value
;
; DM/MCFA 04/01
;
MLimit	classDef	2,paramPtr,inputPtr
;
; Treat both left and right channels.
; Parameter should be the desired limit value.
;
; Memory:   X:              Y:
;   0       ---			limit
;   1       L Input		R Input
;
	move	l:(inputPtr),ab
	move	b,x0
	move	y:(paramPtr)+,x1

	move	x1,b
	max		a,b
	move	b,y0

	move	x0,a
	move	x1,b
	max		a,b

	move	y0,a
	move	ab,l:(outputPtr)
	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class PLimit' editDate: '(11 May 2003 12:02:06 am )'!;
; Limit Input to values below some value
;
; DM/MCFA 04/01
;
PLimit	classDef	2,paramPtr,inputPtr
;
; Treat both left and right channels.
; Parameter should be the negative of the desired limit value.
;
; Memory:   X:              Y:
;   0       ---			limit negated
;   1       L Input		R Input
;
	move	l:(inputPtr),ab
	move	b,x0
	move	y:(paramPtr)+,x1

	move	x1,b
	neg		a
	max		a,b
	neg		b
	move	b,y0

	move	x0,a
	move	x1,b
	neg		a
	max		a,b
	neg		b

	move	y0,a
	move	ab,l:(outputPtr)
	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class TriggerToGate' editDate: '(11 May 2003 12:02:37 am )'!;
; Convert Trigger to Fixed Duration Gate
;
; Output is zero until a zero or negative input arrives. Then it becomes -1.0
; until a positive > 0 trigger arrives. Then the gate goes to 1.0 until
; the duration in samples expires, at which time it resets to zero.
; Trigger events arriving during the gate period are ignored. This gate is a
; one-shot for the indicated duration, and can only be retriggered after the
; gate period expires.
;
; DM/MCFA 03/01
;
TriggerToGate	classDef	2,paramPtr,inputPtr
;
;
; Memory:   X:              Y:
;   0       currentCount  nSampDuration
;	1		left input	  right input
;
	move	l:(inputPtr),ab
	tfr		a,b				outputPtr,inputPtr	
    move    x:(paramPtr),a
	subi	#1,a					;; decr count
	beq		tg_awaitNeg				;; if zero -> wait for zero or negative input
	bmi		tg_awaitPos				;; if negative -> wait for positive > 0 input
	move	a,x:(paramPtr)			;; save decr count
	move	#1.0-1,a
	move	a,x:(outputPtr)		a,y:(inputPtr)		;; send out decr count
	rts
tg_awaitNeg											;; a = 0 here
	tst		b										;; if input <= 0 then save zero in counter
	bgt		tg_done_await_neg						;; for next decr
	move	a,x:(paramPtr)							;; save zero in counter
													;; send out negative pulse till positive trigger arrives
	move  #-1.0,a												;; (doing it here guarantees at least one negative pulse)
tg_done_await_neg
	move	a,x:(outputPtr)		a,y:(inputPtr)		;; send out either zero or -1
	rts
tg_awaitPos
	tst		b										;; a = -1 here
	bgt		tg_setupCounting						;; if input > 0 then we have a trigger
	move	#-1.0,a
	move	a,x:(outputPtr)		a,y:(inputPtr)		;; else send out -1
	rts
tg_setupCounting
	move	y:(paramPtr),a							;; refresh counter with max count
	move	a,x:(paramPtr)	
	move	#1.0-1,a					
	move	a,x:(outputPtr)		a,y:(inputPtr)		;; send out max count
	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'Additional Microsound Classes' editDate: '(13 May 2003 2:19:05 pm )'!;
; Microsounds file for adding new assembly language to Kyma.

; Code that is NOT wombat, These Microsounds will only load on a Capybara-320 or Capybara-320A.
; Code that is wombat , These Microsounds will load only on the cappy 66 or lower
; Code that is not defimed will load on both.

	dsp56300
	extendedMemory	$5000
	org				p:AppendUser


; Petes DSPCode 5.0

	include	'class ReadMemory'
	include	'class WriteMemory'
	include	'class ExponentialDecay'
	include	'class RCL'
	include	'class RCL48'
	include	'class TryOut'
	include	'class MixerWithWrap'
	include	'class MixerWithFlip'
	include	'class AsLogic'
	include	'class DisableGate'
	include	'class TrackAndHold'
	include	'class TrackAndPreHold'
	include	'class GateToTrigger'
	include	'class RampGenerator'
	include	'class Ramp2'
	include	'class Ramp3'
	include	'class CounterRamp'
	include	'class SlewRateLimiter'
	include	'class MixerAndGainWithWrap'
	include	'class MultiplyWithGain'
	include	'class Negate'
	include	'class Switch'
	include	'class OneSamp'
	include	'class AndGate'
	include	'class OrGate'
	include	'class ExOrGate'
	include	'class SRFlipFlop'
	include	'class Toggle'
	include	'class SRRamp'
	include	'class PulseStretcherFreq'
	include	'class PulseStretcherTime'
#if	wombat
	include	'class Divide'
#else
	include	'class Divide320'
	include	'class StepWriter'
	include	'class MultiTapDelay'

;;
;; The following microsounds from David McClain, The Euterpe Group, LLC
;;
	include 'class ALog2'
	include 'class DBConvert'
	include 'class DirectIIR'
	include	'class FBDirectIIR'
	include 'class HWRect'
	include 'class Log2Norm'
	include	'class LRAbsMax'
	include	'class MLimit'
	include 'class PLimit'
	include 'class TriggerToGate'

#endif

	end
! !