
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class DirectIIR' editDate: '(18 January 2002 5:37:15 am )'!;
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
; H(z) = (b0 + b1/z + b2/z^2)/(0.5 - a1/z - a2/z^2)
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'Additional Microsound Classes' editDate: '(29 April 2002 4:25:27 pm )'!;
; Sample microsounds file for adding new assembly language to Kyma.
;

	dsp56300
	extendedMemory	$5000
	org				p:AppendUser

	include 'class DirectIIR'
	end
! !