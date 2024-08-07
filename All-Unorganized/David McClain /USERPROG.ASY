
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class CanonBiquadIIR' editDate: '(22 March 2001 12:04:48 am )'!;
; MultiSection Canonical Biquad IIR Filter
;
; DM/MCFA 03/01
;

; set for max of 8 sections (= 2 + (5*nsections))
CanonBiquadIIR	classDef	42,paramPtr,inputPtr
;
; Filter the left input with a 2-pole Biquad IIR Filter.
; The left and right input values are summed to a mono signal.
; Uses the Canonical Biquad Form to avoid internal node overflows.
;
; Direct Form Difference Equation:
;  y[n] = b0 x[n] + b1 x[n-1] + b2 x[n-2] + a1 y[n-1] + a2 y[n-2]
;
; Canonical Decomposition of Biquad Form:
;  w[n] = 2*(x[n]/2 + a1/2 w[n-1] + a2/2 w[n-2])
;  y[n] = 2*(b0/2 w[n] + b1/2 w[n-1] + b2/2 w[n-2])
;
; As implemented here, following the guidelines in the Motorola App Notes,
; we use half-scaled coefficients with internal scale by *2 to avoid
; overflows in internal nodes of the filter.
;
; Memory:   X:              Y:
;   0       w(n-1)         nsections
;   1       w(n-2)          a1/2 ------- section 1
;	2		w2(n-1)         a2/2
;	3       w2(n-2)         b0/2
;   4       ...             b1/2
;   5		                b2/2
;   6                       a1/2 ------- section 2
;   7                       a2/2 
;   ...
;   41		left input		right input
;
	move	l:(inputPtr),x										; left input -> x1, right input -> x0
	tfr		x0,b		paramPtr,inputPtr						; B = right channel input, copy paramPtr for w access
	add		x1,b	    y:(paramPtr)+,y1						; form mono sum input sample, w[n-1] -> x0, nsections -> y1
	asr		b													; B = (left + right)/2
	asr		b			x:(inputPtr)+,x0	y:(paramPtr)+,y0	; B = x[n]/2, w[n-1] -> x0, A1/2 -> y0

	ori		#$08,MR												; turn on scaling *2 mode	
	do		y1,endOfLoopCBQ
	mac		y0,x0,b		x:(inputPtr)-,x1	y:(paramPtr)+,y0 	; B + A1/2 w[n-1] -> B, w[n-2] -> x1, A2/2 -> y0
	macr	y0,x1,b							y:(paramPtr)+,y0	; B + A2/2 w[n-2] -> B = w[n]/2, B0/2 -> y0
	move	b,y1												; w[n] -> y1
	mpy		y0,y1,b		b,x:(inputPtr)+		y:(paramPtr)+,y0	; b0/2 w[n] -> B, w[n] -> w[n-1], B1/2 -> y0
	mac		y0,x0,b		x0,x:(inputPtr)+ 	y:(paramPtr)+,y0	; B + b1/2 w[n-1] -> B, w[n-1] -> w[n-1], B2/2 -> y0
	mac		y0,x1,b		x:(inputPtr)+,x0	y:(paramPtr)+,y0	; B + b2/2 w[n-2] -> B, w[n-1] -> x0, A1/2 -> y0 (next stage)
endOfLoopCBQ
	rnd		b			outputPtr,inputPtr
	move	b,x:(inputPtr)	b,y:(outputPtr)						; send result to both output channels
	andi	#$0F3,MR											; turn off scaling mode
	rts															; all done

	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class FBDirectIIR' editDate: '(15 April 2001 7:24:56 pm )'!;
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class LRAbsMax' editDate: '(15 April 2001 11:11:51 pm )'!;
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class LRMax' editDate: '(30 May 2001 8:35:18 pm )'!;
; Left/Right Channel Max
;
; DM/MCFA 04/01
;
LRMax	classDef	2,paramPtr,inputPtr
;
; Compute the maximum of left and right inputs.
; Output same result to both left and right output channels.
;
; Memory:   X:              Y:
;   0       ---			---
;   1       L Input		R Input
;
	move	l:(inputPtr),ab
	max		a,b
	move	b,x:(inputPtr)	b,y:(outputPtr)
	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class PLimit' editDate: '(30 May 2001 10:28:35 pm )'!;
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class MLimit' editDate: '(30 May 2001 10:30:49 pm )'!;
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class ALog2' editDate: '(30 May 2001 11:04:43 pm )'!;
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class Crescendo' editDate: '(31 May 2001 10:35:23 am )'!;
; Convert Left and Right amplitudes to quadrature Log2
;   y = log2(x^2)/64 = x(dB)/192.7
;
; DM/MCFA 05/01
;
Crescendo	classDef	9,paramPtr,inputPtr
;
; This is the Crescendo Algorithm given an incoming specturm of real and imaginary components.
; It computes the required spectral gains to achieve the compression needed to overcome hearing loss.
;
; Memory:   X:              Y:
;   0       ---			a1
;   1   	---			a2
;	2		---			a0
;	3		---			b1
;	4		---			b2
;	5		---			b0
;	6		hearing correction input
;	7		loudness correction input
;   8       L Input		R Input
;

	move	l:(inputPtr)-,ab
	move	a,x0
	mpy		x0,x0,a		b,y0
	mpy		y0,y0,b
	add		b,a
	bne		crnzero
	move	#-1.0,a
	bra		crcmprss	
	;
	; Step 1 - Normalize A to get value between 0.5 and 1.0
	;
crnzero
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
	; Step 3 - Divide result by 64 =>  log2(re^2 + im^2)/64
	;
	aslm	#3,a,a
	move	b1,b0
	dec		b
	move	b0,a2									;new sign = characteristic
	asrm	#7,a,a									; rlog -> a
	;
	;
crcmprss
	;;
	;; Adjust to loudness scale and compute gain compression
	;;
	move	x:(inputPtr)-,x0						;loudness value/32*0.1661 -> x0
	add		x0,a	x:(inputPtr)-,x0				;inp loudness -> a, hearing correction -> x0
	;;
	;; threshold for max gain
	;;
	clr		b										; keep result to negative values
	neg		a										; and greater than -60 dB threshold
	max		a,b
	move	#-0.31143,a								;; -0.31143 = -60 dB
	neg		b
	max		a,b
	;;
	;; multiply by hearing correction factor
	;;
	move	b,x1
	mpy		x1,x0,a
	;;
	;; convert back to gain factor = gain/64
	;; at this point we have log2(gain)/32
	;; plan on gain differential across spectrum of 36 dB max (= x64 = 6 bits)
	;;
	subi	#0.1875,a		; = 6/32 back off positive gain to effective attenuation (6 bits)
;	bmi		crcnv2			; probably unnecessary here...
;	move	#1.0,a
;	move	outputPtr,inputPtr
;	bra		crout
crcnv2
	neg		a
	aslm	#6,a,a
	move	a2,y1
	move	#0,a2
	asr		a
	neg		a
	move	a,x0

	;
	; Step 2 - Calculate EXP2 by polynomial approximation.
	; 8-bit accuracy
	;
	mpyr	x0,x0,a  				y:(paramPtr)+,y0	;x**2 get a1
	mpy		x0,y0,a		a,x1		y:(paramPtr)+,y0	;x1*x, mv x**2, get a2
	mac		x1,y0,a					y:(paramPtr)+,y0	;a2* x**2, get a0
	add		y0,a										;add a0
	asrm	y1,a,a
	rnd		a			outputPtr,inputPtr
crout
	;
	; Send result to Left and Right outputs
	;
	move	a,x:(inputPtr)	a,y:(outputPtr)
	rts
	endClass
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class DBConvert' editDate: '(31 May 2001 10:38:14 am )'!;
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class HWRect' editDate: '(24 July 2001 10:50:09 pm )'!;
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class TriggerToGate' editDate: '(18 January 2002 2:45:14 pm )'!;
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class Log2Norm' editDate: '(26 January 2002 2:25:23 pm )'!;
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'Additional Microsound Classes' editDate: '(1 March 2003 11:48:34 am )'!;
; Sample microsounds file for adding new assembly language to Kyma.
;

	dsp56300
	extendedMemory	$5000
	org				p:AppendUser

	include 'class DirectIIR'
	;include	'class CanonBiquadIIR'
	include	'class FBDirectIIR'

	include	'class LRAbsMax'

	include 'class Log2Norm'
	include 'class ALog2'
	include 'class DBConvert'
	include 'class Crescendo'
	;include 'class LRMax'
	include 'class PLimit'
	include	'class MLimit'
	include 'class HWRect'
	include 'class TriggerToGate'

	include	'class ReadMemory'
	include	'class WriteMemory'

	end
! !
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class ReadMemory' editDate: '(1 March 2003 11:49:47 am )'!ReadMemory	classDef	2,paramPtr,inputPtr
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class WriteMemory' editDate: '(1 March 2003 11:49:58 am )'!WriteMemory	classDef	3,paramPtr,inputPtr
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
!DSP56000ProgramOrganizer readProgramSegmentFor: 'class DirectIIR' editDate: '(20 April 2003 5:38:43 pm )'!;
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