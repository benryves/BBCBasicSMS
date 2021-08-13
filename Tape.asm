.module Tape

; Basic format:
; - 0 bit = one cycle of 1200Hz.
; - 1 bit = two cycles of 2400Hz.

; Data byte:
; - 0 dl....dh 1

InputPort = $DD
InputBit  = 3

HalfWaveLengthThreshold = 20

; ---------------------------------------------------------
; GetHalfWaveLength -> Gets the length of half of a wave.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  b = length of the half wave.
;           z and c set if the bit timed out. 
; Destroys: af, bc.
; ---------------------------------------------------------
GetHalfWaveLength:
	di
	
	; Count the length of the half-wave in B.
	ld b,0
	
	; We need to see where the wave changes, so remember the initial state.
	in a,(InputPort)
	ld c,a
	
-:	in a,(InputPort)   ; 11
	xor c              ; 4
	and 1 << InputBit  ; 7
	ret nz             ; 11/5
	inc b              ; 4
	jr nz,-            ; 12/7
	scf
	ret

GetBit:
	call GetHalfWaveLength
	ret z ; Time out.
	
	ld a,b
	cp HalfWaveLengthThreshold
	jr c,GetBit.1

GetBit.0: ; 1x 1200Hz
	
	call GetHalfWaveLength
	
	scf
	ccf
	ret


GetBit.1: ; 2x 2400Hz
	
	call GetHalfWaveLength
	ret z
	
	call GetHalfWaveLength
	ret z
	
	call GetHalfWaveLength
	
	scf
	ret

GetByte:
	
	; Get the start bit.
	call GetBit
	ret z
	
	jr c,GetByte ; We need a 0!
	
	; Fetch 8 data bits.
	ld bc,8<<8
	
-:	push bc
	call GetBit
	pop bc
	ret z
	
	rr c
	djnz -

	; We now have our byte!
	; Get the stop bit.
	
	push bc
	call GetBit
	pop bc
	ret z
	
	ld a,c
	ret c
	
	; If it's not a 1 bit, it's not a stop bit!
	cp c
	ret

.endmodule