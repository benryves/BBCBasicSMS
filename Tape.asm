.module Tape

; Basic format:
; - 0 bit = one cycle of 1200Hz.
; - 1 bit = two cycles of 2400Hz.

; Data byte:
; - 0 dl....dh 1

InputPort = $DD
InputBit  = 3

HalfWaveLengthThreshold = 20

Header.LoadAddress      = 0
Header.ExecutionAddress = 4
Header.BlockNumber      = 8
Header.DataBlockLength  = 10
Header.BlockFlag        = 12
Header.NextFileAddress  = 13
Header.CRC              = 17

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

; ---------------------------------------------------------
; GetBit -> Gets a bit from the tape.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  c = the bit value.
;           z set if the bit timed out. 
; Destroys: af, bc.
; ---------------------------------------------------------
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
	
	; The initial bit might have been a trailing 2400Hz half-wave.
	; If we now receive a 1200Hz half-wave, we should ignore the previous 2400Hz half-wave.
	ld a,b
	cp HalfWaveLengthThreshold
	jr nc,GetBit.0
	
	call GetHalfWaveLength
	ret z
	
	call GetHalfWaveLength
	
	scf
	ret

; ---------------------------------------------------------
; GetByte -> Gets a byte from the tape.
; ---------------------------------------------------------
; Outputs:  a = the byte value.
;           z set if the byte timed out. 
; Destroys: af, bc.
; ---------------------------------------------------------
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

; ---------------------------------------------------------
; GetBlock -> Gets a complete block from the tape.
; ---------------------------------------------------------
; Inputs:   hl = pointer to storage for the block header.
;           de = pointer to storage for the block data.
; Outputs:  ix = address of block data structure.
;           z set if there was a problem receiving.
; Destroys: af, bc, de, hl.
; ---------------------------------------------------------
GetBlock:
	
	; Wait for the synchronisation byte.
-:	push bc
	call GetByte
	pop bc
	jr z,-
	cp $2A
	jr nz,-
	
	; Now read the header!

	; Variable length filename, NUL terminated.
-:	call GetByte
	ret z
	ld (hl),a
	inc hl
	or a
	jr nz,-
	
	push hl
	
	; 19 bytes of further data.
	ld b,19
	
-:	push bc
	call GetByte
	pop bc
	ret z
	ld (hl),a
	inc hl
	djnz -
	
	pop ix

	; Read the data block length.
	ld c,(ix+Header.DataBlockLength+0)
	ld b,(ix+Header.DataBlockLength+1)
	
	ld a,b
	or c
	jr nz,ReadData
	inc a ; Set NZ
	ret

ReadData:
	push bc
	call GetByte
	pop bc
	ret z
	ld (de),a
	inc de
	dec bc
	ld a,b
	or c
	jr nz,ReadData
	
	inc a ; Set NZ
	ret
	

.endmodule