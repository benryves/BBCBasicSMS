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
Header.DataCRC          = 19

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
	
	; Get the CRC
	call GetByte
	ret z
	ld (ix+Header.DataCRC+0),a
	
	call GetByte
	ret z
	ld (ix+Header.DataCRC+1),a
	
	inc a ; Set NZ
	ret
	
	; CRC routine from http://regregex.bbcmicro.net/crc-code.htm
	
        ; CRC-16/XMODEM for 8080/Z80
        ; On entry HL = old CRC, A = byte
        ; On exit HL = new CRC, A,B,C undefined

                                ; Ts  M/code    8080 assembly
crc16_xmodem_f:
        XOR     H               ;  4  AC        XRA     H
        LD      B,A             ;  4  47        MOV     B,A
        LD      C,L             ;  4  4D        MOV     C,L
        RRCA                    ;  4  0F        RRC
        RRCA                    ;  4  0F        RRC
        RRCA                    ;  4  0F        RRC
        RRCA                    ;  4  0F        RRC
        LD      L,A             ;  4  6F        MOV     L,A
        AND     0FH             ;  7  E6 0F     ANI     0FH
        LD      H,A             ;  4  67        MOV     H,A
        XOR     B               ;  4  A8        XRA     B
        LD      B,A             ;  4  47        MOV     B,A
        XOR     L               ;  4  AD        XRA     L
        AND     F0H             ;  7  E6 F0     ANI     F0H
        LD      L,A             ;  4  6F        MOV     L,A
        XOR     C               ;  4  A9        XRA     C
        ADD     HL,HL           ; 11  29        DAD     H
        XOR     H               ;  4  AC        XRA     H
        LD      H,A             ;  4  67        MOV     H,A
        LD      A,L             ;  4  7D        MOV     A,L
        XOR     B               ;  4  A8        XRA     B
        LD      L,A             ;  4  6F        MOV     L,A
        RET                     ; 10  C9        RET

        ; 115 T-states, 25 bytes
CRC16:
	ld hl,0
-:	ld a,(de)
	push bc
	call crc16_xmodem_f
	pop bc
	inc de
	dec bc
	ld a,b
	or c
	jr nz,-
	ret

.endmodule