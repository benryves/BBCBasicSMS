.module Serial

; Pins:
; RxD = 2 Port B Down ($DC.7)
; TxD = 7 Port B TH ($3F.7)

; Z80 CPU Frequency = 3546893 Hz (PAL), 3579540 Hz (NTSC)
; Assume it's 3563217
; 38400 :    93
; 19200 :   186
;  9600 :   371
;  4800 :   742
;  2400 :  1484
;  1200 :  2969
;   600 :  5939
;   300 : 11877

.var ubyte[3] HalfDelay
.var ubyte[3] BitDelay

Reset:
	
	; Set TxD as an output
	di
	ld a,(IOControl)
	and %11110111 ; TH = output
	or  %10000000 ; TH = high
	ld (IOControl),a
	out ($3F),a

	; Reset to 9600 baud
	
	ld hl,9600
	; Fall-through to SetRate

SetRate:

	push hl
	push ix
	push de
	
	; Start at full speed.
	
	ld de,38400
		
	ld a,$2B ; DEC HL
	ld (BitDelay),a
	ld a,$C9 ; RET
	ld (BitDelay+1),a
	ld (HalfDelay+0),a
	
	; Table of bit delays in case we need to slow down.
	
	ld ix,BitDelays
	ld b,BitDelays.Count
	
-:	; Does HL = the selected rate?
	push hl
	or a
	sbc hl,de
	ld a,h
	or l
	pop hl
	jr z,+
	
	; No, so we need to slow the rate.
	
	; Divide DE by 2.
	srl d
	rr e
	
	push hl
	
	; First, double the delay of the half bit delay by copying the old full delay.
	ld a,(BitDelay)
	ld (HalfDelay),a
	
	ld hl,(BitDelay+1)
	ld (HalfDelay+1),hl
	
	; Now, make the full delay the next value in the delay function list.
	ld a,$C3 ; JP
	ld (BitDelay),a
	ld l,(ix+0)
	ld h,(ix+1)
	ld (BitDelay+1),hl
	
	inc ix
	inc ix
	
	pop hl
	
	djnz -
	
+:	pop de
	pop ix
	pop hl
	ret



SendByte:
	di
	
	ld d,a
	ld b,10 ; +1 for initial DJNZ
	
	; Send the start bit
	
	ld a,(IOControl)
	and %01110111
	ld e,a
	
	out ($3F),a
	
	; Send the 8 data bits + stop bit
	djnz SendLoop ; forward djnz, easy way to balance...

SendLoop:
	call BitDelay
	
	; Waste time due to idiocy
	or 0          ; 7
	
	ld a,e        ; 4
	add a,a       ; 4
	scf           ; 4
	rr d          ; 8
	rr a          ; 8
	out ($3F),a   ; 11
	
	djnz SendLoop ; 13
	ret

GetByte:

	di

	; Make sure RxD is high (idle state)

-:	in a,($DC)    ; 11
	add a,a       ; 4
	jr c,+        ; 12/7
	
	; Force NZ to denote error.
	xor a
	dec a
	ret
	
+:

	; Wait for RxD to go low (start bit)
	
	ld bc,0       ; 10
	
-:	in a,($DC)    ; 11
	add a,a       ; 4
	jr nc,+       ; 12/7
	
	djnz -        ; 13/8
	dec c         ; 4
	jr nz,-       ; 12/7
	
	; We've timed out, force NZ to denote error.
	xor a
	inc a
	ret

+:
	
	; Now we need to delay for around half a bit.
	; This is so we samples in the middle of the bit, not at the transitions.

	call HalfDelay

	; Dummy code to slightly better mirror the loop below and match the timing.
	
	or 0          ; 7
	
	inc hl        ; 6
	dec hl        ; 6
	nop           ; 4

	inc hl        ; 6
	dec hl        ; 6
	nop           ; 4

	ld b,8        ; 7

-:	call BitDelay
	
	in a,($DC)    ; 11
	
	; Waste time so the receive loop takes the same amount of time as the send loop.
	
	or 0          ; 7
	inc hl        ; 6
	dec hl        ; 6
	nop           ; 4
	
	; Carry on processing bits!
	
	add a,a       ; 4
	rr c          ; 8
	djnz -        ; 13
	
	; We now have the data in C.
	; Copy to A and set Z flag.
	
	ld a,c
	cp a
	
	ret

BitDelays:
	.dw BitDelay.19200
	.dw BitDelay.9600
	.dw BitDelay.4800
	.dw BitDelay.2400
	.dw BitDelay.1200
	.dw BitDelay.600
	.dw BitDelay.300
BitDelays.Count = ($-BitDelays)/2

BitDelay.19200:
	push bc
	ld b,4
-:	djnz -
	inc bc
	nop
	nop
	pop bc
	ret

BitDelay.9600:
	push bc
	ld bc,13
	call delay
	inc bc
	nop
	nop
	pop bc
	ret
	
BitDelay.4800:
	push bc
	ld bc,42
	call delay
	nop
	nop
	pop bc
	ret

BitDelay.2400:
	push bc
	ld bc,98
	call delay
	inc (hl)
	dec (hl)
	pop bc
	ret
	
BitDelay.1200:
	push bc
	ld bc,213
	call delay
	inc hl
	dec hl
	pop bc
	ret

BitDelay.600:
	push bc
	ld bc,441
	or 0
	or 0
	nop
	call delay
	pop bc
	ret
	
BitDelay.300:
	push bc
	ld bc,898
	call delay
	or 0
	or 0
	pop bc
	ret

delay: ; 61+13*BC
	ld a,b ; 4
	or a ; 4
	jr nz,long_delay_loop ; 12/7
	or c ; 4
	nop ; 4
	jr nz,short_delay ; 12/7
	nop ; 4
	ret ; 10
long_delay_loop:
	neg ; 8
	ld a,(bc) ; 7
	ld a,b ; 4
	ld b,251 ; 7
	call short_delay_loop ; 17 + 4 + 13*(B-1) + 8 + 10
	ld b,a ; 4
	djnz long_delay_loop ; 13/8
	ld a,c ; 4
	or a ; 4
	jr nz,short_delay ; 12/7
	nop ; 4
	ret ; 10
short_delay:
	ld b,c ; 4
short_delay_loop:
	djnz short_delay_loop ; 13/8
	ret ; 10
	
.endmodule