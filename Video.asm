.module Video

; Port defnitions
Data = $BE
Control = $BF

; Local copy of registers
Registers = allocVar(11)

Reset: ; Preload the VDP registers with sensible data.
	di
	
	ld hl,ResetData 
	
	ld b,0
	ld c,11
	
-:	ld a,(hl)
	call SetRegister
	inc hl
	inc b
	dec c
	jr nz,-
	
	jp ClearAll

ResetData:
.db $04
.db $80
.db $FF
.db $FF
.db $FF
.db $FF
.db $FF
.db $FF
.db $00
.db $00
.db $FF
		
SetWriteAddress: ; Set the VRAM pointer to the address in HL.
	di
	push af
	ld a,l
	out (Control),a
	ld a,h
	or %01000000
	out (Control),a
	pop af
	ret

SetReadAddress: ; Set the VRAM pointer to a read address.
	di
	push af
	ld a,l
	out (Control),a
	ld a,h
	out (Control),a
	pop af
	ret

SetRegister: ; Set register B to value A.

	; Store a local copy
	push hl
	push de
	ld e,b
	ld d,0
	ld hl,Registers
	add hl,de
	ld (hl),a
	pop de
	pop hl
	
	out (Control),a
	ld a,%10000000
	or b
	out (Control),a
	ret

GetRegister: ; Retrieve the value of register B in A.
	push hl
	push de
	ld e,b
	ld d,0
	ld hl,Registers
	add hl,de
	ld a,(hl)
	pop de
	pop hl
	ret

SynchroniseRegisters:
	ld b,11
-:	push bc
	dec b
	call GetRegister
	nop
	call SetRegister
	pop bc
	djnz -
	ret

EnableRegisterBits: ; Set the register B bits with the set values in A (bitwise OR).
	push bc
	ld c,a
	call GetRegister
	or c
	call SetRegister
	pop bc
	ret

DisableRegisterBits: ; Clear the register B bits with the clear values in A (bitwise AND).
	push bc
	ld c,a
	call GetRegister
	and c
	call SetRegister
	pop bc
	ret

GotoPalette: ; Set the CRAM pointer to colour a.
	out (Control),a
	ld a,$C0
	out (Control),a
	ret
	
ClearAll:
	ld hl,$0000
	call SetWriteAddress
	ld hl,16*1024
-:	xor a
	out (Data),a
	dec hl
	ld a,h \ or l
	jr nz,-
	call GotoPalette
	xor a
	ld b,64
-:	out (Data),a
	djnz -
	ret

DisplayOn:
	ld a,%01000000
	ld b,$01
	jr EnableRegisterBits

DisplayOff:
	ld a,%10111111
	ld b,$01
	jr DisableRegisterBits

EnableFrameInterrupt:
	ld a,%00100000
	ld b,$01
	jr EnableRegisterBits

DisableFrameInterrupt:
	ld a,%11011111
	ld b,$01
	jr DisableRegisterBits

.endmodule