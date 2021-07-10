.module Video

; Port defnitions
Data = $BE
Control = $BF

Video.OutData = $08

; Local copy of registers
.var ubyte[11] Registers

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
	
	call ClearQueue
	
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
	in a,(Control)
	ld a,l
	out (Control),a
	ld a,h
	or %01000000
	out (Control),a
	ret

SetReadAddress: ; Set the VRAM pointer to a read address.
	di
	in a,(Control)
	ld a,l
	out (Control),a
	ld a,h
	out (Control),a
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

; Data queue system
QueueCapacity = 24

.var ubyte[QueueCapacity * 3] Queue
.var ubyte QueueSize
.var uword QueuePointer

ClearQueue:
	di
	ld hl,Queue
	ld (QueuePointer),hl
	xor a
	ld (QueueSize),a
	ret

; In: DE = address, A = value
Enqueue:
	push af
-:	ld a,(QueueSize)
	cp QueueCapacity
	jr nz,+
	ei
	halt
	jr -
+:	di
	inc a
	ld (QueueSize),a
	pop af
	push hl
	
	ld hl,(QueuePointer)
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ld (hl),a
	inc hl
	
	ld (QueuePointer),hl
	
	pop hl
	ei
	ret

WaitForEmptyQueue:
	ld a,(QueueSize)
	or a
	ret z
	ei
	halt
	jr WaitForEmptyQueue

FlushQueue:
	ld a,(Video.QueueSize)
	or a
	ret z

	push bc
	push de
	push hl
	
	ld b,a
	ld hl,Queue
	ld (QueuePointer),hl
	
-:	ld a,(hl)
	inc hl
	out (Control),a
	
	ld a,(hl)
	inc hl
	out (Control),a
	
	ld a,(hl)
	inc hl
	out (Data),a
	
	djnz -
	
	xor a
	ld (QueueSize),a

+:	pop hl
	pop de
	pop bc
	ret

.endmodule