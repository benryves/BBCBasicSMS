.module VDU

.module Modes

.include "Text.asm"
.include "Mode4.asm"

Count = 2

.endmodule

.var ubyte[3] PutMap
.var ubyte[3] Scroll

Reset:
	xor a
SetMode:
	di
	push af
	
	ld a,$C3 ; JP
	ld (PutMap),a
	ld (Scroll),a
	
	; Reset all video settings to their defaults.
	call Video.Reset
	
	pop af
	
	; Mode-specific initialisation.
	call SetModeInitialize
	
	; Move to the top-left of the screen.
	call HomeUp
	
	; Screen on, enable frame interrupts.
	call Video.DisplayOn
	call Video.EnableFrameInterrupt
	ei
	
	ret

SetModeInitialize:
	or a
	jp z,Modes.Text.Initialise
	dec a
	jp z,Modes.Mode4.Initialise
	ret

FontTileIndex = 0
FontCharOffset = FontTileIndex-' '

.var ubyte CurRow, CurCol
.var ubyte MinRow, MaxRow, MinCol, MaxCol

HomeUp:
	ld a,(MinCol)
	ld (CurCol),a
	ld a,(MinRow)
	ld (CurRow),a
	ret
	

PutChar:
	cp '\r'
	jr nz,+
	ld a,(MinCol)
	ld (CurCol),a
	ret

+:	cp '\n'
	jr nz,+
	
	ld a,(MinCol)
	ld (CurCol),a
	jr NewLine

+:	call PutMap
	; Fall-through to CursorRight

CursorRight:
	ld a,(CurCol)
	inc a
	push bc
	ld bc,(MaxCol)
	cp c
	pop bc
	jr nz,+
	ld a,(MinCol)
+:	ld (CurCol),a
	ret nz
	; Fall-through to NewLine

NewLine:
	ld a,(CurRow)
	inc a
	push bc
	ld bc,(MaxRow)
	cp c
	pop bc
	jr nz,+
	call Scroll
	ld a,(MaxRow)
	dec a
+:	ld (CurRow),a
	ret

CursorLeft:
	ld a,(CurCol)
	push bc
	ld bc,(MinCol)
	cp c
	pop bc
	jr nz,+
	ld a,(MaxCol)
+:	push af
	dec a
	ld (CurCol),a
	pop af
	ret nz
+:	; Move up a row
	ld a,(CurRow)
	push bc
	ld bc,(MinRow)
	cp c
	pop bc
	jr nz,+
	ld a,(MaxRow)
+:	push af
	dec a
	ld (CurRow),a
	pop af
	ret

PutString:
	ld a,(hl)
	inc hl
	or a
	ret z
	push hl
	call PutChar
	pop hl
	jr PutString

	

.endmodule