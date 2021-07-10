.module VDU

; Mode files
.module Modes

.include "Text.asm"
.include "GraphicsII.asm"
.include "Mode4.asm"

Count = 3

.endmodule

; Font data
.module Fonts

Font8x8:
.include "bbc"

Font6x8:
.incbin "Font6x8.bin"

.endmodule

.var ubyte[3] PutMap
.var ubyte[3] Scroll
.var ubyte[3] SetTextColour

.var ubyte TextColour

Reset:
	xor a
SetMode:
	di
	push af
	
	ld a,$C3 ; JP
	ld (PutMap),a
	ld (Scroll),a
	ld (SetTextColour),a
	
	ld hl,Stub
	ld (PutMap+1),hl
	ld (Scroll+1),hl
	
	ld hl,SetTextColourDefault
	ld (SetTextColour+1),hl
	
	; Sensible text colour
	ld a,%11110100
	ld (TextColour),a
	
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

Stub:
	ret

SetModeInitialize:
	or a  \ jp z,Modes.Text.Initialise
	dec a \ jp z,Modes.GraphicsII.Initialise
	dec a \ jp z,Modes.Mode4.Initialise
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

	
SetTextColourDefault:
	push bc
	push af
	pop af
	bit 7,a
	jr nz,SetTextBackgroundColour
SetTextForegroundColour:
	ld b,4
-:	add a,a
	djnz -
	ld b,a
	ld a,(VDU.TextColour)
	and $0F
	or b
	ld (VDU.TextColour),a
	jr DoneSetTextColour
SetTextBackgroundColour:
	and $0F
	ld b,a
	ld a,(VDU.TextColour)
	and $F0
	or b
	ld (VDU.TextColour),a
DoneSetTextColour:
	pop bc
	ret

.endmodule