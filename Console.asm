; =========================================================
; Module: Console
; =========================================================
; Handles outputting text to the screen.
; =========================================================
.module Console

CurRow = allocVar(1)
CurCol = allocVar(1)

MinRow = allocVar(1)
MaxRow = allocVar(1)
MinCol = allocVar(1)
MaxCol = allocVar(1)

Colour = allocVar(1)
SetColour = allocVar(3)

ResetColour:
	ld a,%11110100
	ld (Colour),a
	ret

Reset:
	ld a,$C3
	ld (SetColour),a
	ld hl,SetTextColourDefault
	ld (SetColour+1),hl
	
	call ResetColour

HomeUp:
	ld a,(MinRow)
	ld (CurRow),a
	; Fall-through to HomeLeft
	
HomeLeft:
	ld a,(MinCol)
	ld (CurCol),a
	ret

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
	ld a,(MinCol)
	ld (CurCol),a
	; Fall-through to CursorDown

CursorDown:
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
+:

CursorUp:
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

Clear:
	ret

Tab:
	ret

FlushPendingScroll:
	ret

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
	ld a,(Colour)
	and $0F
	or b
	ld (Colour),a
	jr DoneSetTextColour
SetTextBackgroundColour:
	and $0F
	ld b,a
	ld a,(Colour)
	and $F0
	or b
	ld (Colour),a
DoneSetTextColour:
	pop bc
	ret

.endmodule