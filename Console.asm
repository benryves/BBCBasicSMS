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

Reset:

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
	jr z,+
	jr c,+
	ld a,(MinCol)
	ld (CurCol),a
	jr NewLine
	
+:	ld (CurCol),a
	ret
	

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
	jr z,+
	jr c,+
	call Scroll
	ld a,(MaxRow)
+:	ld (CurRow),a
	ret

CursorLeft:
	ld a,(CurCol)
	dec a
	push bc
	ld bc,(MinCol)
	cp c
	pop bc
	jr nc,+
	ld a,(MaxCol)
+:	push af
	ld (CurCol),a
	pop af
	ret nc
+:

CursorUp:
	ld a,(CurRow)
	push bc
	ld bc,(MinRow)
	dec a
	cp c
	pop bc
	jr nc,+
	ld a,(MaxRow)
+:	push af
	ld (CurRow),a
	pop af
	ret

Clear:
	ret

Tab:
	ret

FlushPendingScroll:
	ret
	
.endmodule