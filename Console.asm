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

.endmodule