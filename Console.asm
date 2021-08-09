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

MaxWidth = allocVar(1)
MaxHeight = allocVar(1)

Colour = allocVar(1)

Status = allocVar(1)
PendingScroll = 0
PageMode = 1
CursorMoved = 2
CursorBlinking = 3

CursorBlinkTimer = allocVar(1)

AreaUnderCursor = allocVar(16)

Reset:
	
	xor a
	ld (Status),a
	ld a,$0F
	ld (Colour),a

ResetViewport:
	
	call ClearPendingScroll
	call ResetConsoleViewport ; Driver-specific function.

HomeUp:
	ld a,(MinRow)
	ld (CurRow),a
	; Fall-through to HomeLeft
	
HomeLeft:
	call ClearPendingScroll
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
	jr MarkCursorMoved
	

NewLine:
	ld a,(MinCol)
	ld (CurCol),a
	; Fall-through to CursorDown

CursorDown:

	call FlushPendingScroll

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
	
	jr MarkCursorMoved

CursorLeft:
	ld a,(CurCol)
	or a
	jr z,CursorLeftWrapped
	dec a
	push bc
	ld bc,(MinCol)
	cp c
	pop bc
	jr nc,+
CursorLeftWrapped:
	scf
	ld a,(MaxCol)
+:	push af
	ld (CurCol),a
	pop af
	jr nc,MarkCursorMoved
+:

CursorUp:
	ld a,(CurRow)
	push bc
	ld bc,(MinRow)
	or a
	jr z,CursorUpWrapped
	dec a
	cp c
	pop bc
	jr nc,+
CursorUpWrapped:
	ld a,(MaxRow)
+:	push af
	ld (CurRow),a
	pop af
	jr MarkCursorMoved

Clear:
	ret

Tab:
	ret

MarkCursorMoved:
	ld a,(Status)
	set CursorMoved,a
	ld (Status),a
	ret

FlushPendingScroll:
	push af
	ld a,(Status)
	bit PendingScroll,a
	res PendingScroll,a
	ld (Status),a
	call nz,Scroll
	pop af
	ret
	
ClearPendingScroll:
	push af
	ld a,(Status)
	res PendingScroll,a
	ld (Status),a
	pop af
	ret

SetPendingScroll:
	push af
	ld a,(Status)
	set PendingScroll,a
	ld (Status),a
	pop af
	ret

; ---------------------------------------------------------
; ScrollBlockNoClear -> Scrolls a block of characters but
;                       doesn't clear the bottom row.
; ---------------------------------------------------------
; Inputs:   hl = offset in VRAM to top left corner.
;           de = stride of VRAM in bytes.
;           b = number of rows to scroll.
;           c = number of columns to scroll.
; Outputs:  None.
; Destroys: af, hl, de, bc.
; ---------------------------------------------------------
ScrollBlockNoClear:
	
	xor a
	dec a
	jr ScrollBlockSetNZ

; ---------------------------------------------------------
; ScrollBlock -> Scrolls a block of characters and clears
;                the bottom row.
; ---------------------------------------------------------
; Inputs:   hl = offset in VRAM to top left corner.
;           de = stride of VRAM in bytes.
;           b = number of rows to scroll.
;           c = number of columns to scroll.
; Outputs:  None.
; Destroys: af, hl, de, bc.
; ---------------------------------------------------------
ScrollBlock:
	
	xor a

ScrollBlockSetNZ:
	push af
	
	dec b
	jr z,ClearBottomRow

ScrollBlockRow:
	add hl,de
	
	push hl
	push bc
	
	; Copy one row.
	call Video.SetReadAddress
	ld hl,(Basic.BBCBASIC_FREE)
	
	ld b,c
-:	in a,(Video.Data) ; 11
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12 <- 36
	
	pop bc
	pop hl
	
	ei
	
	; Write back to the row above.
	push hl
	push bc
	
	or a
	sbc hl,de
	
	call Video.SetWriteAddress
	ld hl,(Basic.BBCBASIC_FREE)
	
	ld b,c
-:	ld a,(hl)          ; 7
	out (Video.Data),a ; 11
	inc hl             ; 6
	djnz -             ; 12 <- 36
	
	pop bc
	pop hl
	
	ei
	djnz ScrollBlockRow

ClearBottomRow:
	
	; Now that we've scrolled, clear that bottom row.
	call Video.SetWriteAddress
	
	pop af
	jr nz,+
	
	ld b,c
-:	ld a,0              ; 7
	out (Video.Data),a  ; 11
	nop                 ; 4
	djnz -              ; 12 <- 34

+:	ei
	ret

; ---------------------------------------------------------
; BeginBlinkingCursor -> Prepare to draw blinking cursor.
; ---------------------------------------------------------
; Destroys: af.
; ---------------------------------------------------------
BeginBlinkingCursor:
	; Is the cursor already blinking?
	ld a,(Status)
	bit CursorBlinking,a
	ret nz
	set CursorBlinking,a
	ld (Status),a
	
	; We haven't already started.
	push hl
	push de
	push bc
	call VDU.PreserveUnderCursor
	pop bc
	pop de
	pop hl
	
	ld a,(Host.TIME)
	ld (CursorBlinkTimer),a
	ret

; ---------------------------------------------------------
; EndBlinkingCursor -> Ends drawing the blinking cursor.
; ---------------------------------------------------------
; Destroys: af.
; ---------------------------------------------------------
EndBlinkingCursor:
	; Is the cursor currently blinking?
	ld a,(Status)
	bit CursorBlinking,a
	ret z
	res CursorBlinking,a
	ld (Status),a

	; We had been drawing a blinking cursor.
	push hl
	push de
	push bc
	call VDU.RestoreUnderCursor
	pop bc
	pop de
	pop hl
	ret

; ---------------------------------------------------------
; DrawBlinkingCursor -> Draws the blinking cursor.
; ---------------------------------------------------------
; Destroys: af.
; ---------------------------------------------------------
DrawBlinkingCursor:
	; Is the cursor set to blink?
	ld a,(Status)
	bit CursorBlinking,a
	call z,BeginBlinkingCursor
	
	; Are we showing the cursor or the character underneath?
	push bc
	ld a,(CursorBlinkTimer)
	ld b,a
	ld a,(Host.TIME)
	sub b
	and %00100000
	pop bc
	
	jr z,DrawBlinkingCursorOn

DrawBlinkingCursorOff:
	push hl
	push de
	push bc
	call VDU.RestoreUnderCursor
	pop bc
	pop de
	pop hl
	ret

DrawBlinkingCursorOn:	
	; What is the cursor?
	ld a,(Host.Flags)
	bit Host.Overwrite,a
	ld a,'_' ; Insert mode cursor
	jr z,+
	ld a,127 ; Overwrite mode cursor 
+:
	jp PutMap

.endmodule