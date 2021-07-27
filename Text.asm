.module Text

Functions:
	.db Function.Initialise \ .dw Initialise
	.db Function.PutMap \ .dw PutMap
	.db Function.Scroll \ .dw Scroll
	.db Function.SetConsoleColour \ .dw SetConsoleColour
	.db Function.ResetConsoleViewport \ .dw ResetConsoleViewport
	.db Function.End

NameTable = $3800

Initialise:
	
	; Switch video mode to TMS9918 TEXT I
	
	ld a,%00000000
	ld b,$00
	call Video.SetRegister
	
	ld a,%10010000 ; M1 = 1
	ld b,$01
	call Video.SetRegister
	
	; Name table is at $3800
	ld a,NameTable/$0400
	ld b,$02
	call Video.SetRegister
	
	; Patterns are at the start of VRAM so set pattern generator table address to 0
	xor a
	ld b,$04
	call Video.SetRegister
	
	; Set background/foreground colour
	ld a,$F1 ; TC, BG
	ld b,$07
	call Video.SetRegister
	
	; Load the font
	
	ld hl,0
	call Video.SetWriteAddress
	
	ld hl,VDU.Fonts.Font6x8
	ld bc,Video.Data ; B = 0, C = Video.Data
	
	otir
	otir
	otir
	
	ret

ResetConsoleViewport:
	call DefaultResetConsoleViewport
	ld a,39
	ld (Console.MaxCol),a
	ld a,40
	ld (Console.MaxWidth),a
	ret

PutMap:
	push hl
	push de
	push bc
	push af
	ld a,(Console.CurRow)
	ld l,a
	ld h,0
	
	; *40 = *32+*8
	add hl,hl ; *2
	add hl,hl ; *4
	add hl,hl ; *8
	ld e,l
	ld d,h
	add hl,hl ; *16
	add hl,hl ; *32
	add hl,de ; *40
	
	ld a,(Console.CurCol)
	ld e,a
	ld d,0
	add hl,de
.if NameTable != 0
	ld de,NameTable
	add hl,de
.endif
	call Video.SetWriteAddress
	pop af
	add a,FontCharOffset
	out (Video.Data),a
	pop bc
	pop de
	pop hl
	ret

Scroll:
	push bc
	push de
	push hl
	
	; Get the pointer to the top left corner.
	ld a,(Console.MinRow)
	ld l,a
	ld h,0
	
	; *40 = *32+*8
	add hl,hl ; *2
	add hl,hl ; *4
	add hl,hl ; *8
	ld e,l
	ld d,h
	add hl,hl ; *16
	add hl,hl ; *32
	add hl,de ; *40
	
	ld a,(Console.MinCol)
	ld e,a
	ld d,0
	add hl,de
	
	; Move one row down.
	ld de,NameTable + 40
	add hl,de
	
	
	; How many columns will we need to move?
	ld a,(Console.MinCol)
	ld c,a
	ld a,(Console.MaxCol)
	sub c
	inc a
	ld c,a
	
	; How many rows will we need to move?
	ld a,(Console.MinRow)
	ld b,a
	ld a,(Console.MaxRow)
	sub b
	ld b,a
	
	; We'll be moving row by row.
	ld de,40
	
	; If there are zero rows to scroll, skip ahead.
	jr z,ClearBottomRow
	
ScrollRows:
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
	
	; Move down to the next row.
	add hl,de
	djnz ScrollRows

ClearBottomRow:
	
	
	; Now that we've scrolled, clear that bottom row.
	or a
	sbc hl,de
	call Video.SetWriteAddress
	
	ld b,c
-:	ld a,' '*1+FontCharOffset ; 7
	out (Video.Data),a        ; 11
	nop                       ; 4
	djnz -                    ; 12 <- 34
	ei
	
	pop hl
	pop de
	pop bc
	ret

SetConsoleColour:
	call GraphicsII.SetConsoleColour
	ld a,(Console.Colour)
	ld b,$07
	call Video.SetRegister
	ei
	ret

.endmodule