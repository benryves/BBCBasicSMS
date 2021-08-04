.module Mode4ReducedColour

PatternGenerator = $0000 ; 14KB, 448 tiles total.
NameTable        = $3800 ; 1792 bytes
SpriteTable      = $3F00 ; 256 bytes
TopOfMemory      = $4000 ; 16KB

Functions:
	.db Function.Initialise \ .dw Initialise
	.db Function.PutMap \ .dw PutMap
	.db Function.Scroll \ .dw Scroll
	.db Function.SetConsoleColour \ .dw SetConsoleColour
	.db Function.SetPixel \ .dw SetPixel
	.db Function.SelectPalette \ .dw SelectPalette
	.db Function.End

Initialise:
	
	; Fill the nametable.
	
	ld hl,NameTable
	call Video.SetWriteAddress
	
	ld hl,$0000
	call WriteNameTable
	
	ld hl,$0800
	call WriteNameTable
	
	; Initialise the two palettes.
	
	; Top half of screen:    0123012301230123
	; Bottom half of screen: 0000111122223333
	
	ld c,%00000000
	ld a,0
	call WritePaletteEntry
	
	ld c,%00000011
	ld a,1
	call WritePaletteEntry
	
	ld c,%00001111
	ld a,2
	call WritePaletteEntry
	
	ld c,%00111111
	ld a,3
	call WritePaletteEntry
	
	; Disable sprites
	ld hl,SpriteTable
	call Video.SetWriteAddress
	ld a,$D0
	out (Video.Data),a
	
	ret

WriteNameTable:
	ld bc,448 ; 448 = maximum number of tiles.
-:	ld a,l
	out (Video.Data),a
	ld a,h
	out (Video.Data),a
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,-
	ret
	

PutMap:
	push hl
	push de
	push bc
	push af
	
	; What is the pattern underneath (CurCol, CurRow)?
	ld a,(VDU.Console.CurCol)
	ld e,a
	ld a,(VDU.Console.CurRow)
	
	call GetNameTableValueForTile
	
	; Now we need to find that tile inside the pattern generator.
	; Address = (nametable entry & 511) * 32
	
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	
	; Carry flag = colour palette.
	; If we're using the second colour palette, shift the pointer by two bytes.
	jr nc,+
	inc hl
	inc hl
+:

	; Offset by the pattern generator address.
.if PatternGenerator != 0
	ld de,PatternGenerator
	add hl,de
.endif

	; Restore the character number.
	pop af
	
	; Store pattern generator pointer for later.
	push hl
	
	; Load the font pointer.
	add a,FontCharOffset
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,Fonts.Font8x8
	add hl,de
	ex de,hl
	
	; Restore pattern generator pointer.
	pop hl
	
	ld b,8
--:	push bc
	call Video.SetWriteAddress
	push hl
	
	
	; Write the two bitplanes
	ld a,(Console.Colour)
	ld l,a

	ld b,2	
-:	rrc l
	sbc a,a
	ld c,a  ; Foreground bitmask.
	rrc l
	rrc l
	sbc a,a
	ld h,a ; Background bitmask.
	rlc l
	rlc l
	
	ld a,(de) ; Character value.
	cpl
	and h     ; Background mask.
	ld h,a
	ld a,(de)
	and c     ; Foreground mask.
	or h      ; Result
	
	out (Video.Data),a
	djnz -
	
	pop hl
	ld bc,4
	add hl,bc
	
	pop bc
	inc de ; Next value from character mask
	djnz --
	
	
	pop bc
	pop de
	pop hl
	ei
	ret

; ---------------------------------------------------------
; GetNameTableValueForTile -> gets the name table entry
;                             at a certain tile position.
; ---------------------------------------------------------
; Inputs:   a = tile row.
;           e = tile column.
; Outputs:  hl = nametable entry at (e,a).
; Destroys: af, hl, b.
; ---------------------------------------------------------
GetNameTableValueForTile:
	add a,a
	add a,a
	add a,a
	ld l,a
	ld h,0
	ld d,h
	add hl,hl
	add hl,hl
	; HL = row * 32
	add hl,de
	; HL = row * 32 + col
	add hl,hl
	; HL = (row * 32 + col) * 2
	ld de,NameTable
	add hl,de
	
	; Read the nametable entry under the cursor to get the tile number.
	call Video.SetReadAddress
	nop
	in a,(Video.Data) ; LSB
	ld l,a
	inc hl
	nop
	dec hl
	in a,(Video.Data) ; MSB
	ld h,a
	ret
	
Scroll:
	
	push bc
	push de
	push hl
	
	; Get the pointer to the top left corner.
	ld a,(Console.MinRow)
	ld l,a
	ld h,0

	; *64
	ld b,6
-:	add hl,hl
	djnz -
	
	ld a,(Console.MinCol)
	add a,a
	
	ld e,a
	ld d,0
	add hl,de
	
.if NameTable != 0
	ld de,NameTable
	add hl,de
.endif

	; How many columns will we need to move?
	ld a,(Console.MinCol)
	ld c,a
	ld a,(Console.MaxCol)
	sub c
	inc a
	add a,a
	ld c,a
	
	; How many rows will we need to move?
	ld a,(Console.MinRow)
	ld b,a
	ld a,(Console.MaxRow)
	sub b
	inc a
	ld b,a
	
	; We'll store the top row in BASIC's FREE memory.
	ld de,(Basic.BBCBASIC_FREE)
	push de
	push bc
	push bc
	push hl
	
	call Video.SetReadAddress

	ld b,c	
-:	in a,(Video.Data) ; 11
	ld (de),a         ; 12
	inc de            ; 6
	djnz -            ; 12 <- 41.
	
	
	pop hl
	pop bc
	ld (Basic.BBCBASIC_FREE),de
	
	; We'll be moving row by row.
	ld de,64
	
	call Console.ScrollBlockNoClear
	
	; Restore column/row count and FREE
	pop bc
	pop de
	ld (Basic.BBCBASIC_FREE),de
	
	; HL -> bottom row.
	call Video.SetWriteAddress

	ld b,c
-:	ld a,(de)
	out (Video.Data),a
	inc de
	djnz -
	
	; Now, the slow bit: clear the bottom row.
	ld a,(Console.MinRow)
	push af
	ld a,(Console.MaxRow)
	ld (Console.MinRow),a
	call VDU.DefaultClear
	pop af
	ld (Console.MinRow),a
	
	pop hl
	pop de
	pop bc
	ei
	ret
	
; ---------------------------------------------------------
; SelectPalette -> Selects the palette.
; ---------------------------------------------------------
; Inputs:   a = "physical" colour (from BBC BASIC palette).
;           b = "physical" colour.
;           c = logical colour.
;           hl = pointer to RGB colour (if applicable).
; Destroys: af, hl, bc.
; ---------------------------------------------------------
SelectPalette:
	ld b,c
	push bc
	call Mode4.ParsePaletteCommand
	ld c,a
	pop af
	; Fall-through...

; ---------------------------------------------------------
; WritePaletteEntry -> Updates a particular palette entry.
; ---------------------------------------------------------
; Inputs:   a = logical palette index (0..3).
;           c = colour to set (6-bit BGR).
; Destroys: af, hl, b.
; ---------------------------------------------------------
WritePaletteEntry:
	
	and %11
	ld h,a
	
	ld b,4
-:	ld a,h
	call Video.GotoPalette
	ld a,c
	out (Video.Data),a
	inc h
	inc h
	inc h
	inc h
	djnz -
	
	ld a,h
	rlca
	rlca
	add a,16
	
	call Video.GotoPalette
	
	ld a,c
	ld b,4
-:	out (Video.Data),a
	djnz -
	
	ei
	ret

; ---------------------------------------------------------
; SetConsoleColour -> Updates the current text colour.
; ---------------------------------------------------------
; Inputs:   a = the new colour.
; Destroys: af, c.
; ---------------------------------------------------------
SetConsoleColour:
	bit 7,a
	jr nz,SetConsoleColour.Background

SetConsoleColour.Foreground:
	and %11
	ld c,a
	ld a,(Console.Colour)
	and %1100
	or c
	ld (Console.Colour),a
	ret
	
SetConsoleColour.Background:
	and %11
	add a,a
	add a,a
	ld c,a
	ld a,(Console.Colour)
	and %11
	or c
	ld (Console.Colour),a
	ret

SelectPalette:

.endmodule