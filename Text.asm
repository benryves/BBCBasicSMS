.module Text

Vectors:
	jp Execute
	jp PutMap
	ret \ nop \ nop ; BeginPlot - No graphics in text mode.
	ret \ nop \ nop ; SetAlignedHorizontalLineSegment - No graphics in text mode.

NameTable = $3800

Execute:
	cp Driver.Execute.Reset
	jr z,Initialise
	cp Driver.Execute.ScrollUp
	jp z,Scroll
	cp Driver.Execute.GetCursorArea
	jp z,PreserveUnderCursor
	cp Driver.Execute.SetCursorArea
	jp z,RestoreUnderCursor
	ret

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
	push hl
	ld bc,Video.Data ; B = 0, C = Video.Data
	
	otir
	otir
	otir
	
	; And now, the inverted version!
	
	pop hl
	ld c,3
	
-:	ld a,(hl)
	xor %11111100
	out (Video.Data),a
	inc hl
	djnz -
	dec c
	jr nz,-
	
	
	ret

AMul40:
	ld l,a
	ld h,0
HLMul40:
	; *40 = *32+*8
	add hl,hl ; *2
	add hl,hl ; *4
	add hl,hl ; *8
	ld e,l
	ld d,h
	add hl,hl ; *16
	add hl,hl ; *32
	add hl,de ; *40
	ret

GetNameTableAddressForCursor:
	ld a,(Console.CurRow)
	call AMul40
	ld a,(Console.CurCol)
	ld e,a
	ld d,0
	add hl,de
.if NameTable != 0
	ld de,NameTable
	add hl,de
.endif
	ret

PutMap:
	and $7F
	push hl
	push de
	push bc
	push af
	
	call GetNameTableAddressForCursor
	call Video.SetWriteAddress
	pop af
	add a,FontCharOffset
	cp 96
	jr c,+
	xor a
+:	ld b,a

	ld a,(VDU.Console.Colour)
	ld c,a
	rlca
	rlca
	rlca
	rlca
	xor c
	srl a
	ld a,0
	
	; Same foreground and background colour = draw as block.
	jr c,+
	ld b,127
+:	

	; Is the foreground colour black?
	srl c
	jr c,+
	ld a,96
+:	
	add a,b
	out (Video.Data),a
	pop bc
	pop de
	pop hl
	ei
	ret

Scroll:
	push bc
	push de
	push hl
	
	; Get the pointer to the top left corner.
	ld a,(Console.MinRow)
	call AMul40
	ld a,(Console.MinCol)
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
	ld c,a
	
	; How many rows will we need to move?
	ld a,(Console.MinRow)
	ld b,a
	ld a,(Console.MaxRow)
	sub b
	inc a
	ld b,a
	
	; We'll be moving row by row.
	ld de,40
	
	call Console.ScrollBlock
	
	pop hl
	pop de
	pop bc
	ei
	ret

PreserveUnderCursor:
	push hl
	call GetNameTableAddressForCursor
	call Video.SetReadAddress
	in a,(Video.Data)
	pop hl
	ld (hl),a
	ei
	ret

RestoreUnderCursor:
	push hl
	call GetNameTableAddressForCursor
	call Video.SetWriteAddress
	pop hl
	ld a,(hl)
	out (Video.Data),a
	ei
	ret

; ---------------------------------------------------------
; SelectDefaultPalette -> Selects the default palette.
; ---------------------------------------------------------
; Destroys: af, hl, bc.
; ---------------------------------------------------------
SelectDefaultPalette:
	xor a
	ld b,a
	ld c,a
	call SelectPalette
	ld a,15
	ld b,a
	ld c,a
	; Fall-through

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
	ret ; TODO
	; Pretend we're setting the palette normally in our 2-colour palette.
	ld a,c
	and 1
	ld c,a
	ld a,b
;	call MasterSystem16Colours.SelectPalette
	
	; Convert the console colour to a TMS9918 pair.
	ld a,(VDU.Console.Colour)
	and $11
	call VDU.Palettes.ConvertColourPairToTMS9918
	
	; Set the text colour.
	ld b,$07
	jp Video.SetRegister

.endmodule