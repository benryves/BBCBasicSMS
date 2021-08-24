.module MasterSystem4Colours

Vectors:
	jp Execute
	jp PutMap
	jp MasterSystem16Colours.BeginPlot
	jp SetAlignedHorizontalLineSegment

Execute:
	or a \ jr z,Initialise
	dec a \ ret z
	dec a \ ret z
	dec a \ jp z,ScrollDown
	dec a \ jp z,ScrollUp
	dec a \ jp z,GetUserDefinedCharacter
	dec a \ jp z,SetUserDefinedCharacter
	dec a \ jp z,PreserveUnderCursor
	dec a \ jp z,RestoreUnderCursor
	dec a \ jp z,SelectPalette
	dec a \ jp z,SelectDefaultPalette
	jp MasterSystem16Colours.Execute

PatternGenerator = $0000 ; 14KB, 448 tiles total.
NameTable        = $3800 ; 1536 bytes
FillPatterns     = $3E00 ; 32 bytes
SpriteTable      = $3F00 ; 256 bytes
TopOfMemory      = $4000 ; 16KB

MinGraphicsTile  = 0
MaxGraphicsTile  = 384   ; +1

UserDefinedChars = PatternGenerator + MaxGraphicsTile * 32
EndOfUserDefinedChars = NameTable
UserDefinedCharCount = (EndOfUserDefinedChars - UserDefinedChars) / 8

Initialise:
	
	; Fill the nametable.
	
	ld hl,NameTable
	call Video.SetWriteAddress
	
	ld hl,MinGraphicsTile
	call WriteNameTable
	
	ld hl,MinGraphicsTile | $0800
	call WriteNameTable
	
	; Disable sprites
	ld hl,SpriteTable
	call Video.SetWriteAddress
	ld a,$D0
	out (Video.Data),a

	; Load the pattern fill data.
	call SetDefaultFillPatterns
	
	; Callback jumps.
	ld a,$C3
	ld (Driver.ManipulatePixelColour),a
	ld (Driver.ManipulatePixelBitmask),a
	
	ret

WriteNameTable:
	ld bc,MaxGraphicsTile - MinGraphicsTile
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

SetDefaultFillPatterns:
	ld hl,DefaultPatterns
	ld de,FillPatterns
	jp MasterSystem16Colours.LoadPackedFillPatterns

DefaultPatterns:
.db %10100101 ; 2 1 2 1 - GCOL 16
.db %00001111 ; 1 1 1 1 
.db %10100101 ; 2 1 2 1 - GCOL 32
.db %01011010 ; 1 2 1 2
.db %11110000 ; 2 2 2 2 - GCOL 48
.db %01011010 ; 1 2 1 2
.db %11110101 ; 2 3 2 3 - GCOL 64
.db %11111010 ; 3 2 3 2

; ---------------------------------------------------------
; PutMap -> Draws a character on the display.
; ---------------------------------------------------------
; Inputs:   a = character to display.
;           (Console.CurCol) = cursor column.
;           (Console.CurRow) = cursor row.
; Destroys: None.
; ---------------------------------------------------------
PutMap:
	push hl
	push de
	push bc
	push af
	
	; What is the pattern underneath (CurCol, CurRow)?
	ld a,(Console.CurCol)
	ld e,a
	ld a,(Console.CurRow)
	
	call GetPatternGeneratorAddressForTile

	; Restore the character number.
	pop af
	
	; Store pattern generator pointer for later.
	push hl
	
	; Is it a user-defined character?
	cp $80
	jr c,ROMFont
	
	ld hl,8
	call Host.GetSafeScratchMemoryHL
	jr c,+
	call GetUserDefinedCharacter
	ex de,hl
	
	jr WriteFontData
+:
	ld a,' '

ROMFont:
	
	; Load the font pointer.
	add a,FontCharOffset
	jr c,+
	xor a
+:	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,Fonts.Font8x8
	add hl,de
	ex de,hl
	
WriteFontData:
	
	; Restore pattern generator pointer.
	pop hl
	call Video.SetWriteAddress
	
	ld b,8
--:	push bc
	
	; Write the two bitplanes
	ld a,(Console.Colour)
	ld l,a

	ld b,2	
-:	rrc l ; 0
	sbc a,a
	ld c,a  ; Foreground bitmask.
	rrc l ; 1
	rrc l ; 2
	rrc l ; 3
	
	rrc l ; 4
	sbc a,a
	ld h,a ; Background bitmask.
	
	rlc l ; 3
	rlc l ; 2
	rlc l ; 1
	rlc l ; 0
	
	ld a,(de) ; Character value.
	cpl
	and h     ; Background mask.
	ld h,a
	ld a,(de)
	and c     ; Foreground mask.
	or h      ; Result
	
	out (Video.Data),a
	djnz -            ; 8
	
	inc de            ; 10 Next value from character mask
	pop bc            ; 10
	in a,(Video.Data) ; 11 <- 39
	
	push hl           ; 11
	pop hl            ; 10
	in a,(Video.Data) ; 11
	djnz --           ; 13
	
	
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

; ---------------------------------------------------------
; GetPatternGeneratorAddressForCursor -> gets the address
; of the pattern generator data for the cursor.
; ---------------------------------------------------------
; Inputs:   CurCol, CorRow.
; Outputs:  hl = pattern generator address.
; Destroys: af, de, b.
; ---------------------------------------------------------	
GetPatternGeneratorAddressForCursor:
	ld a,(VDU.Console.CurCol)
	ld e,a
	ld a,(VDU.Console.CurRow)

; ---------------------------------------------------------
; GetPatternGeneratorAddressForTile -> gets the address of
; the pattern generator data for a tile.
; ---------------------------------------------------------
; Inputs:   a = tile row.
;           e = tile column.
; Outputs:  hl = pattern generator address.
; Destroys: af, de, b.
; ---------------------------------------------------------
GetPatternGeneratorAddressForTile:
	call GetNameTableValueForTile
	
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
	ret

ScrollDown:
	push bc
	push de
	push hl
	
	; We'll be clearing the top row.
	ld bc,(Console.CurRow)
	push bc
	ld a,(Console.MinRow)
	ld (Console.CurRow),a
	
	; Get the pointer to the bottom left corner.
	ld a,(Console.MaxRow)
	
	; 64 bytes per row.
	ld de,-64
	jr ScrollFromRow
	
ScrollUp:
	
	push bc
	push de
	push hl
	
	; We'll be clearing the bottom row.
	ld bc,(Console.CurRow)
	push bc
	ld a,(Console.MaxRow)
	ld (Console.CurRow),a

	; Get the pointer to the top left corner.
	ld a,(Console.MinRow)
	
	; 64 bytes per row.
	ld de,64

ScrollFromRow:
	
	ld (TempSize),de
	
	
	call MasterSystem16Colours.AMul64
	
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
	
	; We'll store the top row on the stack.
	di
	ex de,hl
	ld hl,-32 ; Leave enough room for a regular stack.
	add hl,sp
	ex de,hl
	
	push de
	
	push bc
	push bc
	push hl
	
	call Video.SetReadAddress

	ld b,c	
-:	in a,(Video.Data) ; 11
	ld (de),a         ; 12
	dec de            ; 6
	djnz -            ; 12 <- 41.
	
	
	pop hl
	pop bc
	
	; We'll be moving row by row.
	ld de,(TempSize)
	
	call Console.ScrollBlockNoClear
	
	; Restore column/row count and place we stored the top row.
	pop bc
	pop de
	
	; HL -> bottom row.
	call Video.SetWriteAddress

	ld b,c
-:	ld a,(de)
	out (Video.Data),a
	dec de
	djnz -
	
	; Now, the slow bit: clear the bottom row.
	ld bc,(Console.MinRow)
	push bc
	
	ld a,(Console.CurRow)
	ld (Console.MinRow),a
	ld (Console.MaxRow),a
	call Console.Clear
	
	pop bc
	ld (Console.MinRow),bc
	
	pop bc
	ld (Console.CurRow),bc
	
	pop hl
	pop de
	pop bc
	ret
	
; ---------------------------------------------------------
; SelectPalette -> Selects the palette.
; ---------------------------------------------------------
; Inputs:   b = "physical" colour (from BBC BASIC palette).
;           c = logical colour.
;           hl = pointer to RGB colour (if applicable).
; Destroys: af, hl, bc.
; ---------------------------------------------------------
SelectPalette:
	push bc
	call MasterSystem16Colours.ParsePaletteCommand
	pop bc
	; Fall-through...

; ---------------------------------------------------------
; WritePaletteEntry -> Updates a particular palette entry.
; ---------------------------------------------------------
; Inputs:   c = logical palette index (0..3).
;           a = colour to set (6-bit BGR).
; Destroys: af, hl, b.
; ---------------------------------------------------------
WritePaletteEntry:
	
	ld l,a
	
	ld a,c
	and %11
	ld h,a
	
	ld c,l
	
	ld b,4
-:	ld a,h
	call Video.SetPalette ; palette[a] = c
	inc h
	inc h
	inc h
	inc h
	djnz -
	
	ld a,h
	rlca
	rlca
	add a,16
	
	; We can set the rest of the palette directly,
	; as it's colours 16..31 which won't end up in the RAM copy.
	call Video.GotoPalette
	
	ld a,c
	ld b,4
-:	out (Video.Data),a ; 11
	inc hl             ; 6
	dec hl             ; 6
	djnz -             ; 13 <- 36
	
	ei
	ret
	

; ---------------------------------------------------------
; SelectDefaultPalette -> Selects the default palette.
; ---------------------------------------------------------
; Destroys: af, hl, bc.
; ---------------------------------------------------------
SelectDefaultPalette:
	ld hl,DefaultPalette
	ld bc,4<<8
-:	push bc
	push hl
	ld b,c
	ld a,(hl)
	call WritePaletteEntry
	pop hl
	pop bc
	inc c
	inc hl
	djnz -
	ret

DefaultPalette:
.db %00000000
.db %00000011
.db %00001111
.db %00111111

; ---------------------------------------------------------
; SetAlignedHorizontalLineSegment -> Sets a tile-aligned
;                                horizontal line segment.
; ---------------------------------------------------------
; Inputs:   d = X coordinate.
;           e = Y cooridnate.
;           h = set pixel mask (OR).
;           l = clear pixel mask (AND).
; Destroys: af, bc, de, hl.
; ---------------------------------------------------------
SetAlignedHorizontalLineSegment:
	
	push hl
	
	push de
	
	srl d
	srl d
	srl d
	
	srl e
	srl e
	srl e
	
	ld a,e
	ld e,d
	
	; (E,A)
	call GetPatternGeneratorAddressForTile
	
	pop de
	
	; DE = pixel (X,Y)
	; We need to advance 4 bytes for Y & 7
	
	ld a,e
	and %111
	ld e,a
	add a,a
	add a,a
	ld c,a
	ld b,0
	add hl,bc
	
	; HL -> pattern generator address for where we need to draw.
	
	; Are we using a solid colour or a pattern fill?
	ld a,(Graphics.PlotMode)
	cp 16
	jr c,SolidColour
	
PatternFill:

	; Fetch the pattern row data from VRAM.
	sub 16
	and %00110000
	rrca
	ld c,a
	ld a,e ; Y coordinate
	and 7
	or c
	ld c,a
	ld b,0
	
	; Retrieve masks from stack.
	pop de
	; Save pattern generator address.
	push hl
	
	ld hl,FillPatterns
	add hl,bc
	call Video.SetReadAddress
	in a,(Video.Data)
	ld c,a
	
	pop hl
	push hl

	call Video.SetReadAddress
	ld hl,TempTile

	; Unpack bitmask to bit planes: AB -> BB (first bit plane), AA (second bit plane).
	ld b,2	
	push bc
	ld a,c
--:	and %00001111
	
	ld b,a
	add a,a
	add a,a
	add a,a
	add a,a
	or b
	ld c,a
	
	push de
	
	; Modify the original Draw and Erase bitmasks with our pattern bitmask for the current bitplane.
	and d
	ld d,a
	
	ld a,c
	or e
	ld e,a
	
	; The filler only does a single bitplane at a time.
	in a,(Video.Data)
	call Driver.ManipulatePixelBitmask
	ld (hl),a
	inc hl
	
	pop de
	pop bc
	
	dec b
	jr z,GeneratedTileRow
	
	ld a,c
	rlca
	rlca
	rlca
	rlca
	ld c,a
	push bc
	jr --

SolidColour:
	; Retrieve masks from stack.
	pop de
	
	; Store pattern generator address.
	push hl
	call Video.SetReadAddress
	
	; At this point, we'll  use TempTile to store the generated tile.
	ld hl,TempTile
	
	ld a,(Graphics.PlotColour)
	ld c,a
	ld b,2
	call Driver.ManipulatePixelBitmask

GeneratedTileRow:
	
	
	; Get ready to write the new data to VRAM!
	pop hl
	call Video.SetWriteAddress
	
	
	ld hl,TempTile
	ld b,2
-:	ld a,(hl)           ; 7
	out (Video.Data),a  ; 11
	inc hl              ; 6
	djnz -              ; 12 <- 36
	
	ei
	ret

PreserveUnderCursor:
	call GetPatternGeneratorAddressForCursor
	call Video.SetReadAddress	
	
	ld hl,VDU.Console.AreaUnderCursor
	ld de,0 ; Can't write to ROM!
	ld b,16
	
-:	in a,(Video.Data) ; 11
	ld (hl),a         ; 7
	inc hl            ; 6
	inc de            ; 6
	dec de            ; 6
	in a,(Video.Data) ; 11 <- 36
	ld (hl),a         ; 7
	inc hl            ; 6
	ex de,hl          ; 4
	djnz -            ; 13/8
	
	ei
	ret

RestoreUnderCursor:
	call GetPatternGeneratorAddressForCursor
	call Video.SetWriteAddress
	
	ld hl,VDU.Console.AreaUnderCursor
	ld b,8
	
-:	ld a,(hl)          ; 7
	out (Video.Data),a ; 11
	inc hl             ; 6
	ld de,0            ; 10
	ld a,(hl)          ; 7
	out (Video.Data),a ; 11 <- 34
	inc hl             ; 6
	inc hl             ; 6
	dec hl             ; 6
	nop                ; 4
	in a,(Video.Data)  ; 11 <- 33
	push hl            ; 11
	pop hl             ; 11
	in a,(Video.Data)  ; 11 <- 33
	nop                ; 4
	djnz -             ; 13/8
	
	ei
	ret

; ---------------------------------------------------------
; GetUserDefinedCharacterAddress -> Gets address in VRAM
; ---------------------------------------------------------
; Inputs:   a = character to get the address of.
; Outputs:  nc if the character is out of range.
;           hl = address of the character otherwise.
; Destroys: af, de.
; ---------------------------------------------------------
GetUserDefinedCharacterAddress:
	ld de,UserDefinedChars
	add a,a
	jr c,+ ; As we're expecting to map from $80..$FF, this should carry
	
	; Alternatively, this may be one of those fill patterns...
	cp 6
	ret nc
	srl a
	ret z
	dec a
	ret z
	dec a
	add a,a
	
	ld de,FillPatterns
	
+:	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,de
	scf
	ret

SetUserDefinedCharacter:
	ld a,c
	
	cp 11
	jp z,SetDefaultFillPatterns
	
	or a
	jp m,SetPattern

	cp 6
	jr c,SetPattern

	cp 12
	ret c
	
	cp 16
	ret nc
	
	; Fill a simpler pattern.
	
	push af
	push hl
	
	ld e,l
	ld d,h

	; Pack the two pixel values into a single value.
	ld c,4
--:	push bc	
	
	ld b,2
	xor a
-:	srl (hl)
	inc hl
	rr a
	srl (hl)
	dec hl
	rr a
	ld c,a
	srl a
	srl a
	djnz -
	
	or c
	
	; Store, then move on.
	
	ld (de),a
	inc de
	inc hl
	inc hl
	
	pop bc
	dec c
	jr nz,--
	
	pop hl
	
	; Copy the top half into the bottom half.
	push hl
	ld bc,4
	ldir
	pop hl
	
	; Subtract 10 then load as a normal (full) tile.
	pop af
	sub 10

SetPattern:

	push hl
	call GetUserDefinedCharacterAddress
	pop de
	
	ret nc
	
	call Video.SetWriteAddress
	
	ld b,8
-:	ld a,(de)           ; 7
	out (Video.Data),a  ; 11
	inc de              ; 6
	djnz -              ; 13 = 37 clocks
	
	ei
	ret


GetUserDefinedCharacter:
	push hl
	ld a,c
	
	call GetUserDefinedCharacterAddress
	
	jr c,+
	pop hl
	ret
		
+:	call Video.SetReadAddress
	ld b,8
	
	pop hl
	push hl
	
-:	in a,(Video.Data)   ; 11
	ld (hl),a           ; 7
	inc hl              ; 6
	djnz -              ; 13 = 37 clocks
	
	pop hl
	scf
	ei
	ret

.endmodule