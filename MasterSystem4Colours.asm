.module MasterSystem4Colours

PatternGenerator = $0000 ; 14KB, 448 tiles total.
NameTable        = $3800 ; 1536 bytes
SpriteTable      = $3F00 ; 256 bytes
TopOfMemory      = $4000 ; 16KB

MinGraphicsTile  = 0
MaxGraphicsTile  = 384   ; +1

UserDefinedChars = PatternGenerator + MaxGraphicsTile * 32
EndOfUserDefinedChars = NameTable
UserDefinedCharCount = (EndOfUserDefinedChars - UserDefinedChars) / 8

Functions:
	.db Function.Initialise \ .dw Initialise
	.db Function.PutMap \ .dw PutMap
	.db Function.Scroll \ .dw Scroll
	.db Function.BeginPlot \ .dw MasterSystem16Colours.BeginPlot
	.db Function.SetAlignedHorizontalLineSegment \ .dw SetAlignedHorizontalLineSegment
	.db Function.SelectPalette \ .dw SelectPalette
	.db Function.SelectDefaultPalette \ .dw SelectDefaultPalette
	.db Function.PreserveUnderCursor \ .dw PreserveUnderCursor
	.db Function.RestoreUnderCursor \ .dw RestoreUnderCursor
	.db Function.SetUserDefinedCharacter \ .dw SetUserDefinedCharacter
	.db Function.GetUserDefinedCharacter \ .dw GetUserDefinedCharacter
	.db Function.End

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
	
	; Callback jumps.
	ld a,$C3
	ld (ManipulatePixelColour),a
	ld (ManipulatePixelBitmask),a
	
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
	
	call GetUserDefinedCharacter
	ex de,hl
	
	jr WriteFontData

ROMFont:
	
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
	
Scroll:
	
	push bc
	push de
	push hl
	
	; Get the pointer to the top left corner.
	ld a,(Console.MinRow)
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
	call MasterSystem16Colours.ParsePaletteCommand
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
	call Video.SetPalette
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
	ld a,c
	ld c,(hl)
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
	add a,a
	add a,a
	ld e,a
	ld d,0
	add hl,de
	
	; Retrieve masks
	pop de
	; D = bits to OR to set (DRAW)
	; E = bits to AND to clear (ERASE)
	
	; HL -> pattern generator address for where we need to draw.
	push hl
	call Video.SetReadAddress
	
	; At this point, we'll  use TempTile to store the generated tile.
	ld hl,TempTile
	
	ld a,(Graphics.PlotColour)
	ld c,a
	ld b,2
	call ManipulatePixelBitmask
	
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
	add a,a
	ret nc ; As we're expecting to map from $80..$FF, must carry
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	ld de,UserDefinedChars
	add hl,de
	scf
	ret

SetUserDefinedCharacter:
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
	call GetUserDefinedCharacterAddress
	ret nc
		
	call Video.SetReadAddress
	ld hl,(Basic.BBCBASIC_FREE)
	push hl
	ld b,8
	
-:	in a,(Video.Data)   ; 11
	ld (hl),a           ; 7
	inc hl              ; 6
	djnz -              ; 13 = 37 clocks
	
	pop hl
	scf
	ei
	ret

.endmodule