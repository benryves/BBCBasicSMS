.module MasterSystem2Colours

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

Functions:
	.db Function.Initialise \ .dw Initialise
	.db Function.PutMap \ .dw MasterSystem4Colours.PutMap
	.db Function.Scroll \ .dw MasterSystem4Colours.Scroll
	.db Function.BeginPlot \ .dw MasterSystem16Colours.BeginPlot
	.db Function.SetAlignedHorizontalLineSegment \ .dw SetAlignedHorizontalLineSegment
	.db Function.SelectPalette \ .dw SelectPalette
	.db Function.SelectDefaultPalette \ .dw SelectDefaultPalette
	.db Function.PreserveUnderCursor \ .dw MasterSystem4Colours.PreserveUnderCursor
	.db Function.RestoreUnderCursor \ .dw MasterSystem4Colours.RestoreUnderCursor
	.db Function.SetUserDefinedCharacter \ .dw SetUserDefinedCharacter
	.db Function.GetUserDefinedCharacter \ .dw MasterSystem4Colours.GetUserDefinedCharacter
	.db Function.End

Initialise:
	
	call MasterSystem4Colours.Initialise

	; Load the pattern fill data.
	call SetDefaultFillPatterns	
	ret

SetDefaultFillPatterns:
	ld hl,DefaultPatterns
	ld de,FillPatterns
	call MasterSystem16Colours.LoadPackedFillPatterns
	
	; Diagonal stripes are four rows hight unlike the other styles.
	ld hl,FillPatterns+3*8
	call Video.SetWriteAddress
	
	ld b,8
	ld a,%00010001
-:	out (Video.Data),a ; 11
	rlca               ; 4
	dec hl             ; 6
	djnz -             ; 12/7	
	ret

DefaultPatterns:
.db %10101010 ; GCOL 16
.db %00000000
.db %10101010 ; GCOL 32
.db %01010101
.db %11111111 ; GCOL 48
.db %01010101

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
; Inputs:   a = logical palette index (0..1).
;           c = colour to set (6-bit BGR).
; Destroys: af, hl, b.
; ---------------------------------------------------------
WritePaletteEntry:
	
	and %1
	ld h,a
	
	ld b,8
-:	ld a,h
	call Video.SetPalette
	inc h
	inc h
	djnz -
	
	ld a,h
	rlca
	rlca
	add a,16
	ld h,a
	
	; We can set the rest of the palette directly,
	; as it's colours 16..31 which won't end up in the RAM copy.
	ld l,2
--:	call Video.GotoPalette
	
	ld a,c
	ld b,4
-:	out (Video.Data),a ; 11
	inc hl             ; 6
	dec hl             ; 6
	djnz -             ; 13 <- 36
	
	ld a,h
	add a,8
	dec l
	jr nz,--
	
	ei
	ret
	

; ---------------------------------------------------------
; SelectDefaultPalette -> Selects the default palette.
; ---------------------------------------------------------
; Destroys: af, hl, bc.
; ---------------------------------------------------------
SelectDefaultPalette:
	xor a
	ld c,a
	call WritePaletteEntry
	ld c,%00111111
	ld a,c
	jr WritePaletteEntry

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
	call MasterSystem4Colours.GetPatternGeneratorAddressForTile
	
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

	ld a,c
	push de
	
	; Modify the original Draw and Erase bitmasks with our pattern bitmask for the current bitplane.
	and d
	ld d,a
	
	ld a,c
	or e
	ld e,a
	
	; The filler only does a single bitplane at a time.
	in a,(Video.Data)
	call ManipulatePixelBitmask
	ld (hl),a
	
	pop de
	
	jr GeneratedTileRow

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
	ld b,1
	call ManipulatePixelBitmask

GeneratedTileRow:
	
	; Get ready to write the new data to VRAM!
	pop hl
	call Video.SetWriteAddress
	
	ld hl,TempTile
	ld a,(hl)           ; 7
	out (Video.Data),a  ; 11
	
	ei
	ret

SetUserDefinedCharacter:

	cp 11
	jp z,SetDefaultFillPatterns
	
	or a
	jp m,MasterSystem4Colours.SetPattern

	cp 6
	jp c,MasterSystem4Colours.SetPattern

	cp 12
	ret c
	
	cp 15
	ret nc
	
	; Fill a simpler pattern.
	
	push af
	push hl
	
	ld e,l
	ld d,h

	; Pack the two pixel values into a single value.
	ld b,4
-:	xor a
	
	srl (hl)
	inc hl
	rr a
	
	srl (hl)
	inc hl
	rr a
	
	ld c,a
	srl a
	srl a
	or c
	
	ld c,a
	srl a
	srl a
	srl a
	srl a
	or c
	
	; Store, then move on.
	
	ld (de),a
	inc de
	
	djnz -
	
	pop hl
	
	; Copy the top half into the bottom half.
	push hl
	ld bc,4
	ldir
	pop hl
	
	; Subtract 10 then load as a normal (full) tile.
	pop af
	sub 10

	jp MasterSystem4Colours.SetPattern

.endmodule