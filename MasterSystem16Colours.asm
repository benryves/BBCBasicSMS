.module MasterSystem16Colours

Vectors:
	jp Execute
	jp PutMap
	jp BeginPlot
	jp SetAlignedHorizontalLineSegment

PatternGenerator = $0000 ; 14KB, 448 tiles total.
NameTable        = $3800 ; 1536 bytes
FillPatterns     = $3E00 ; 32 bytes
SpriteTable      = $3F00 ; 256 bytes
TopOfMemory      = $4000 ; 16KB

FreeGraphicsTile = allocVar(2) ; What's the next free graphics tile number?

MinGraphicsTile = 128+FontCharOffset
MaxGraphicsTile = 448 ; +1

; 96 tiles for text
; 352 tiles for general graphics?
; 22x16 (176x128)

NameTablePtr = TempTile+0
NameTableEntry = TempTile+2
PatternGeneratorPtr = TempTile+4

Execute:
	or a \ jr z,Initialise
	dec a \ ret z
	dec a \ ret z
	dec a \ ret z
	dec a \ jp z,Scroll
	dec a \ jp z,GetUserDefinedCharacter
	dec a \ jp z,SetUserDefinedCharacter
	dec a \ jp z,PreserveUnderCursor
	dec a \ jp z,RestoreUnderCursor
	dec a \ jp z,SelectPalette
	dec a \ ret z
	dec a \ ret z
	dec a \ ret z
	ret

Initialise:

	; Load the font.
	ld hl,0
	call Video.SetWriteAddress
	
	ld hl,VDU.Fonts.Font8x8
	ld d,128
LoadChar:
	ld c,8
LoadCharRow:
	ld a,(hl)
	inc hl
	ld b,4
-:	out (Video.Data),a
	djnz -
	dec c
	jr nz,LoadCharRow
	dec d
	jr nz,LoadChar
	
	; Load the pattern fill data.
	call SetDefaultFillPatterns
	
	; Set the first free graphics tile index to be the smallest free graphics tile index.
	ld hl,MinGraphicsTile
	ld (FreeGraphicsTile),hl
	
	; Disable sprites
	ld hl,SpriteTable
	call Video.SetWriteAddress
	ld a,$D0
	out (Video.Data),a
	
	; Callback jump.
	ld a,$C3
	ld (Driver.ManipulatePixelBitmask),a

	ret
	

SetDefaultFillPatterns:
	ld hl,DefaultPatterns
	ld de,FillPatterns
	
LoadPackedFillPatterns:
	ex de,hl
	call Video.SetWriteAddress
	ex de,hl
	ld b,4
--:	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld c,4
-:	ld a,e             ; 4
	nop                ; 4
	nop                ; 4
	out (Video.Data),a ; 11 <- 35
	dec c              ; 4
	ld a,d             ; 4
	inc hl             ; 6
	dec hl             ; 6
	nop                ; 4
	out (Video.Data),a ; 11 <- 35
	jr nz,-            ; 12/7
	djnz --
	ret

DefaultPatterns:
.db %00001011 ; 3 1 - GCOL 16
.db %00000111 ; 1 3
.db %00100011 ; 5 1 - GCOL 32
.db %00010011 ; 1 5
.db %00001110 ; 3 2 - GCOL 48
.db %00001101 ; 2 3
.db %00011111 ; 3 7 - GCOL 64
.db %00101111 ; 7 3

AMul64:
	ld l,a
	ld h,0
HLMul64:
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ret

GetNameTableAddressForCursor:
	ld a,(Console.CurRow)
	call AMul64
	ld a,(Console.CurCol)
	add a,a
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
	
	out (Video.Data),a ; 11
	push hl            ; 11
	pop hl             ; 10
	xor a              ;  4 = 36 clocks, > 33 so should be safe for VDP
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
	call AMul64
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
	
	; We'll be moving row by row.
	ld de,64
	
	call Console.ScrollBlock
	
	pop hl
	pop de
	pop bc
	ei
	ret

BeginPlot:
	; Valid plot operators are only in the range 0..7.
	ld d,a
	and %111
	ld l,a
	
	ld h,0
	add hl,hl
	
	; Check if plot mode is 16, 32, 48 or 64 for pattern fills.
	ld a,d
	cp 16
	jr nc,BeginPlotPattern

BeginPlotSolid:
	ld a,$C3 ; JP
	ld (Driver.ManipulatePixelBitmask),a
	ld de,PlotOperators
	add hl,de
	ld de,Driver.ManipulatePixelBitmask+1
	ldi
	ldi
	ret	

BeginPlotPattern:
	ld de,PatternPlotOperators
	add hl,de
	ld de,Driver.ManipulatePixelBitmask
	ldi
	ldi
	ld a,$C9
	ld (de),a ; RET
	ret

PlotOperators:
	.dw ManipulatePixelBitmaskPlot
	.dw ManipulatePixelBitmaskOR
	.dw ManipulatePixelBitmaskAND
	.dw ManipulatePixelBitmaskEOR
	.dw ManipulatePixelBitmaskNOT
	.dw Stub
	.dw ManipulatePixelBitmaskANDNOT
	.dw ManipulatePixelBitmaskORNOT

PatternPlotOperators:
	or d \ and e ; Plot
	or d \ ret ; OR
	and e \ ret ; AND
	xor d \ ret ; EOR
	ret \ ret ; NOP
	ret \ ret ; NOP
	ret \ ret ; NOP
	ret \ ret ; NOP

SetAlignedHorizontalLineSegment:
	
	; IN: (D,E) = (X,Y)
	; H = set pixel mask (OR)
	; L = clear pixel mask (AND)
	
	push hl
	
	; Get the address of the nametable tile from the (X,Y)
	ld a,e
	srl a
	srl a
	srl a
	ld l,a
	ld h,0

	; *32
	ld b,5
-:	add hl,hl
	djnz -
	
	; Add X/8
	ld a,d
	srl a
	srl a
	srl a
	ld c,a
	ld b,0
	add hl,bc
	
	; 2 bytes per nametable entry
	add hl,hl
	ld bc,NameTable
	add hl,bc
	
	ld (NameTablePtr),hl
	
	call Video.SetReadAddress
	
	in a,(Video.Data)
	ld (NameTableEntry+0),a ; 13
	inc hl                  ; 6
	dec hl                  ; 6
	in a,(Video.Data)       ; 11 <- 36
	ld (NameTableEntry+1),a
	
	; If the MSB is set, it's definitely already a graphics tile.
	.if MinGraphicsTile <= 256
	srl a
	jr c,AlreadyMadeGraphicsTile
	.endif
	
	.if MinGraphicsTile % 256 != 0
	ld a,(NameTableEntry+0)
	cp MinGraphicsTile
	jr nc,AlreadyMadeGraphicsTile
	.endif
	
	; We'll need to allocate a graphics tile.
	
	; Write the new nametable entry using the next free graphics tile index.
	call Video.SetWriteAddress
	
	ld hl,(FreeGraphicsTile)
	ld a,l
	out (Video.Data),a
	ld a,h              ; 4
	and %00000001       ; 7
	push hl             ; 11
	out (Video.Data),a  ; 11 <- 33
	
	; Now write the pattern data for the new tile.
	; HL = tile number, so multiply by 32.
	ld b,5
-:	add hl,hl
	djnz -
	ld (PatternGeneratorPtr),hl
	call Video.SetWriteAddress
	
	; The old nametable entry referred to the font, so initialise
	; the new graphics tile with the font data.
	
	ld hl,(NameTableEntry)
	add hl,hl
	add hl,hl
	add hl,hl
	ld bc,Fonts.Font8x8
	add hl,bc
	
	ld b,8
--:	ld c,3
-:	ld a,(hl)          ; 7
	out (Video.Data),a ; 11
	dec c              ; 4
	jr nz,-            ; 12/7 <- 34
	inc hl             ; 6
	ld a,0             ; 7
	out (Video.Data),a ; 11
	djnz --            ; 12/7 <- 35
	
	pop hl
	
	; Increment the graphics tile pointer.
	inc hl
	push hl
	ld bc,-MaxGraphicsTile
	add hl,bc
	ld a,h
	or l
	pop hl
	jr nz,+
	ld hl,MinGraphicsTile
+:	ld (FreeGraphicsTile),hl
	
	; Reload pointer to the pattern generator from earlier.
	ld hl,(PatternGeneratorPtr)
	jr GotGraphicsTile

AlreadyMadeGraphicsTile:
	; Get the pointer to the pattern generator.
	ld hl,(NameTableEntry)
	ld b,5
-:	add hl,hl
	djnz -
	
GotGraphicsTile:
	; Offset the pattern generator address by the Y coordinate inside the tile.
	; That's & 7, * 4 -> or * 4, & (7 * 4)
	ld a,e
	rlca
	rlca
	and %00011100
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

	ld b,4
	; Shift off the last two bits as our LSB for the first and second pixels.
--:	xor a
	srl c
	rr a
	srl c
	rr a
	
	push bc
	
	; Duplicate the two pixels to fill our entire byte row.
	ld b,4
	ld c,a
-:	rrca
	rrca
	or c
	djnz -
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
	djnz --
	
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
	ld b,4
	call Driver.ManipulatePixelBitmask

GeneratedTileRow:
	
	pop hl
	call Video.SetWriteAddress
	
	ld hl,TempTile
	ld b,4
-:	ld a,(hl)          ; 7
	out (Video.Data),a ; 11
	inc hl             ; 6
	djnz -             ; 12/7 <- 36
	
	ei
	ret

ManipulatePixelBitmaskPlot: ; GCOL 0,<c>
-:	in a,(Video.Data) ; 11
	srl c             ; 8
	jr nc,ClearBit    ; 12/7
SetBit:
	or d              ; 4
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12/7 <- 55
	ret
ClearBit:
	and e             ; 4
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12/7 <- 60
	ret

ManipulatePixelBitmaskOR: ; GCOL 1,<c>
-:	in a,(Video.Data) ; 11
	srl c             ; 8
	jr nc,BitOR0      ; 12/7
BitOR1:
	or d              ; 4
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12/7 <- 55
	ret
BitOR0:
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12/7 <- 56
	ret

ManipulatePixelBitmaskAND: ; GCOL 2,<c>
-:	in a,(Video.Data) ; 11
	srl c             ; 8
	jr c,BitAND1     ; 12/7
BitAND0:
	and e             ; 4
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12/7 <- 55
	ret
BitAND1:
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12/7 <- 56
	ret

ManipulatePixelBitmaskEOR: ; GCOL 3,<c>
-:	in a,(Video.Data) ; 11
	srl c             ; 8
	jr nc,BitEOR0     ; 12/7
BitEOR1:
	xor d             ; 4
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12/7 <- 55
	ret
BitEOR0:
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12/7 <- 56
	ret

ManipulatePixelBitmaskNOT: ; GCOL 4,<c>
-:	in a,(Video.Data) ; 11
	xor d             ; 4
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12 <- 40
	ret

ManipulatePixelBitmaskANDNOT: ; GCOL 6,<c>
	ld a,c
	cpl
	ld c,a
	jr ManipulatePixelBitmaskAND

ManipulatePixelBitmaskORNOT: ; GCOL 7,<c>
	ld a,c
	cpl
	ld c,a
	jr ManipulatePixelBitmaskOR

; ---------------------------------------------------------
; SelectPalette -> Selects the palette.
; ---------------------------------------------------------
; Inputs:   b = "physical" colour (from BBC BASIC palette).
;           c = logical colour.
;           hl = pointer to RGB colour (if applicable).
; Destroys: af, hl, bc.
; ---------------------------------------------------------
SelectPalette:
	; Get ready at the logical palette entry.
	push bc
	call ParsePaletteCommand
	pop bc
	ld b,c
	ld c,a
	ld a,b
	and $F
	ld b,a
	call Video.SetPalette
	ld a,b
	add a,16
	call Video.SetPalette
	ret

; ---------------------------------------------------------
; ParsePaletteCommand -> Converts a palette command into
;                        a CRAM colour value.
; ---------------------------------------------------------
; Inputs:   b = "physical" colour (from BBC BASIC palette).
;           hl = pointer to RGB colour (if applicable).
; Outputs:  a = CRAM colour value in 00bbggrr form.
; Destroys: af, hl, bc.
; ---------------------------------------------------------
ParsePaletteCommand:
	ld a,b
	; We need the palette in BGR order.
	inc hl
	inc hl
	
	; Are we setting a physical colour to an RGB value?
	cp -1
	jr z,SelectSixBitRGB
	cp 16
	jr z,SelectEightBitRGB
	
	; We're just loading one of the stock physical colours.
	and $0F
	ld c,a
	ld b,0
	ld hl,VDU.Palettes.SegaMasterSystem
	add hl,bc
	ld a,(hl)
	ret

SelectEightBitRGB:
	ld b,3
-:	ld a,(hl)
	add a,a
	rl c
	add a,a
	rl c
	dec hl
	djnz -
	ld a,c
	ret
	
SelectSixBitRGB:
	ld b,3
-:	ld a,(hl)
	add a,a
	add a,a
	add a,a
	rl c
	add a,a
	rl c
	dec hl
	djnz -
	ld a,c
	ret


PreserveUnderCursor:
	call GetNameTableAddressForCursor
	call Video.SetReadAddress	
	
	in a,(Video.Data)                    ; 11
	ld (VDU.Console.AreaUnderCursor+0),a ; 13
	inc hl \ dec hl                      ; 12 <- 36
	in a,(Video.Data)
	ld (VDU.Console.AreaUnderCursor+1),a
	
	ei
	ret

RestoreUnderCursor:
	call GetNameTableAddressForCursor
	call Video.SetWriteAddress
	
	ld a,(VDU.Console.AreaUnderCursor+0)  ; 13
	out (Video.Data),a                    ; 11
	inc hl \ dec hl                       ; 12 <- 36
	ld a,(VDU.Console.AreaUnderCursor+1)
	out (Video.Data),a
	
	ei
	ret

SetUserDefinedCharacter:
	ld a,c

	cp 11
	jp z,SetDefaultFillPatterns

	cp 6
	jr c,SetFillPattern

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
	
	ld c,4
--:	ld b,4
-:	srl (hl)
	inc hl
	rr a
	srl (hl)
	dec hl
	rr a
	djnz -
	
	; Store, then move on.
	
	ld (de),a
	inc de
	inc hl
	inc hl
	
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

SetFillPattern:

	or a
	ret z
	dec a
	ret z
	dec a
	
	add a,a
	add a,a
	add a,a
	
	ex de,hl
	
	ld c,a
	ld b,0
	ld hl,FillPatterns
	add hl,bc
	
	call Video.SetWriteAddress
	
	ld b,8
-:	ld a,(de)           ; 7
	out (Video.Data),a  ; 11
	inc de              ; 6
	djnz -              ; 13 = 37 clocks
	
	ei
	ret
	

GetUserDefinedCharacter:
	or a
	ret

.endmodule