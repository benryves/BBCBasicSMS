.module Mode4

PatternGenerator = $0000 ; 14KB, 448 tiles total.
NameTable        = $3800 ; 1792 bytes
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

Functions:
	.db Function.Initialise \ .dw Initialise
	.db Function.PutMap \ .dw PutMap
	.db Function.Scroll \ .dw Scroll
	.db Function.BeginPlot \ .dw BeginPlot
	.db Function.SetPixel \ .dw SetPixel
	.db Function.SetAlignedHorizontalLineSegment \ .dw SetAlignedHorizontalLineSegment
	.db Function.SelectPalette \ .dw SelectPalette
	.db Function.PreserveUnderCursor \ .dw PreserveUnderCursor
	.db Function.RestoreUnderCursor \ .dw RestoreUnderCursor
	.db Function.End

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
	
	; Load the palette
	ld de,2
--:	ld hl,VDU.Palettes.SegaMasterSystem
	ld b,16
-:	ld a,d
	ld c,(hl)
	call Video.SetPalette
	inc hl
	inc d
	djnz -
	dec e
	jr nz,--
	
	ld hl,MinGraphicsTile
	ld (FreeGraphicsTile),hl
	
	; Disable sprites
	ld hl,SpriteTable
	call Video.SetWriteAddress
	ld a,$D0
	out (Video.Data),a
	
	; Callback jump.
	ld a,$C3
	ld (ManipulatePixelBitmask),a

	ret


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
	push hl
	push de
	push bc
	push af

	call GetNameTableAddressForCursor
	call Video.SetWriteAddress
	ei
	
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
	ld h,0
	ld l,a
	add hl,hl
	ld de,PlotOperators
	add hl,de
	ld de,ManipulatePixelBitmask+1
	ldi
	ldi
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

SetPixel:
	; IN (D,E) = (X,Y)
	
	; Generate the masking values.
	ld a,d
	and %00000111
	ld h,%10000000
	jr z,+
	ld b,a
-:	srl h
	djnz -
+:	ld a,h
	cpl
	ld l,a

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
	ld b,4
	call ManipulatePixelBitmask

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
; Inputs:   a = "physical" colour (from BBC BASIC palette).
;           b = "physical" colour.
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
; Inputs:   a = "physical" colour (from BBC BASIC palette).
;           hl = pointer to RGB colour (if applicable).
; Outputs:  a = CRAM colour value in 00bbggrr form.
; Destroys: af, hl, bc.
; ---------------------------------------------------------
ParsePaletteCommand:
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

.endmodule