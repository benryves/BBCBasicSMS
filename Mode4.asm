.module Mode4

PatternGenerator = $0000 ; 14KB, 448 tiles total.
NameTable        = $3800 ; 1792 bytes
SpriteTable      = $3F00 ; 256 bytes
TopOfMemory      = $4000 ; 16KB

ScrollRowOffset = allocVar(1) ; Row number that's at the top of the screen
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
	.db Function.SetGraphicsColour \ .dw SetGraphicsColour
	.db Function.SetAlignedHorizontalLineSegment \ .dw SetAlignedHorizontalLineSegment
	.db Function.SelectPalette \ .dw SelectPalette
	.db Function.End

Initialise:

	; The default state from Video.Reset is pretty close to Master System Mode 4 anyway!
	xor a
	ld (ScrollRowOffset),a
	
	;ld a,5 ; Scroll along to centre
	;ld b,$08
	;call Video.SetRegister

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
	ld b,3
-:	out (Video.Data),a
	djnz -
	xor a
	out (Video.Data),a
	dec c
	jr nz,LoadCharRow
	dec d
	jr nz,LoadChar
	
	; Load the palette
	xor a
	call Video.GotoPalette
	ld hl,VDU.Palettes.SegaMasterSystem
	ld b,16
-:	ld a,(hl)
	inc hl
	out (Video.Data),a
	djnz -

	ld b,16	
-:	dec hl
	ld a,(hl)
	out (Video.Data),a
	djnz -
	
	ld hl,MinGraphicsTile
	ld (FreeGraphicsTile),hl
	
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

PutMap:
	push hl
	push de
	push bc
	push af

	ld a,(Console.CurRow)
	ld l,a
	ld a,(ScrollRowOffset)
	add a,l
	cp 28
	jr c,+
	sub 28
+:	ld l,a
	ld h,0

	; *64
	ld b,6
-:	add hl,hl
	djnz -
	
	ld a,(Console.CurCol)
	add a,a
	
	ld e,a
	ld d,0
	add hl,de
	
.if NameTable != 0
	ld de,NameTable
	add hl,de
.endif
	
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
	ret

Scroll:
	push bc
	push hl
	
	; Adjust the scroll counter and VDP scroll register.
	ld a,(ScrollRowOffset)
	inc a
	cp 28
	jr nz,+
	xor a
+:	ld (ScrollRowOffset),a
	
	add a,a
	add a,a
	add a,a
	ld b,$09
	call Video.SetRegister
	
	; Blank the line that's just hoved into view.
	ld a,(ScrollRowOffset)
	add a,23
	cp 28
	jr c,+
	sub 28
+:	
	ld l,a
	ld h,0
	ld b,6
-	add hl,hl
	djnz -
	
.if NameTable != 0
	ld bc,NameTable
	add hl,bc
.endif
	
	call Video.SetWriteAddress
	ei
	
	ld b,32
-:	ld a,' '*1+FontCharOffset
	out (Video.Data),a
	push hl
	pop hl
	xor a
	out (Video.Data),a
	nop
	nop
	djnz -
	
	pop hl
	pop bc
	ret

BeginPlot:
	dec a
	jr z,SetForegroundPixel
	dec a
	jr z,InvertPixel

SetBackgroundPixel:
	ld hl,GetPixelBackgroundColour
	ld (ManipulatePixelColour+1),hl
	ld hl,ManipulatePixelBitmaskPlot
	ld (ManipulatePixelBitmask+1),hl
	ret

InvertPixel:
	ld hl,Stub
	ld (ManipulatePixelColour+1),hl
	ld hl,ManipulatePixelBitmaskInvert
	ld (ManipulatePixelBitmask+1),hl
	ret

SetForegroundPixel:
	ld hl,GetPixelForegroundColour
	ld (ManipulatePixelColour+1),hl
	ld hl,ManipulatePixelBitmaskPlot
	ld (ManipulatePixelBitmask+1),hl
	ret

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
	ld a,(ScrollRowOffset)
	add a,l
	cp 28
	jr c,+
	sub 28
+:	ld l,a
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
	
	call ManipulatePixelColour
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
	
	ret

GetPixelForegroundColour:
	ld a,(Graphics.Colour)
	rrca
	rrca
	rrca
	rrca
	ld c,a
	ret

GetPixelBackgroundColour:
	ld a,(Graphics.Colour)
	ld c,a
	ret

ManipulatePixelBitmaskPlot:
	ld b,4
-:	in a,(Video.Data) ; 11
	srl c             ; 8
	jr nc,ClearBit    ; 12/7
SetBit:
	or d              ; 4
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12/7
	ret
ClearBit:
	and e             ; 4
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12/7
	ret
	
ManipulatePixelBitmaskInvert:
	ld b,4
-:	in a,(Video.Data) ; 11
	xor d             ; 4
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12 <- 40
	ret

SetGraphicsColour:
	ld hl,Graphics.Colour
	jr SetColour

SetConsoleColour:
	ld hl,Console.Colour

SetColour:
	or a
	ld e,a
	ld a,(hl)
	ld d,a
	jp p,SetForegroundColour
	
SetBackgroundColour:
	ld a,d
	and $F0
	ld d,a
	ld a,e
	and $0F
	or d
	ld (hl),a
	ret

SetForegroundColour:
	ld a,d
	and $0F
	ld d,a
	ld a,e
	rrca
	rrca
	rrca
	rrca
	and $F0
	or d
	ld (hl),a
	ret

SelectPalette:
	; Get ready at the logical palette entry.
	push af
	ld a,c
	and $0F
	call Video.GotoPalette
	pop af

	; We need the palette in BGR order.
	inc hl
	inc hl
	
	; Are we setting a physical colour to an RGB value?
	cp -1
	jr z,SelectSixBitRGB
	cp 16
	jr z,SelectEightBitRGB
	
	; We're just loading one of the stock physical colours.
	ld a,b
	and $0F
	ld c,a
	ld b,0
	ld hl,VDU.Palettes.SegaMasterSystem
	add hl,bc
	ld a,(hl)
	
	out (Video.Data),a
	ei
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
	out (Video.Data),a
	ei
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
	out (Video.Data),a
	ei
	ret
	



.endmodule