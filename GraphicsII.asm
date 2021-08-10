.module GraphicsII

Functions:
	.db Function.Initialise \ .dw Initialise
	.db Function.PutMap \ .dw PutMap
	.db Function.Scroll \ .dw Scroll
	.db Function.BeginPlot \ .dw BeginPlot
	.db Function.SetUserDefinedCharacter \ .dw SetUserDefinedCharacter
	.db Function.GetUserDefinedCharacter \ .dw GetUserDefinedCharacter
	.db Function.SetAlignedHorizontalLineSegment \ .dw SetAlignedHorizontalLineSegment
	.db Function.SelectPalette \ .dw MasterSystem16Colours.SelectPalette
	.db Function.PreserveUnderCursor \ .dw PreserveUnderCursor
	.db Function.RestoreUnderCursor \ .dw RestoreUnderCursor
	.db Function.End

PatternGenerator = $0000 ; 6KB
ColourTable      = $2000 ; 6KB
NameTable        = $3800 ; 768 bytes
UserDefinedChars = $3B00 ; 1280 bytes = 160 chars
TopOfMemory      = $4000 ; 16KB

.if NameTable % $400
.echoln "Bad address for Graphics II Name Table"
.endif
.if ColourTable % kb(8)
.echoln "Bad address for Graphics II Colour Table Generator"
.endif
.if PatternGenerator % kb(8)
.echoln "Bad address for Graphics II Pattern Generator"
.endif

Initialise:

	; Switch video mode to TMS9918 GRAPHICS II
	
	ld a,%00000010 ; M3 = 1
	ld b,$00
	call Video.SetRegister
	
	ld a,%10000000
	ld b,$01
	call Video.SetRegister
	
	; Name table address
	ld a,NameTable / $400
	ld b,$02
	call Video.SetRegister
	
	; Colour table address
	ld a,ColourTable / $40
	ld b,$03
	call Video.SetRegister
	
	; Patterns generator address
	ld a,PatternGenerator / $800
	ld b,$04
	call Video.SetRegister
	
	; Set background/foreground colour
	ld a,$F1 ; TC, BG
	ld b,$07
	call Video.SetRegister

	; Fill name table with sensible data
	ld hl,NameTable
	call Video.SetWriteAddress
	
	; 768 bytes (3*256) that just count up from 0..255 and loop.
	xor a
	ld b,a
	ld c,3
-:	out (Video.Data),a
	inc a
	djnz -
	dec c
	jr nz,-
	
	; Reset the colours to sensible defaults.
	ld hl,ColourTable
	call Video.SetWriteAddress
	
	; 6KB of colour data
	ld bc,kb(6)/256
	
	xor a
-:	out (Video.Data),a
	djnz -
	dec c
	jr nz,-
	
	; Set up my own vectors!
	ld a,$C3 ; JP
	ld (ManipulatePixelColour),a
	
	ld a,$C9 ; RET
	ld (ManipulatePixelBitmask+1),a
	ret

; ---------------------------------------------------------
; GetVRAMOffsetForCursor -> gets the offset into VRAM for
; a particular cursor position.
; ---------------------------------------------------------
; Inputs:   CurCol, CurRow
; Outputs:  hl = offset to VRAM data for tile at (e,a).
; Destroys: af, de, b.
; ---------------------------------------------------------
GetVRAMOffsetForCursor:
	ld a,(Console.CurCol)
	ld e,a
	ld a,(Console.CurRow)

; ---------------------------------------------------------
; GetVRAMOffsetForCursor -> gets the offset into VRAM for
; a particular cursor position.
; ---------------------------------------------------------
; Inputs:   a = tile row.
;           e = tile column.
; Outputs:  hl = offset to VRAM data for tile at (e,a).
; Destroys: af, b.
; ---------------------------------------------------------
GetVRAMOffsetForTile:
	; Row *32, *8 = * 256
	ld l,a	
	and %11111000
	ld h,a
	
	ld a,0 ; <- optimise
	add a,l
	and %00000111
	or h
	ld h,a
	
	ld l,0

	; Column *8
	ld a,e
	add a,a
	add a,a
	add a,a
	ld e,a
	
	ld d,0
	add hl,de
	ret

PutMap:
	push hl
	push de
	push bc
	push af

	call GetVRAMOffsetForCursor
	
	pop af  ; Get the character number again
	push hl ; Save the address offset for later...
	
	; Is it a user-defined character?
	cp $80
	jr c,ROMFont
	
	; Yes, so read back the user-defined character.
	call GetUserDefinedCharacter
	ex de,hl
	
	jr WriteFontData

ROMFont:
	
	; Get the address of the data in the font.
	add a,FontCharOffset
	add a,a
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	ld bc,VDU.Fonts.Font8x8
	add hl,bc
	ex de,hl

WriteFontData:

	; Set the write address.
	pop hl
	push hl
.if PatternGenerator != 0
	ld bc,PatternGenerator
	add hl,bc
.endif
	call Video.SetWriteAddress

	; Write the font data
	ld b,8	
-:	ld a,(de)           ; 7
	out (Video.Data),a  ; 11
	inc de              ; 6
	djnz -              ; 13 = 37 clocks
	
	; Write the colour data
	pop hl
	
	ld de,ColourTable
	add hl,de
	call Video.SetWriteAddress
	
	ld a,(VDU.Console.Colour)
	call VDU.Palettes.ConvertColourPairToTMS9918
	
	ld b,8	
-:	out (Video.Data),a  ; 11
	nop                 ; 4
	nop                 ; 4
	djnz -              ; 13 = 32 clocks
	
	pop bc
	pop de
	pop hl
	ei
	ret

Scroll:
	push bc
	push de
	push hl
	
	; Get the pointer to the top left corner in the pattern table.
	ld a,(Console.MinCol)
	ld e,a
	ld a,(Console.MinRow)
	call GetVRAMOffsetForTile
	
	push hl

.if PatternGenerator != 0
	ld de,PatternGenerator
	add hl,de
.endif
	
	; How many columns will we need to move?
	ld a,(Console.MinCol)
	ld c,a
	ld a,(Console.MaxCol)
	sub c
	inc a
	add a,a
	add a,a
	add a,a
	ld c,a
	
	; How many rows will we need to move?
	ld a,(Console.MinRow)
	ld b,a
	ld a,(Console.MaxRow)
	sub b
	inc a
	ld b,a
	
	push bc
	
	ld de,256
	call Console.ScrollBlock
	
	pop bc
	pop hl

.if ColourTable != 0
	ld de,ColourTable
	add hl,de
.endif

	ld de,256	
	call Console.ScrollBlock
	
	pop hl
	pop de
	pop bc
	ei
	ret

BeginPlot:
	ld a,(Graphics.PlotShape)
	and 3
	dec a
	jr z,SetForegroundPixel
	dec a
	jr z,InvertPixel

SetBackgroundPixel:
	ld hl,ManipulatePixelColour.SetBackground
	ld (ManipulatePixelColour+1),hl
	ld a,$A1 ; AND C
	ld (ManipulatePixelBitmask+0),a
	ret

InvertPixel:
	ld hl,ManipulatePixelColour.Invert
	ld (ManipulatePixelColour+1),hl
	ld a,$A8 ; XOR B
	ld (ManipulatePixelBitmask+0),a
	ret

SetForegroundPixel:
	ld hl,ManipulatePixelColour.SetForeground
	ld (ManipulatePixelColour+1),hl
	ld a,$B0 ; OR B
	ld (ManipulatePixelBitmask+0),a
	ret

SetAlignedHorizontalLineSegment:
	
	; IN: (D,E) = (X,Y)
	; H = set pixel mask (OR)
	; L = clear pixel mask (AND)
	push hl

	; Character row = Y / 8
	
	; Row *32, *8 = * 256
	ld a,e
	srl a
	srl a
	srl a
	ld l,a	
	and %11111000
	ld h,a
	
	ld a,0 ; <- optimise
	add a,l
	and %00000111
	or h
	ld h,a
	
	; Offset by row value
	ld a,e
	and %00000111
	ld l,a

	; Character column = X / 8

	; Column / 8, * 8 = truncate
	ld a,d
	and %11111000
	
	ld c,a
	ld b,0
	add hl,bc
	
	; Retrieve bitmask
	pop bc
	
	; HL -> offset into VRAM to set pixel
	push hl ; Store offset

.if PatternGenerator != 0
	ld de,PatternGenerator
	add hl,de
.endif

	call Video.SetReadAddress
	in a,(Video.Data)

	call ManipulatePixelBitmask
	
	call Video.SetWriteAddress
	out (Video.Data),a
	
	ei
	
	pop hl ; Restore offset into VRAM

.if ColourTable != 0
	ld bc,ColourTable
	add hl,bc
.endif

	call Video.SetReadAddress
	in a,(Video.Data)
	
	call ManipulatePixelColour
	
	call Video.SetWriteAddress
	out (Video.Data),a
	
	ei
	
	ret

ManipulatePixelColour.SetForeground:
	and %00001111
	ld c,a
	ld a,(VDU.Graphics.ForegroundColour)
	call VDU.Palettes.ConvertPaletteIndexToTMS9918A
	rlca
	rlca
	rlca
	rlca
	or c
	ret

ManipulatePixelColour.SetBackground:
	and %11110000
	ld c,a
	ld a,(VDU.Graphics.BackgroundColour)
	rrca
	rrca
	rrca
	rrca
	call VDU.Palettes.ConvertPaletteIndexToTMS9918A
	or c
	ret

ManipulatePixelColour.Invert:
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

PreserveUnderCursor:
	xor a
	jr ManageCursor

RestoreUnderCursor:
	ld a,-1
	
ManageCursor:
	ld b,a
	
	call GetVRAMOffsetForCursor
	push hl
	
	.if ColourTable != 0
	ld de,ColourTable
	add hl,de
	.endif
	
	pop de  ; DE = offset into VRAM
	push hl
	
	.if PatternGenerator == 0
	push de
	.else
	ld hl,PatternGenerator
	add hl,de
	push hl
	.endif
	
	ld c,2
	
	ld a,b
	or a
	jr nz,ManageCursorWrite

ManageCursorRead:	
	
--:	pop hl
	call Video.SetReadAddress
	ld de,VDU.Console.AreaUnderCursor+0
	bit 0,c
	jr z,+
	ld de,VDU.Console.AreaUnderCursor+8
+:	ld b,8
-:	in a,(Video.Data) ; 11
	ld (de),a         ; 7
	inc de            ; 6
	djnz -            ; 13/8 <- 37
	dec c
	jr nz,--
	
	ei
	ret

ManageCursorWrite:

--:	pop hl
	call Video.SetWriteAddress
	ld de,VDU.Console.AreaUnderCursor+0
	bit 0,c
	jr z,+
	ld de,VDU.Console.AreaUnderCursor+8
+:	ld b,8
-:	ld a,(de)          ;  7
	out (Video.Data),a ; 11
	inc de             ; 6
	djnz -             ; 13/8 <- 37
	dec c
	jr nz,--
	

	ei
	ret

.endmodule