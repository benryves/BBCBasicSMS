.module GraphicsII

Functions:
	.db Function.Initialise \ .dw Initialise
	.db Function.PutMap \ .dw PutMap
	.db Function.Scroll \ .dw Scroll
	.db Function.BeginPlot \ .dw BeginPlot
	.db Function.SetPixel \ .dw SetPixel
	.db Function.SetUserDefinedCharacter \ .dw SetUserDefinedCharacter
	.db Function.SetConsoleColour \ .dw SetConsoleColour
	.db Function.SetGraphicsColour \ .dw SetGraphicsColour
	.db Function.SetAlignedHorizontalLineSegment \ .dw SetAlignedHorizontalLineSegment
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

ScrollValue = allocVar(1)

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
	ld (ScrollValue),a
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
	
	; Make text background colour transparent.
	ld a,(VDU.Console.Colour)
	and %11110000
	ld (VDU.Console.Colour),a
	
	; 6KB of colour data
	ld bc,kb(6)/256
	
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

PutMap:
	push hl
	push de
	push bc
	push af

	; Row *32, *8 = * 256
	ld a,(Console.CurRow)
	ld l,a	
	and %11111000
	ld h,a
	
	ld a,(ScrollValue)
	add a,l
	and %00000111
	or h
	ld h,a
	
	ld l,0

	; Column *8
	ld a,(Console.CurCol)
	add a,a
	add a,a
	add a,a
	
	ld e,a
	ld d,0
	add hl,de
	
	pop af  ; Get the character number again
	push hl ; Save the address offset for later...
	
	; Is it a user-defined character?
	cp $80
	jr c,ROMFont
	
	; Yes, so read back the user-defined character.
	call GetUserDefinedCharacterAddress
		
	call Video.SetReadAddress
	ld hl,TempTile
	push hl
	ld b,8
	
-:	in a,(Video.Data)   ; 11
	ld (hl),a           ; 7
	inc hl              ; 6
	djnz -              ; 13 = 37 clocks
	
	pop de
	
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
	ld b,8	
-:	out (Video.Data),a  ; 11
	nop                 ; 4
	nop                 ; 4
	djnz -              ; 13 = 32 clocks
	
	pop bc
	pop de
	pop hl
	ret

Scroll:
	push bc
	push de
	push hl
	
	; Fill name table with sensible data
	ld hl,NameTable
	call Video.SetWriteAddress
	
	; 768 bytes (3*256) that just count up from 0..255 and loop.
	ld a,(ScrollValue)
	
	ld d,a ; for later...
	ld e,0
	
	inc a
	and 7
	ld (ScrollValue),a	
	
	; Top window
	
	ld hl,NameTable + 0 * 32
	call RotateWindow
	
	ld hl,PatternGenerator + 8 * 256
	call CopyToWindowAbove
	
	ld hl,ColourTable + 8 * 256
	call CopyToWindowAbove
	
	; Middle window
	
	ld hl,NameTable + 8 * 32
	call RotateWindow
	
	ld hl,PatternGenerator + 16 * 256
	call CopyToWindowAbove

	ld hl,ColourTable + 16 * 256
	call CopyToWindowAbove

	; Bottom window
	
	ld hl,NameTable + 16 * 32
	call RotateWindow
	
	; Clear bottom row
	
	ld hl,PatternGenerator + 16 * 256
	add hl,de
	call Video.SetWriteAddress
	ei

	ld b,0
	xor a
-:	out (Video.Data),a ; 11
	nop                ; 4
	nop                ; 4
	djnz -             ; 13 = 32
	
	ld hl,ColourTable + 16 * 256
	add hl,de
	call Video.SetWriteAddress
	ei
	
	ld b,0
	ld a,(VDU.Console.Colour)
-:	out (Video.Data),a ; 11
	nop                ; 4
	nop                ; 4
	djnz -             ; 13 = 32
	
	pop hl
	pop de
	pop bc
	ret

; HL = address of nametable window to rotate
RotateWindow:
	; Multiply by 32.
	ld a,(ScrollValue)
	ld b,5
-:	add a,a
	djnz -
	
	call Video.SetWriteAddress
	ei

-:	out (Video.Data),a ; 11
	inc a              ; 4
	djnz -             ; 13 = 28
	
	ret

; HL = address of where to copy data from
; DE = offset into scrolled area (previous scroll value * 256)
CopyToWindowAbove:
	push hl
	add hl,de
	call Video.SetReadAddress
	ei
	
	ld hl,(Basic.BBCBASIC_FREE)
	ld b,0
-:	in a,(Video.Data) ; 11
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 13 = 37
	
	pop hl
	ld bc,-8*256
	add hl,bc
	
	add hl,de
	call Video.SetWriteAddress
	ei
	
	ld hl,(Basic.BBCBASIC_FREE)
	ld b,0
-:	ld a,(hl)          ; 7
	out (Video.Data),a ; 11
	inc hl             ; 6
	djnz -             ; 13 = 37
	
	ret

BeginPlot:
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

	; Character row = Y / 8
	
	; Row *32, *8 = * 256
	ld a,e
	srl a
	srl a
	srl a
	ld l,a	
	and %11111000
	ld h,a
	
	ld a,(ScrollValue)
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
	ld a,(VDU.Graphics.Colour)
	and %11110000
	or c
	ret

ManipulatePixelColour.SetBackground:
	and %11110000
	ld c,a
	ld a,(VDU.Graphics.Colour)
	and %00001111
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
	
	ex de,hl
	ld bc,8*256 + Video.Data
	otir
	
	ei
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
	push hl
	ld d,a
	ld bc,Palettes.TMS9918A
	jp p,SetForegroundColour
	
SetBackgroundColour:
	ld a,d
	and $F0
	ld d,a
	
	ld a,e
	and $0F
	ld l,a
	ld h,0
	add hl,bc
	ld a,(hl)
	and $0F
	or d
	pop hl
	ld (hl),a
	ret

SetForegroundColour:
	ld a,d
	and $0F
	ld d,a
	
	ld a,e
	and $0F
	ld l,a
	ld h,0
	add hl,bc
	ld a,(hl)
	rrca
	rrca
	rrca
	rrca
	and $F0
	or d
	pop hl
	ld (hl),a
	ret

.endmodule