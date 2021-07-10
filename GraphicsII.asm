.module GraphicsII

PatternGenerator = $0000 ; 6KB
ColourTable      = $2000 ; 6KB
NameTable        = $3800 ; 768 bytes

.if NameTable % $400
.echoln "Bad address for Graphics II Name Table"
.endif
.if ColourTable % kb(8)
.echoln "Bad address for Graphics II Colour Table Generator"
.endif
.if PatternGenerator % kb(8)
.echoln "Bad address for Graphics II Pattern Generator"
.endif

.var ubyte ScrollValue

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
	ld a,(VDU.TextColour) ; TC, BG
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
	ld a,(VDU.TextColour)
	and %11110000
	ld (VDU.TextColour),a
	
	; 6KB of colour data
	ld bc,kb(6)/256
	
-:	out (Video.Data),a
	djnz -
	dec c
	jr nz,-
	
	; Set up vectors
	ld hl,PutMap
	ld (Parent.PutMap+1),hl
	ld hl,Scroll
	ld (Parent.Scroll+1),hl
	ld hl,SetPixel
	ld (Parent.SetPixel+1),hl
	
	; Screen bounds
	ld a,0
	ld (MinRow),a
	ld a,24
	ld (MaxRow),a
	
	ld a,2
	ld (MinCol),a
	ld a,30
	ld (MaxCol),a

	ret


PutMap:
	push hl
	push de
	push bc
	push af

	; Row *32, *8 = * 256
	ld a,(CurRow)
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
	ld a,(CurCol)
	add a,a
	add a,a
	add a,a
	
	ld e,a
	ld d,0
	add hl,de
	
	pop af  ; Get the character number again
	push hl ; We'll need the address offset for the colour too
	
.if PatternGenerator != 0
	ld de,PatternGenerator
	add hl,de
.endif
	call Video.SetWriteAddress
	
	; Get the address of the data in the font.
	add a,FontCharOffset
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld bc,VDU.Fonts.Font8x8
	add hl,bc

	; Write the font data
	ld b,8	
-:	ld a,(hl)           ; 7
	out (Video.Data),a  ; 11
	inc hl              ; 6
	djnz -              ; 13 = 37 clocks
	
	; Write the colour data
	pop hl
	
	ld de,ColourTable
	add hl,de
	call Video.SetWriteAddress
	
	ld a,(VDU.TextColour)
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
	ld a,(VDU.TextColour)
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

SetPixel:

	; IN (D,E) = (X,Y)

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
	
	; HL -> offset into VRAM to set pixel

	push hl ; Store offset

.if PatternGenerator != 0
	ld de,PatternGenerator
	add hl,bc
.endif

	
	; Bitmask
	ld c,%10000000
	
	ld a,d
	and %00000111
	jr z,+
	ld b,a
-:	srl c
	djnz -
+:

	call Video.SetReadAddress
	in a,(Video.Data)

	or c
	
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
	
	and %00001111
	ld c,a
	
	ld a,(VDU.GraphicsColour)
	and %11110000
	or c
	
	call Video.SetWriteAddress
	out (Video.Data),a
	
	ei
	
	ret

.endmodule