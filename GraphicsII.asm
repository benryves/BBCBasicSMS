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
	ld hl,(CurRow-1)
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
	
	ld de,PatternGenerator | $4000 ; Write
	add hl,de
	ex de,hl
	
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
-:	ld a,(hl)
	call Video.Enqueue
	inc hl
	inc de
	djnz -
	
	; Write the colour data
	pop hl
	
	ld de,ColourTable | $4000
	add hl,de
	ex de,hl
	
	ld a,(VDU.TextColour)
	ld b,8	
-:	call Video.Enqueue
	inc de
	djnz -
	
	pop bc
	pop de
	pop hl
	ret

Scroll:
	push bc
	push de
	push hl
	
	call Video.WaitForEmptyQueue
	
	; Fill name table with sensible data
	ld hl,NameTable
	call Video.SetWriteAddress
	
	; 768 bytes (3*256) that just count up from 0..255 and loop.
	ld a,(ScrollValue)
	add a,32
	ld (ScrollValue),a
	
	ld bc,3
-:	out (Video.Data),a
	inc a
	djnz -
	dec c
	jr nz,-

	
	pop hl
	pop de
	pop bc
	ret
.endmodule