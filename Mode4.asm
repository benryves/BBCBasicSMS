.module Mode4

Initialise:

	; The default state from Video.Reset is pretty close to Master System Mode 4 anyway!
	
	ld a,5 ; Scroll along to centre
	ld b,$08
	call Video.SetRegister

	; Load the font.
	ld hl,0
	call Video.SetWriteAddress
	
	ld hl,Font
	ld d,(Font.End-Font)/8
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
	xor a
	call Video.GotoPalette
	ld hl,Palette
	ld b,32
-:	ld a,(hl)
	inc hl
	out (Video.Data),a
	djnz -
	
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

Font:
.include "bbc"
Font.End:


Palette:
.db %010000, %000000, %000000, %000000, %000000, %000000, %000000, %000000 ; Tiles   0..7
.db %000000, %000000, %000000, %000000, %000000, %000000, %000000, %111111 ; Tiles   8..F
.db %000000, %011101, %000011, %110000, %000000, %000000, %000000, %000000 ; Sprites 0..7
.db %000000, %000000, %000000, %000000, %000000, %000000, %000000, %010000 ; Sprites 8..F

PutMap:
	push hl
	push de
	push bc
	push af

	ld a,(CurRow)
	ld l,a
	ld h,0

	; *32
	ld b,6
-:	add hl,hl
	djnz -
	
	ld a,(CurCol)
	add a,a
	
	ld e,a
	ld d,0
	add hl,de
	
	ld de,$3800|$4000
	add hl,de
	ex de,hl
	
	pop af
	add a,FontCharOffset
	
	call Video.Enqueue
	
	xor a
	inc de
	call Video.Enqueue
	
	pop bc
	pop de
	pop hl
	ret

Scroll:
	ret

.endmodule