.module Mode4

NameTable = $3800

Initialise:

	; The default state from Video.Reset is pretty close to Master System Mode 4 anyway!
	
	ld a,5 ; Scroll along to centre
	ld b,$08
	call Video.SetRegister

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
	push bc
	push de
	push hl
	
	call Video.WaitForEmptyQueue
	
	ld de,0
	ld b,23
	
	ei
	halt

-:	ld a,b
	and %11
	jr nz,+
	
	ei
	halt

+:	push bc
	
	ld hl,NameTable + 64 ; Source to copy from
	add hl,de
	
	call Video.SetReadAddress
	ld hl,(Basic.BBCBASIC_FREE)
	ld bc,64*256 + Video.Data
	inir
	
	ld hl,NameTable ; Destination to copy to
	add hl,de
	call Video.SetWriteAddress
	
	ld hl,(Basic.BBCBASIC_FREE)
	ld bc,64*256 + Video.Data
	otir
	
	ld hl,64
	add hl,de
	ex de,hl

	pop bc
	djnz -
	
	ld hl,NameTable+23*64
	call Video.SetWriteAddress
	
	ld b,32
-:	ld a,' '*1+FontCharOffset
	out (Video.Data),a
	xor a
	out (Video.Data),a
	djnz -
	
	pop hl
	pop de
	pop bc
	ret
.endmodule