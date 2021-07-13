.module Mode4

NameTable = $3800

ScrollRowOffset = allocVar(1)

Initialise:

	; The default state from Video.Reset is pretty close to Master System Mode 4 anyway!
	xor a
	ld (ScrollRowOffset),a
	
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
	ld a,(ScrollRowOffset)
	add a,l
	cp 28
	jr c,+
	sub 28
+:	ld l,a
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
	
.endmodule