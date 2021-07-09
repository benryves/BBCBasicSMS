.module Text

NameTable = $3800

Initialise:
	
	; Switch video mode to TMS9918 TEXT I
	
	ld a,%00000000
	ld b,$00
	call Video.SetRegister
	
	ld a,%10010000 ; M1 = 1
	ld b,$01
	call Video.SetRegister
	
	; Name table is at $3800
	ld a,NameTable/$0400
	ld b,$02
	call Video.SetRegister
	
	; Patterns are at the start of VRAM so set pattern generator table address to 0
	xor a
	ld b,$04
	call Video.SetRegister
	
	; Set background/foreground colour
	ld a,%11110100 ; TC, BG
	ld b,$07
	call Video.SetRegister
	
	ld a,0
	ld (MinRow),a
	ld a,24
	ld (MaxRow),a
	
	ld a,2
	ld (MinCol),a
	ld a,40
	ld (MaxCol),a
	
	; Load the font
	
	ld hl,0
	call Video.GotoHL
	
	ld hl,Font
	ld bc,Video.Data ; B = 0, C = Video.Data
	
	otir
	otir
	otir
	
	; Set up vectors
	ld hl,PutMap
	ld (Parent.PutMap+1),hl
	
	ret

PutMap:
	push hl
	push de
	push bc
	push af
	ld a,(CurRow)
	ld l,a
	ld h,0
	
	; *40 = *32+*8
	add hl,hl ; *2
	add hl,hl ; *4
	add hl,hl ; *8
	ld e,l
	ld d,h
	add hl,hl ; *16
	add hl,hl ; *32
	add hl,de ; *40
	
	ld a,(CurCol)
	ld e,a
	ld d,0
	add hl,de
	ld de,NameTable
	add hl,de
	call Video.GotoHL
	pop af
	add a,FontCharOffset
	out (Video.Data),a
	pop bc
	pop de
	pop hl
	ret

Font:
	.incbin "Font6x8.bin"

.endmodule