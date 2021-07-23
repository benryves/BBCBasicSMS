.module Text

Functions:
	.db Function.Initialise \ .dw Initialise
	.db Function.PutMap \ .dw PutMap
	.db Function.Scroll \ .dw Scroll
	.db Function.SetConsoleColour \ .dw SetConsoleColour
	.db Function.End

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
	ld a,$F1 ; TC, BG
	ld b,$07
	call Video.SetRegister
	
	ld a,0
	ld (Console.MinRow),a
	ld a,23
	ld (Console.MaxRow),a
	
	ld a,2
	ld (Console.MinCol),a
	ld a,39
	ld (Console.MaxCol),a
	
	; Load the font
	
	ld hl,0
	call Video.SetWriteAddress
	
	ld hl,VDU.Fonts.Font6x8
	ld bc,Video.Data ; B = 0, C = Video.Data
	
	otir
	otir
	otir
	
	ret

PutMap:
	push hl
	push de
	push bc
	push af
	ld a,(Console.CurRow)
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
	
	ld a,(Console.CurCol)
	ld e,a
	ld d,0
	add hl,de
.if NameTable != 0
	ld de,NameTable
	add hl,de
.endif
	call Video.SetWriteAddress
	pop af
	add a,FontCharOffset
	out (Video.Data),a
	pop bc
	pop de
	pop hl
	ret

Scroll:
	push bc
	push de
	push hl
	
	ld de,0
	ld b,23
	
	ei
	halt

-:	ld a,b
	and %111
	jr nz,+
	
	ei
	halt

+:	push bc
	
	ld hl,NameTable + 40 ; Source to copy from
	add hl,de
	
	call Video.SetReadAddress
	ld hl,(Basic.BBCBASIC_FREE)
	ld bc,40*256 + Video.Data
	inir
	
	ld hl,NameTable ; Destination to copy to
	add hl,de
	call Video.SetWriteAddress
	
	ld hl,(Basic.BBCBASIC_FREE)
	ld bc,40*256 + Video.Data
	otir
	
	ld hl,40
	add hl,de
	ex de,hl

	pop bc
	djnz -
	
	ld hl,NameTable + 23*40
	call Video.SetWriteAddress
	
	ld a,' '*1+FontCharOffset
	ld b,40
-:	out (Video.Data),a
	djnz -
	
	pop hl
	pop de
	pop bc
	ei
	ret

SetConsoleColour:
	call GraphicsII.SetConsoleColour
	ld a,(Console.Colour)
	ld b,$07
	call Video.SetRegister
	ei
	ret

.endmodule