; Script
.incscript "Scripts.cs"

; Definitions
.include "System.inc"

; BBC BASIC's scratch memory will be at RAM ($C000..$C2FF)
.varloc Memory.Ram + 768, 256

.org $00
	di
	im 1
	jp Boot

.org $38
	reti

.org $66
	retn

; Libraries:
.include "Video.asm"
.include "Host.asm"
.include "AT.asm"
.include "Keyboard.asm"
.include "UK.inc"

Boot:
	; Make sure SP points somewhere sensible.
	ld sp,Memory.Stack
	
	; Set paging to something sensible.
	xor a
	ld ($FFFC),a
	ld ($FFFD),a
	inc a
	ld ($FFFE),a
	inc a
	ld ($FFFF),a

Main:
	
	; Reset the VDP to sensible defaults.
	call Video.Reset
	
	; Clear VRAM and palette.
	call Video.ClearAll
	
	; Load the font.
	ld hl,FontTileIndex*8
	call Video.GotoHL
	
	ld hl,Font
	ld d,(Font.End-Font)/8
LoadChar:
	ld c,8
LoadCharRow:
	ld a,(hl)
	inc hl
	ld b,4
-:	out (Ports.Video.Data),a
	djnz -
	dec c
	jr nz,LoadCharRow
	dec d
	jr nz,LoadChar
	
	; Load the palette
	call LoadPalette
	
	call Video.ScreenOn	
	
	xor a
	ld (MinRow),a
	ld (CurRow),a
	ld a,24
	ld (MaxRow),a
	
	ld a,2
	ld (MinCol),a
	ld (CurCol),a
	ld a,30
	ld (MaxCol),a
	
	; Load the keyboard layout
	ld hl,KeyboardLayouts.UK
	call Keyboard.LoadManualLayout
	
	jp Basic.BBCBASIC_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FontTileIndex = 0
FontCharOffset = FontTileIndex-' '

.var ubyte CurRow, CurCol
.var ubyte MinRow, MaxRow, MinCol, MaxCol

PutMap:
	push bc
	push af
	ld a,(CurRow)
	ld l,a
	ld h,0
	ld b,6
-:	add hl,hl
	djnz -
	ld a,(CurCol)
	add a,a
	ld e,a
	ld d,0
	add hl,de
	ld de,$3800
	add hl,de
	call Video.GotoHL
	pop af
	add a,FontCharOffset
	out (Ports.Video.Data),a
	xor a
	out (Ports.Video.Data),a
	pop bc
	ret
	
PutChar:
	cp '\r'
	jr nz,+
	ld a,(MinCol)
	ld (CurCol),a
	jr NewLine

+:	cp '\n'
	jr nz,+
	
	ld a,(MinCol)
	ld (CurCol),a
	jr NewLine

+:	call PutMap
	ld a,(CurCol)
	inc a
	push bc
	ld bc,(MaxCol)
	cp c
	pop bc
	jr nz,+
	ld a,(MinCol)
+:	ld (CurCol),a
	ret nz

NewLine:
	ld a,(CurRow)
	inc a
	push bc
	ld bc,(MaxRow)
	cp c
	pop bc
	jr nz,+
	ld a,(MinRow)
+:	ld (CurRow),a
	ret

PutString:
	ld a,(hl)
	inc hl
	or a
	ret z
	push hl
	call PutChar
	pop hl
	jr PutString

HelloWorld:
	;   "12345678901234567890123456789012"
	.db "\n\n\n"
	.db "   Hello, world!\n"
	.db "   =============\n\n"
	
	.db "   This is running on a\n"
	.db "   Sega Master System.\n"
	.db "\n"
	.db "                      Hooray!"
	.db "\n"
	.db 0

PutHexNybble:
	cp 10
	jr c,+
	add a,'A'-10
	jp PutChar
+:	add a,'0'
	jp PutChar

PutHexByte:
	push af
	srl a
	srl a
	srl a
	srl a
	call PutHexNybble
	pop af
	and %1111
	jr PutHexNybble

PutHexWord:
	push hl
	ld a,h
	call PutHexByte
	pop hl
	ld a,l
	jr PutHexByte


Font:
.include "bbc"
Font.End:

LoadPalette:
	xor a
	call Video.GotoPalette
	ld hl,Palette
	ld b,32
-:	ld a,(hl)
	inc hl
	out (Ports.Video.Data),a
	djnz -
	ret

Palette:
.db %010000, %000000, %000000, %000000, %000000, %000000, %000000, %000000 ; Tiles   0..7
.db %000000, %000000, %000000, %000000, %000000, %000000, %000000, %111111 ; Tiles   8..F
.db %000000, %011101, %000011, %110000, %000000, %000000, %000000, %000000 ; Sprites 0..7
.db %000000, %000000, %000000, %000000, %000000, %000000, %000000, %010101 ; Sprites 8..F

; *TIJUMP,MAIN/P:4100,EXEC,EVAL,FPP,RAM/P:C000
; *BBCBASIC/N/Y/E

.if $>$4000
.echoln "Too much code :("
.endif

.org $4000
.include "BBC BASIC.asm"