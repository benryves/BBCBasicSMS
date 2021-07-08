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
	jp Interrupt

.org $66
	retn

Interrupt:
	push af
	
	in a,($BF)
	bit 7,a
	jr z,NotFrameInterrupt

FrameInterrupt:
	
	ld a,(VDU.QueueSize)
	or a
	jr z,NotFrameInterrupt

	push bc
	push de
	push hl
	
	ld a,%01100000
	ld a,%00000000
	ld b,1
	call Video.SetReg
	
-:	call VDU.Dequeue
	call VDU.PutChar
	
	ld a,(VDU.QueueSize)
	or a
	jr nz,-
	
	ld a,%01100000
	ld b,1
	call Video.SetReg
	
	in a,($BF)
	
	pop hl
	pop de
	pop bc
	
NotFrameInterrupt:
	pop af
	reti

; Libraries:
.include "Video.asm"
.include "Host.asm"
.include "AT.asm"
.include "Keyboard.asm"
.include "UK.inc"
.include "VDU.asm"

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
	ld hl,VDU.FontTileIndex*8
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
	
	; Get the VDU queue ready
	call VDU.ClearQueue
	
	; Screen on, frame interrupts.
	ld a,%01100000
	ld b,$01
	call Video.SetReg
	
	xor a
	ld (VDU.MinRow),a
	ld (VDU.CurRow),a
	ld a,24
	ld (VDU.MaxRow),a
	
	ld a,2
	ld (VDU.MinCol),a
	ld (VDU.CurCol),a
	ld a,30
	ld (VDU.MaxCol),a
	
	; Load the keyboard layout
	ld hl,KeyboardLayouts.UK
	call Keyboard.LoadManualLayout
	
	jp Basic.BBCBASIC_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PutHexNybble:
	cp 10
	jr c,+
	add a,'A'-10
	jp VDU.Enqueue
+:	add a,'0'
	jp VDU.Enqueue

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