; Script
.incscript "Scripts.cs"

; BBC BASIC's scratch memory will be at RAM ($C000..$C2FF)
.varloc $C000+768, 256

.org $00
	di
	im 1
	jp Boot

.org $38
	jp Interrupt

.org $66
	push af
	ld a,(Host.Flags)
	set Host.Pause,a
	ld (Host.Flags),a
	pop af
	retn

.var ubyte FrameCounter

Interrupt:
	push af
	
	in a,($BF)
	bit 7,a
	jr z,NotFrameInterrupt

FrameInterrupt:

	; Handle the 100Hz TIME counter
	
	ld a,(FrameCounter)
	sub 100 ; 100Hz timer
	
-:	push af
	push hl
	ld hl,(Host.TIME)
	inc hl
	ld (Host.TIME),hl
	ld a,h
	or l
	jr nz,+
	ld hl,(Host.TIME+2)
	inc hl
	ld (Host.TIME+2),hl
+:	pop hl
	
	; Will we need to read the keyboard to trap Escape?
	ld a,(Host.TrapKeyboardTimer)
	or a
	jr z,+
	dec a
	ld (Host.TrapKeyboardTimer),a
+:
	
	pop af
	add a,60 ; 60Hz video refresh
	jp m,-
	ld (FrameCounter),a
	
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
	ld sp,$DFF0
	
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
	
	; Reset the VDU.
	call VDU.Reset
	
	; Load the keyboard layout
	ld hl,KeyboardLayouts.UK
	call Keyboard.LoadManualLayout
	
	jp Basic.BBCBASIC_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PutHexNybble:
	cp 10
	jr c,+
	add a,'A'-10
	jp VDU.PutChar
+:	add a,'0'
	jp VDU.PutChar

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

; *TIJUMP,MAIN/P:4100,EXEC,EVAL,FPP,RAM/P:C000
; *BBCBASIC/N/Y/E

.if $>$4000
.echoln "Too much code :("
.else
.echoln $4000-$
.endif

.org $4000
.include "BBC BASIC.asm"