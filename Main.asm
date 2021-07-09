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
	retn

.var ubyte FrameCounter

Interrupt:
	push af
	
	in a,($BF)
	bit 7,a
	jr z,NotFrameInterrupt

FrameInterrupt:
	
	ld a,(VDU.QueueSize)
	or a
	jr z,VDUQueueEmpty

	push bc
	push de
	push hl
	
	call Video.DisplayOff
	
-:	call VDU.Dequeue
	call VDU.PutChar
	
	ld a,(VDU.QueueSize)
	or a
	jr nz,-
	
	call Video.DisplayOn
	
	in a,($BF)
	
	pop hl
	pop de
	pop bc

VDUQueueEmpty:

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

; *TIJUMP,MAIN/P:4100,EXEC,EVAL,FPP,RAM/P:C000
; *BBCBASIC/N/Y/E

.if $>$4000
.echoln "Too much code :("
.endif

.org $4000
.include "BBC BASIC.asm"