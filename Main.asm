; Script
.incscript "Scripts.cs"

.emptyfill $FF

; BBC BASIC's scratch memory will be in RAM ($DC00..$DEFF)
EndOfVariables = $DC00
Variables      = EndOfVariables - 256

HIMEM = Variables
PAGE = $C000

.function allocVar(size)
	allocVar = Variables
	Variables += size
.endfunction

IOControl = allocVar(1)

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

FrameCounter = allocVar(1)

Interrupt:
	push af
	
	in a,($BF)
	bit 7,a
	call nz,FrameInterrupt
	
	pop af
	reti

FrameInterrupt:

	; Handle the 100Hz TIME counter
	
	ld a,(FrameCounter)
	sub 100 ; 100Hz timer
	
-:	push af
	
	; Update TIME.
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

	; Update the sound.
	call Sound.Tick
	
	pop af
	add a,60 ; 60Hz video refresh
	jp m,-
	ld (FrameCounter),a
	ret

; Libraries:
.include "Video.asm"
.include "Host.asm"
.include "AT.asm"
.include "Keyboard.asm"
.include "UK.inc"
.include "VDU.asm"
.include "Serial.asm"
.include "PCLink2.asm"
.include "Sound.asm"
.include "Maths.asm"

Boot:
	; Make sure SP points somewhere sensible.
	ld sp,HIMEM
	
	; Set paging to something sensible.
	xor a
	ld ($FFFC),a
	ld ($FFFD),a
	inc a
	ld ($FFFE),a
	inc a
	ld ($FFFF),a
	
	; Set up the IOControl mirror
	ld a,%11111111
	ld (IOControl),a
	out ($3F),a

Main:

	; Reset the VDP to sensible defaults.
	call Video.Reset
	
	; Clear VRAM and palette.
	call Video.ClearAll
	
	; Reset the VDU.
	call VDU.Reset
	
	; Load the keyboard layout.
	ld hl,KeyboardLayouts.UK
	call Keyboard.LoadManualLayout
	
	; Serial port initialisation.
	call Serial.Reset
	
	; Sound initialisation.
	call Sound.Reset

	; Jump into BASIC.
	jp Basic.BBCBASIC_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; *TIJUMP,MAIN/P:4100,EXEC,EVAL,FPP,RAM/P:DC00
; *BBCBASIC/N/Y/E

.if Variables > EndOfVariables
	.fail strformat("Too many variables! Please free up {0} bytes.", Variables - EndOfVariables)
.else
	.echoln strformat("{0} bytes free for variables.", EndOfVariables - Variables)
.endif

.if $>$4000
	.fail "Too much code."
.else
	.echoln strformat("{0} bytes free for code page page 0.", $4000 - $)
.endif

.org $4000
.include "BBC BASIC.asm"

.echoln strformat("{0} bytes free for code page page 1.", $8000 - $)