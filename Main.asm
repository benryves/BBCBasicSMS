; Script
.incscript "Scripts.cs"

.emptyfill $FF

; BBC BASIC's scratch memory will be in RAM ($DC00..$DEFF)
EndOfVariables = $DC00
Variables      = EndOfVariables - 256

HIMEM = Variables

.function allocVar(size)
	allocVar = Variables
	Variables += size
.endfunction

PAGE = allocVar(2)
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
	
	; Assume we have a stock 8KB machine, so place PAGE at $C000.
	ld hl,$C000
	ld (PAGE),hl
	
	; Additional memory is in the range $8000..$BFFF
	; Enable the cartridge RAM.
	ld a,($FFFC)
	or %00001000 ; RAM enable ($8000..$BFFF)
	ld ($FFFC),a
	
	; Check if the cartridge RAM is writable.
	ld hl,$8000
	ld b,0
-:	ld a,(hl)
	push hl
	inc hl
	dec hl
	pop hl
	inc (hl)
	push hl
	inc hl
	dec hl
	pop hl
	cp (hl)
	jp z,NoCartridgeRAM
	djnz -
	
	; How much cartridge RAM do we have?
	ld b,5
	ld de,1024

	ld ix,$8000
-:	push ix
	pop iy
	add iy,de
	
	; Check the mirror address - changing (ix) should not change (iy).
	ld a,(iy)
	inc (ix)
	sub (iy)
	dec (ix)
	or a
	jr nz,+
	
	sla d
	djnz -
	srl d
+:	
	; DE = amount of cartridge RAM.
	; Decrease PAGE backwards to include it.
	ld hl,(PAGE)
	or a
	sbc hl,de
	ld (PAGE),hl
	jr FoundCartridgeRAM

NoCartridgeRAM:
	ld a,($FFFC)
	and %11110111 ; RAM disable ($8000..$BFFF)
	ld ($FFFC),a

FoundCartridgeRAM:
	
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
	
	; Sign on message.
	ld hl,SignOnMessage
	call VDU.PutString

	; How much RAM do we have?
	ld hl,$E000 ; Top of memory
	ld de,(PAGE)
	or a
	sbc hl,de
	srl h
	srl h
	ld b,h
	xor a
-:	inc a
	djnz -
	
	call VDU.PutDecimalByte
	ld a,'K'
	call VDU.PutChar
	ld a,'\r'
	call VDU.PutChar

	; Jump into BASIC.
	jp Basic.BBCBASIC_START

SignOnMessage:
.db "SEGA Master System ",0

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