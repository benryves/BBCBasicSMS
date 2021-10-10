.incscript "Scripts.cs"

.sdsctag Program.Version, Program.Name, Program.Notes, Program.Author

.defpage 0, kb(16), $0000
.defpage 1, kb(16), $4000
.defpage 2, kb(16), $8000
.defpage 3, kb(16), $8000

.page 0

; Resources
.include "Tokens.inc"

.emptyfill $FF

; BBC BASIC's scratch memory will be in RAM ($DC00..$DEFF)
EndOfVariables = $DC00
Variables      = EndOfVariables - 272

HIMEM = Variables

.function allocVar(size)
	allocVar = Variables
	Variables += size
.endfunction

PAGE = allocVar(2)
IOControl = allocVar(1)

TrapKeyboardCounter = allocVar(1)

TempPtr      = allocVar(2)
TempCapacity = allocVar(2)
TempSize     = allocVar(2)

.org $00
	di
	im 1
	jp Boot

BankedCallPreserveHL = allocVar(2)
BankedCallPreserveDE = allocVar(2)

.org $08
	ld (BankedCallPreserveHL),hl
+:	ld (BankedCallPreserveDE),de
	
	pop hl ; Return address.
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	
	push hl ; Return past vector.
	ld hl,+
	push hl ; We need to return to our RAM-reenabler.
	
	; Disable RAM.
	push af
	ld a,($FFFC)
	and ~(1 << 3)
	ld ($FFFC),a
	pop af
	
	; call (de)
	push de
	ld hl,(BankedCallPreserveHL)
	ld de,(BankedCallPreserveDE)
	ret

+:	; Enable RAM.
	push af
	ld a,($FFFC)
	or 1 << 3
	ld ($FFFC),a
	pop af
	
	; Return properly.
	ret
	

.org $38
	jp Interrupt

.org $66
	call Host.PressEscapeKey
	retn

FrameCounter = allocVar(1)

Interrupt:
	push af
	
	; Check the reset button.
	in a,($DD)
	bit 4,a
	call z,Host.PressBreakKey
	
	in a,($BF)

FrameInterrupt:
	
	; Handle the 100Hz TIME counter
	
	ld a,(FrameCounter)
	sub 100 ; 100Hz timer
	jr nc,FinishedFrameInterrupt
	
-:	push hl
	push af
	
	; Decrement the trap keyboard counter.
	ld a,(TrapKeyboardCounter)
	dec a
	jp m,+
	ld (TrapKeyboardCounter),a
+:
	
	; Update TIME.
	ld hl,(Host.TIME)
	inc hl
	ld (Host.TIME),hl
	ld a,h
	or l
	jr nz,+
	ld hl,(Host.TIME+2)
	inc hl
	ld (Host.TIME+2),hl
+:	

	; Indicate that the host can try to read the keyboard if it wants.
	ld a,(Sound.Status)
	bit Sound.Status.Active,a
	jr z,SoundIsInactive
	ld a,(Sound.ChannelUpdateTimer)
	cp Sound.ChannelUpdatePeriod
	jr nz,SoundIsBusy
SoundIsInactive:
	ld a,(Host.Flags)
	set Host.GetKeyPending,a
	ld (Host.Flags),a
SoundIsBusy:
CheckingKeyboardInInterrupt:

	; Update the sound.
	call Sound.Tick
	
	ld a,(VDU.FieldRate)
	ld l,a
	
	pop af
	add a,l
	pop hl
	jr nc,-

FinishedFrameInterrupt:
	ld (FrameCounter),a
	
	pop af
	ei
	reti

; Libraries:
.include "Video.asm"
.include "AT.asm"
.include "Keyboard.asm"
.include "KeyboardBuffer.asm"
.include "UK.inc"
.include "Serial.asm"
.include "PCLink2.asm"
.include "Maths.asm"
.include "CLI.asm"
.include "Tape.asm"
.include "Sound.asm"
.include "VDrive.asm"
.include "File.asm"

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
	ld de,1024 ; Start with 1KB.
	ld b,5 ; Check for 1KB, 2KB, 4KB, 8KB, 16KB.

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
	; Decrease PAGE backwards to include it in BASIC's memory map.
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
	
	; Set the TRAP keyboard counter to a sensible default.
	xor a
	ld (TrapKeyboardCounter),a

Main:

	; Reset the VDP to sensible defaults.
	call Video.Reset
	
	; Clear VRAM and palette.
	call Video.ClearAll
	
	; Reset the VDU.
	.bcall "VDU.Reset"
	
	; Load the keyboard layout.
	ld hl,KeyboardLayouts.UK
	call Keyboard.LoadManualLayout
	
	; Serial port initialisation.
	call Serial.Reset
	
	; Sound initialisation.
	call Sound.Reset
	
	; File system reset.
	call File.Reset
	
	; Sign on message.
	ld hl,SignOnMessage
	.bcall "VDU.PutString"

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
	
	.bcall "VDU.PutDecimalByte"
	ld a,'K'
	.bcall "VDU.PutChar"
	ld a,'\r'
	.bcall "VDU.PutChar"
	
	; Jump into BASIC.
	jp Basic.BBCBASIC_START

SignOnMessage:
.db "SEGA Master System ",0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; *TIJUMP,MAIN/P:4100,EXEC,EVAL,FPP,RAM/P:DC00
; *BBCBASIC/N/Y/E

.if $>$4000
	.fail "Too much code."
.else
	.echoln strformat("{0} bytes free for code page page 0.", $4000 - $)
.endif

.page 1

.include "BBC BASIC.asm"
.include "Host.asm"
PCLink2.Trap = Host.TrapFileTransfers

.echoln strformat("{0} bytes free for code on page 1.", $8000 - $)

.page 2

.include "VDU.asm"

.echoln strformat("{0} bytes free for code on page 2.", $C000 - $)

.page 3



.if Variables > EndOfVariables
	.fail strformat("Too many variables! Please free up {0} bytes.", Variables - EndOfVariables)
.else
	.echoln strformat("{0} bytes free for variables.", EndOfVariables - Variables)
.endif
