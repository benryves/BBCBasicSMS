;------------------------------------------------------------------------------- 
;@doc:file 
; 
; === Host.asm ===
;
;   Provides the interface between and BBC BASIC and its host environment.
;
;
;@doc:end
;------------------------------------------------------------------------------- 
.module Host

TIME = allocVar(4)

KeyboardRate = allocVar(1)

global.Host.Flags = allocVar(1) ;; HACK for broken name resolution
EscapeError = 0
GetKeyPending = 1
CursorKeysFixed = 2
EscapeKeyDisabled = 3
EscapeErrorDisabled = 4
Loading = 6

OSLINE.Override = allocVar(2)
OSWRCH.Override = allocVar(2)

ExecHandle = allocVar(1)
SpoolHandle = allocVar(1)

; ==========================================================================
; Subroutine environment
; --------------------------------------------------------------------------
; Subroutines are called in a conventional fashion, with the processor's
; stack containing the return address. Control should be returned to BASIC
; with a RET instruction.
; ==========================================================================

; ==========================================================================
; OSINIT
; --------------------------------------------------------------------------
; Initialise operating system and filing system.
; --------------------------------------------------------------------------
; Outputs:    DE: initial value of HIMEM
;             HL: initial value of PAGE
;             F: Z set (Z) = don't boot,
;                Z reset (NZ) = CHAIN file named in string accumulator.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
OSINIT:
	
	; Reset the clock.
	ld hl,0
	ld (TIME+0),hl
	ld (TIME+2),hl
	
	; Clear the exec/spool handles.
	xor a
	ld (ExecHandle),a
	ld (SpoolHandle),a
	
	; Clear the flags.
	xor a
	ld (Flags),a
	
	ld a,%0101011
	ld (KeyboardRate),a
	
	call RESET
	
	call Sound.PlayBell
	
	ld de,HIMEM  ; HIMEM
	ld hl,(PAGE) ; PAGE
	
	scf ; don't boot
	ret

; ==========================================================================
; OSRDCH
; --------------------------------------------------------------------------
; Wait for a keypress at the console keyboard.
; --------------------------------------------------------------------------
; Outputs:    A: ASCII code of key pressed.
; Destroyed:  AF.
; ==========================================================================
OSRDCH:
	.bcall "VDU.BeginBlinkingCursor"
	
-:	call KeyboardBuffer.GetKeyImmediate
	
	jr z,+              ; No key.
	jp m,+              ; Ignore extended keys.
	
	bit 7,a
	jr z,OSRDCH.GotKey
	
	ld e,a
	ld a,(VDU.Console.ConsoleFlags)
	bit VDU.Console.CursorEditingDisabled,a
	ld a,e
	jr nz,OSRDCH.GotKey

+:	push af
	.bcall "VDU.DrawBlinkingCursor"
	pop af
	jr -

OSRDCH.GotKey:
	push af
	.bcall "VDU.EndBlinkingCursor"	
	pop af
	ret

; ==========================================================================
; OSKEY
; --------------------------------------------------------------------------
; Wait a specified maximum time for a keypress at the console.
; --------------------------------------------------------------------------
; Inputs:     HL: Time limit (centiseconds).
; Outputs:    F: If time-out occurred, reset carry flag (NC).
;             A: ASCII code of key pressed.
; Destroyed:  AF.
; ==========================================================================
OSKEY:
	push bc
	push de
		
	bit 7,h
	jr z,OSKEY.NotNegative
	
	inc h
	jr nz,OSKEY.Return0
	
	ld a,l
	or a
	ld a,'P'
	jr z,OSKEY.ReturnValue
	
	call KeyboardBuffer.GetDeviceKey
	ld a,l
	neg
	
	cp 4 ; 1 = Shift, 2 = Ctrl, 3 = Alt
	jr nc,OSKEY.CheckKey.NotModifier
	
	add a,3 ; Left: 4 = Shift, 5 = Ctrl, 6 = Alt.
	call OSKEY.CheckKey
	jr z,OSKEY.ReturnTrue
	
	add a,3 ; Right: 7 = Shift, 8 = Ctrl, 9 = Alt.

OSKEY.CheckKey.NotModifier:
	call OSKEY.CheckKey
	jr nz,OSKEY.ReturnFalse

OSKEY.ReturnTrue:
	xor a
	scf
	ccf
	pop de
	pop bc
	ret
OSKEY.ReturnFalse:
OSKEY.Return0:
	xor a
OSKEY.ReturnValue:
	scf
	pop de
	pop bc
	ret

OSKEY.CheckKey:
	ld hl,KeyboardBuffer.HeldKeys
	ld bc,KeyboardBuffer.HeldKeyCount
	cpir
	ret

OSKEY.NotNegative:
	
	; Calculate the end time-1 (easier to check for <0 instead of <=0 later)
	ld de,(TIME)
	dec de
	add hl,de

	ei
OSKEY.Loop:
	call KeyboardBuffer.GetKeyImmediate
	
	jr z,OSKEY.NoKey
	jp m,OSKey.NoKey
	
	bit 7,a
	jr z,OSKEY.GotKey
	
	ld e,a
	ld a,(VDU.Console.ConsoleFlags)
	bit VDU.Console.CursorEditingDisabled,a
	ld a,e
	jr nz,OSKEY.GotKey
	

OSKEY.NoKey:
	; Has the timer expired?
+:	push hl
	ld de,(TIME)
	or a
	sbc hl,de
	ex de,hl
	pop hl
	
	; Check if timer has gone negative.
	bit 7,d
	jr nz,OSKEY.TimedOut
	
	; No, so draw the flashing cursor.
	.bcall "VDU.DrawBlinkingCursor"
	halt
	jr OSKEY.Loop

OSKEY.TimedOut:
	scf ; Timed out, so pretend we dont have a key.
	
OSKEY.GotKey:
	ccf
	
	push af
	.bcall "VDU.EndBlinkingCursor"
	pop af
	
	pop de
	pop bc
	ei
	ret

; ==========================================================================
; OSLINE
; --------------------------------------------------------------------------
; Read a line, terminated by RETURN, from the console.
; --------------------------------------------------------------------------
; Inputs:     HL: Addresses destination RAM for entered line.
;                 A maximum of 256 bytes are available for the line,
;                 including the CR terminator. L will always be zero
;                 (i.e. the buffer lies on a page boundary).
; Outputs:    A: Must be zero.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
OSLINE:

	ld bc,(OSLINE.Override)
	ld a,b
	or a
	jr z,+
	push bc
	ret

+:
	.bcall "VDU.BeginBlinkingCursor"
	ld bc,255 ; B = current length (0), C = maximum length (excluding \r terminator).
	ld d,0 ; D = index into current string.

OSLINE.Loop:
	
	; Are we typing the last character of the string?
	; If so, make sure it's CR-terminated.
	ld a,d
	cp b
	jr nz,+
	ld (hl),'\r'
+:
	
-:	call CheckEscape
	.bcall "VDU.DrawBlinkingCursor"
	call KeyboardBuffer.GetKeyImmediate
	jr z,-
	
	jp m,OSLINE.ExtendedKey
	
	; Control keys.
	cp '\b'
	jp z,OSLINE.Backspace
	cp 127
	jp z,OSLINE.Delete
	cp '\r'
	jp z,OSLINE.Return
	cp 21 ; NAK
	jp z,OSLINE.Clear
	
	cp 7
	jr nz,+
	.bcall "VDU.PutChar"
	jr OSLINE.Loop
+:

	; Is it a cursor key, and is cursor editing enabled?
	cp $C0
	jr c,OSLINE.NotCursorKey
	
	ld e,a
	ld a,(VDU.Console.ConsoleFlags)
	bit VDU.Console.CursorEditingDisabled,a
	ld a,e
	jr nz,OSLINE.NotCursorKey
	
	; Ignore modifiers.
	and $CF
	
	cp $C6
	call z,OSLINE.Insert
	
	cp $C7
	jp z,OSLINE.Delete
	
	cp $C8
	call z,OSLINE.Home
	
	cp $C9
	call z,OSLINE.End
	
	cp $CC
	call z,OSLINE.Left
	
	cp $CD
	call z,OSLINE.Right
	
	cp $CE
	call z,OSLINE.Down
	
	cp $CF
	call z,OSLINE.Up
	
	jr OSLINE.Loop

OSLINE.NotCursorKey:
	; Check if it's in the range 32..127.
	cp 32
	jr c,OSLINE.Loop
	cp 128
	jr nc,OSLINE.Loop
	
	; Store the character in E.
	ld e,a
	
	; Is this going to be an insert or overwrite command?
	ld a,(VDU.Console.ConsoleFlags)
	bit VDU.Console.Overwrite,a
	jr z,OSLINE.InsertCharacter
	
	; If we're at the end of the current string, treat it as an insert, even in overwrite mode.
	ld a,d
	cp b
	jr z,OSLINE.InsertCharacter
	
	; This is an overwrite.
	ld a,e
	ld (hl),a
	inc hl
	inc d
	
	push af
	.bcall "VDU.EndBlinkingCursor"
	pop af
	
	; Print the character.
	.bcall "VDU.PutChar"
	jp OSLINE.Loop

OSLINE.InsertCharacter
	; Check if we've got enough space.
	ld a,c
	or a
	jp z,OSLINE.Loop

OSLINE.EnoughSpace:	
	
	push af
	.bcall "VDU.EndBlinkingCursor"
	pop af
	
	; Move all characters after the current one right.
	push bc
	push de
	push hl
	
	; Where's the end of the string?
	; It's at HL+B-D
	push bc
	ld c,b
	ld b,0
	add hl,bc
	ld c,d
	or a
	sbc hl,bc
	pop bc
	
	; Quantity to copy is B-D+1
	ld a,b
	sub d
	inc a
	ld c,a
	ld b,0
	
	; Set DE to point to the last character in the string.
	ld e,l
	ld d,h
	
	; Advance DE to point at the character just after the end of the string (\r terminator).
	inc de
	
	lddr
	
	pop hl
	pop de
	pop bc
	
	; Store the character.
	ld a,e
	ld (hl),a
	inc hl
	inc d
	
	; Increment the current length (B), decrement the maximum length (C).
	inc b
	dec c
	
	; Print the character.
	.bcall "VDU.PutChar"
	
	ld e,0 ; Don't draw a blank character on the end of the new line.

OSLINE.RepaintToEnd:
	
	; Are there any characters between the current one and the end of the line?
	ld a,b
	sub d
	jr nz,OSLINE.RepaintToEnd.HasCharacters
	
	ld a,e
	cp '\r'
	jr nz,+
	ld a,127
+:	or a
	jr z,+
	.bcall "VDU.PutMap"
+	jp OSLINE.Loop

OSLINE.RepaintToEnd.HasCharacters:

	; We need to repaint...
	push bc
	push hl
	
	ld bc,(VDU.Console.CurRow)
	push bc
	
	; Redraw the rest of the characters.
	ld b,a

-:	ld a,(hl)
	.bcall "VDU.PutMap"
	inc hl
	
	; Are we about to run off the bottom right of the screen?
	ld a,(VDU.Console.CurCol)
	ld c,a
	ld a,(VDU.Console.MaxCol)
	cp c
	jr nz,+
	
	ld a,(VDU.Console.CurRow)
	ld c,a
	ld a,(VDU.Console.MaxRow)
	cp c
	jr z,OSLINE.RepaintToEnd.HitBottomRight
	
+:	.bcall "VDU.Console.CursorRight"
	djnz -


OSLINE.RepaintToEnd.HitEndOfLine:

	ld a,e
	cp '\r'
	jr nz,+
	ld a,127
+:	or a

	jr z,+
	.bcall "VDU.PutMap"
+:

OSLINE.RepaintToEnd.HitBottomRight:

	; Move the cursor back to where it should be.
	pop bc
	ld (VDU.Console.CurRow),bc
	
	pop hl
	pop bc
	jp OSLINE.Loop

OSLINE.Return:
	
	; Move to the end.
	call OSLINE.End
	
	.bcall "VDU.EndBlinkingCursor"
	
	ld a,'\r'
	.bcall "VDU.PutChar"
	
	xor a
	ret

OSLINE.ExtendedKey:
	
	jp OSLINE.Loop

OSLINE.Insert:
	push af
	ld a,(VDU.Console.ConsoleFlags)
	xor 1<<VDU.Console.Overwrite
	ld (VDU.Console.ConsoleFlags),a
	pop af
	ret

OSLINE.Delete:
	; Same as moving right once, then backspacing.
	
	; Are we at the end?
	ld a,d
	cp b
	jp z,OSLINE.Loop
	
	.bcall "VDU.EndBlinkingCursor"
	.bcall "VDU.CursorRight"
	inc hl
	inc d
	
	; Run-on to Backspace handler.

OSLINE.Backspace:
	; Are we at the start of the string? (Offset from start = 0)?
	ld a,d
	or a
	jp z,OSLINE.Loop
	
	.bcall "VDU.EndBlinkingCursor"
	
	; Move all characters from the current cursor position+1 to the end of the string left one.
	push hl
	push de
	push bc
	
	ld a,b
	sub d
	inc a
	ld c,a
	ld b,0
	
	ld e,l
	ld d,h
	dec de
	ldir
	
	pop bc
	pop de
	pop hl
	
	; Decrease the current length of the string, increase the free space.
	dec b
	inc c
	
	; Move the cursor left.
	.bcall "VDU.CursorLeft"
	
	dec hl
	dec d

	ld e,127 ; Blank off the end of the backspaced line.
	jp OSLINE.RepaintToEnd

OSLINE.Home:
	push af
	; Are we already at the start?
	ld a,d
	or a
	jr z,+

	.bcall "VDU.EndBlinkingCursor"

-:	dec hl
	dec d
	.bcall "VDU.CursorLeft"
	call c,OSLINE.RepaintTopLine
	ld a,d
	or a
	jr nz,-
	
+:	pop af
	ret
	
OSLINE.End:
	push af
	; Are we already at the end?
	ld a,d
	cp b
	jr z,+

	.bcall "VDU.EndBlinkingCursor"

-:	inc hl
	inc d
	
	.bcall "VDU.CursorRight"
	call c,OSLINE.RepaintBottomLine
	
	ld a,b
	cp d
	jr nz,-
	
+:	pop af
	ret

OSLINE.Left:
	push af
	
	; Are we already at the start?
	ld a,d
	or a
	jr z,+
	
	.bcall "VDU.EndBlinkingCursor"
	
	dec hl
	dec d
	
	.bcall "VDU.CursorLeft"
	call c,OSLINE.RepaintTopLine
	
+:	pop af
	ret

OSLINE.Right:
	push af
	
	; Are we already at the end?
	ld a,d
	cp b
	jr z,+
	
	.bcall "VDU.EndBlinkingCursor"
	
	inc hl
	inc d
	
	.bcall "VDU.CursorRight"
	call c,OSLINE.RepaintBottomLine
	
+:	pop af
	ret

OSLINE.Up:
	push af
	
	ld a,(VDU.Console.MinCol)
	ld e,a
	ld a,(VDU.Console.MaxCol)
	sub e
	inc a
	
	; a = number of characters to move left.
	ld e,a
	cp d
	jr c,+
	
	pop af
	jr OSLINE.Home
	
+:	.bcall "VDU.EndBlinkingCursor"
	
	push de
	ld d,0
	or a
	sbc hl,de
	pop af
	sub e
	ld d,a
	
	.bcall "VDU.CursorUp"
	call c,OSLINE.RepaintTopLine
	
	pop af
	ret
	

OSLINE.Down:
	push af
	
	ld a,(VDU.Console.MinCol)
	ld e,a
	ld a,(VDU.Console.MaxCol)
	sub e
	inc a
	
	; a = number of characters to move right.
	ld e,a
	ld a,b
	sub d
	cp e
	jr nc,+
	
	pop af
	jr OSLINE.End
	
+:	.bcall "VDU.EndBlinkingCursor"
	
	push de
	ld d,0
	add hl,de
	pop af
	add a,e
	ld d,a

	.bcall "VDU.CursorDown"
	call c,OSLINE.RepaintBottomLine
	
	pop af
	ret

OSLINE.Clear:
	
	; Is the line already clear?
	ld a,b
	or a
	jp z,OSLINE.Loop
	
	push af
	.bcall "VDU.EndBlinkingCursor"
	pop af
	
	; Move to the end of the line.
	sub d
	jr z,+
	
-:	push af
	.bcall "VDU.CursorRight"
	inc hl
	inc d
	pop af
	dec a
	jr nz,-
+:
	
	; Move back to the start, clearing as we go.
-:	dec hl
	dec d
	inc c
	ld (hl),'\r'
	ld a,127
	.bcall "VDU.PutChar"
	djnz -
	
	jp OSLINE.Loop

OSLINE.RepaintTopLine:
OSLINE.RepaintBottomLine:
OSLINE.RepaintCurrentLine:
	; Preserve editor and cursor state.
	ld a,(VDU.Console.CurCol)
	push af
	push bc
	push de
	push hl
	
	; Work backwards to the start of the line.
	
	; Are we already at the start of the line?
	ld a,d
	or a
	jr z,OSLINE.RepaintAtLeftEdge
	
	; Are we already in the leftmost column?
	ld a,(VDU.Console.MinCol)
	ld e,a
	ld a,(VDU.Console.CurCol)
	sub e
	jr z,OSLINE.RepaintAtLeftEdge
	ld e,a
	; e = distance from left edge.
	
-:	dec hl
	.bcall "VDU.Console.CursorLeft"
	dec d
	jr z,OSLINE.RepaintAtLeftEdge
	dec e
	jr z,OSLINE.RepaintAtLeftEdge
	jr -

OSLINE.RepaintAtLeftEdge:
	
	; We are now at the left most edge.
	
	; Get the distance to the end of the line.
	ld a,b
	sub d
	inc a
	ld d,a
	
	; Get the distance to the right of the viewport.
	ld a,(VDU.Console.CurCol)
	ld e,a
	ld a,(VDU.Console.MaxCol)
	sub e
	inc a
	ld e,a

-:	ld a,(hl)
	.bcall "VDU.PutMap"
	inc hl
	dec d
	jr z,OSLINE.RepaintAtRightEdge
	dec e
	jr z,OSLINE.RepaintAtRightEdge
	.bcall "VDU.COnsole.CursorRight"
	jr -

OSLINE.RepaintAtRightEdge:

	; Restore editor and cursor state.
	pop hl
	pop de
	pop bc
	pop af
	ld (VDU.Console.CurCol),a
	ret

; ==========================================================================
; OSLINE.Prefilled
; --------------------------------------------------------------------------
; Read a line, terminated by RETURN, from the console.
; The input buffer pointed to by HL may be pre-filled.
; --------------------------------------------------------------------------
; Inputs:     HL: Addresses destination RAM for entered line.
;                 A maximum of 256 bytes are available for the line,
;                 including the CR terminator. L will always be zero
;                 (i.e. the buffer lies on a page boundary).
; Outputs:    A: Must be zero.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
OSLINE.Prefilled:
	.bcall "VDU.BeginBlinkingCursor"
	ld bc,255
	ld d,0
-:	ld a,(hl)
	cp '\r'
	jp z,OSLINE.Loop
	.bcall "VDU.PutLiteralChar" ; In case there are any control codes embedded in the line.
	inc d
	inc hl
	inc b
	dec c
	jr -

; ==========================================================================
; OSASCI
; --------------------------------------------------------------------------
; Sends a character to the console output device (screen),
; converting CR to a LF CR pair.
; --------------------------------------------------------------------------
; Inputs:     A: Character to output.
; Destroyed:  Nothing.
; ==========================================================================
OSASCI:
	cp '\r'
	jr nz,OSWRCH
	ld a,'\n'
	call OSWRCH
	ld a,'\r'
	jr OSWRCH

; ==========================================================================
; PROMPT
; --------------------------------------------------------------------------
; Print the normal BASIC prompt character.
; --------------------------------------------------------------------------
; Inputs:     F: Carry reset for "long" prompt, set for "short" prompt.
; Destroyed:  AF, DE, HL.
; ==========================================================================
PROMPT:
	ld a,'>'
	.bcall "VDU.PutChar"
	ret

; ==========================================================================
; OSWRCH
; --------------------------------------------------------------------------
; Sends a character to the console output device (screen).
; --------------------------------------------------------------------------
; Inputs:     A: Character to output.
; Destroyed:  Nothing.
; ==========================================================================
OSWRCH:
	push hl
	push af
	
	ld a,(SpoolHandle)
	or a
	jr z,+
	
	pop af
	push af
	
	push hl
	push de
	push bc
	
	ld de,(SpoolHandle)
	call File.WriteByte
	
	pop bc
	pop de
	pop hl
	
	
+:
	
	ld hl,(OSWRCH.Override)
	ld a,h
	or l
	jr z,OSWRCH.NoOverride

	pop af
	
	push bc
	ld bc,OSWRCH.OverrideReturn
	
	push bc
	jp (hl)

OSWRCH.OverrideReturn:	
	pop bc
	pop hl
	ei
	ret

OSWRCH.NoOverride:
	pop af
	pop hl
	
	.bcall "VDU.WriteByte"
	ei
	ret

; ==========================================================================
; LTRAP
; --------------------------------------------------------------------------
; Test for an operator abort when LISTing (ESCape).
; --------------------------------------------------------------------------
; Destroyed:  AF.
; ==========================================================================
LTRAP:
	; Force a keyboard update.
	xor a
	ld (TrapKeyboardCounter),a
	; Fall-through to TRAP.
	
; ==========================================================================
; TRAP
; --------------------------------------------------------------------------
; Test for an operator abort when running a program (ESCape).
; --------------------------------------------------------------------------
; Destroyed:  AF, HL.
; ==========================================================================
TRAP:
	ei
	
	call CheckEscape
	
	ld a,(TrapKeyboardCounter)
	or a
	ret nz
	
	ld a,10
	ld (TrapKeyboardCounter),a
	
-:	call KeyboardBuffer.GetDeviceKey
	ret nz ; No key
	jr -

PressEscapeKey:
	push af
	ld a,(Flags)
	and (1<<EscapeKeyDisabled)|(1<<EscapeErrorDisabled)
	jr nz,+
-:	ld a,(Flags)
	set EscapeError,a
	ld (Flags),a
+	pop af
	ret

PressBreakKey:
	rst 0

SetEscape:
	push af
	jr -

AcknowledgeEscape:
	ld l,255
ClearEscape:
	push af
	ld a,(Flags)
	res EscapeError,a
	ld (Flags),a
	pop af
	ret

CheckEscape:
	; Is the Escape condition flag set?
	ld a,(Flags)
	bit EscapeError,a
	ret z
	
TRAP.Escape:
	
	ld a,(Flags)
	res EscapeError,a
	ld (Flags),a
	
	.bcall "VDU.Console.EndBlinkingCursor"
	
	ld a,17 ; Escape
	call Basic.BBCBASIC_EXTERR
	.db "Escape", 0

TrapFileTransfers:
	ei
	
	.bcall "VDU.DrawBlinkingCursor"
	
	; Is Escape pressed?
-:	ld a,(Flags)
	bit EscapeError,a
	jr nz,TrapFileTransfers.Trap
	
	; Shall we poll the keyboard?
	call KeyboardBuffer.GetDeviceKey
	jr nz,TrapFileTransfers.CarryOn ; No key
	jr -

TrapFileTransfers.CarryOn:
	scf ; Carry on.
	ret

TrapFileTransfers.Trap:
	or a ; Don't carry on.
	ret

; ==========================================================================
; RESET
; --------------------------------------------------------------------------
; Reset system prior to outputting an error message.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; ==========================================================================
RESET:

	push hl
	push de
	push bc
	
	ld a,(SpoolHandle)
	or a
	jr z,+
	
	ld b,a
	xor a
	ld (SpoolHandle),a
	ld a,b
	call File.Close
	
+:
	
	; No custom OSLINE/OSWRCH handlers.
	ld hl,0
	ld (OSLINE.Override),hl
	ld (OSWRCH.Override),hl
	

	; Clear the "Escape" state.
	ld a,(Flags)
	and ~((1<<EscapeError) | (1<<EscapeKeyDisabled) | (1<<EscapeErrorDisabled))
	ld (Flags),a	

	call KeyboardBuffer.Reset
	
	; Shh!
	call Sound.Silence
	
	; Make sure the video registers match our local copies.
	call Video.SynchroniseRegisters
	
	; Flush the VDU queue.
	xor a
	ld (VDU.CommandQueue.Waiting),a
	
	; Switch the tape motor off.
	call Tape.MotorOff
	
	pop bc
	pop de
	pop hl
	ret

; ==========================================================================
; OSLOAD
; --------------------------------------------------------------------------
; Load a program file into RAM.
; --------------------------------------------------------------------------
; Inputs:     HL: addresses a file descriptor (filename) terminated by CR.
;             DE: the address in RAM at which the file should be loaded.
;             BC: the maximum file size for which RAM is available (bytes).
; Ouptuts:    F: If the carry flag is RESET (NC) this indicates that there
;                was insufficient space to load the entire file, BASIC
;                issues the "No room" error.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
OSLOAD:
	push de
	
	
	ld a,(File.FileSystem)
	cp File.FileSystems.Tape1200
	jr z,OSLOAD.Tape
	cp File.FileSystems.Tape300
	jr z,OSLOAD.Tape
	cp File.FileSystems.PCLink2
	jr z,OSLOAD.PCLink
	
	jp DeviceFault
	
OSLOAD.PCLink:
	.bcall "VDU.BeginBlinkingCursor"
	call PCLink2.GetFile
	push af
	.bcall "VDU.EndBlinkingCursor"
	pop af
	jr +

OSLOAD.Tape:
	call Tape.GetFile

+:	pop hl
	jr nz,OSLOAD.Error
	jp c,TRAP.Escape
	
	call RepairProgram
	
	scf
	ret
	
OSLOAD.Error:
	; What sort of error?
	jp nc,DeviceFault
	ccf
	ret

RepairProgram:
	ld c,l
	ld b,h
	
-:	ld a,(hl) ; Start of line
	cp '\r'
	ret nz
	
	inc hl
	ld a,(hl)
	cp -1
	jr z,CanConvert
	
	inc hl
	inc hl
	ld e,(hl)
	ld d,0
	add hl,de
	dec hl
	dec hl
	dec hl
	jr -

CanConvert:

	; If we get this far, there's a valid Acorn BBC BASIC program in memory.
	ld l,c
	ld h,b
	
-:	ld a,(hl)
	cp '\r'
	ret nz
	inc hl    ; Advance past CR
	ld d,(hl) ; High part of line number.
	ld a,d
	cp -1
	jr z,WriteEOF
	inc hl
	ld e,(hl) ; Low part of line number.
	inc hl
	ld b,(hl) ; Line length
		
	push hl
	dec hl
	ld (hl),d
	dec hl
	ld (hl),e
	dec hl
	ld (hl),b
	pop hl
	
	ld d,h
	ld e,l
	inc hl
	
	ld a,b
	sub 3
	ret c
	ld c,a
	ld b,0
	ldir
	ex de,hl
	jr -

WriteEOF:

	dec hl
	ld (hl),0
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	xor a
	ret

; ==========================================================================
; OSSAVE
; --------------------------------------------------------------------------
; Save an area of RAM to a program file.
; --------------------------------------------------------------------------
; Inputs:     HL: addresses a file descriptor (filename) terminated by CR.
;             DE: the address in RAM at which the data to save starts.
;             BC: the length of the data to save (bytes).
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
OSSAVE:

	ld a,(File.FileSystem)
	cp File.FileSystems.Tape1200
	jr z,OSSAVE.Tape
	cp File.FileSystems.Tape300
	jr z,OSSAVE.Tape
	cp File.FileSystems.PCLink2
	jr z,OSSAVE.PCLink2
	
	jp DeviceFault

OSSAVE.PCLink2:
	.bcall "VDU.BeginBlinkingCursor"
	call PCLink2.SendFile
	push af
	.bcall "VDU.EndBlinkingCursor"
	pop af
	jp nz,DeviceFault
	jp c,TRAP.Escape
	ret

OSSAVE.Tape:
	call Tape.WriteFile
	ret

; ==========================================================================
; OSOPEN
; --------------------------------------------------------------------------
; Open a file for reading or writing.
; --------------------------------------------------------------------------
; Inputs:     HL: addresses a file descriptor (filename) terminated by CR.
;             AF: -1, NZ, NC = OPENOUT
;                  0,  Z,  C = OPENIN
;                  1, NZ,  C = OPENUP
; Outputs:    A: the file "handle" (channel number) allocated. This may be 
;                any value from 1 to 255. If a value 0 is returned, the file
;                could not be opened. Normal practice is to return with A=0
;                if the file  specified in OPENIN or OPENUP was not found
;                but to abort with an appropriate message (e.g.
;                "Directory full") in the event of other errors.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
OSOPEN:
	jp File.Open

; ==========================================================================
; OSSHUT
; --------------------------------------------------------------------------
; Close a file previously opened with OSOPEN.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number) to close.
;                If E is 0 then all open files (if any) are closed.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
OSSHUT:
	jp File.Close

; ==========================================================================
; OSBGET
; --------------------------------------------------------------------------
; Read a single byte from an open file.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number).
; Outputs:    A: The byte read from the file.
; Destroyed:  AF, BC.
; ==========================================================================
OSBGET:
	jp File.GetByte

; ==========================================================================
; OSBPUT
; --------------------------------------------------------------------------
; Write a single byte to an open file.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number).
;             A: The value to write to the file.
; Destroyed:  AF, BC.
; ==========================================================================
OSBPUT:
	jp File.WriteByte

; ==========================================================================
; OSSTAT
; --------------------------------------------------------------------------
; Read the status of an open file.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number).
; Outputs:    F: If at the end-of-file, return Z.
;                If not at end-of file, return NZ.
; Destroyed:  AF, DE, HL.
; ==========================================================================
OSSTAT:
	jp File.IsEOF

; ==========================================================================
; GETPTR
; --------------------------------------------------------------------------
; Read the sequential pointer of an open file.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number).
; Outputs:    DEHL: the 32-bit pointer.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
GETPTR:
	jp File.GetPointer

; ==========================================================================
; PUTPTR
; --------------------------------------------------------------------------
; Update the sequential pointer of an open file.
; --------------------------------------------------------------------------
; Inputs:     A: The file handle (channel number).
;             DEHL: The new file pointer.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
PUTPTR:
	jp File.SetPointer

; ==========================================================================
; GETEXT
; --------------------------------------------------------------------------
; Return the length of an open file.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number).
; Outputs:    DEHL: File size (bytes).
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
GETEXT:
	jp SORRY

; ==========================================================================
; OSCLI
; --------------------------------------------------------------------------
; Execute a "star" command.
; --------------------------------------------------------------------------
; Inputs:     HL: addresses the "star" command in RAM, terminated by CR.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
OSCLI = CLI.Execute

; ==========================================================================
; PUTCSR
; --------------------------------------------------------------------------
; Move the text cursor to a given location.
; --------------------------------------------------------------------------
; Inputs:     DE: Horizontal position of cursor (0 is the left-hand column).
;             HL: Vertical position of cursor (0 is the top row).
; Destroyed:  AF, DE, HL.
; ==========================================================================
PUTCSR:
	; Quick 16-bit bounds check
	ld a,d
	or a
	ret nz
	ld a,h
	or a
	ret nz
	
	; VDU 31,X,Y
	
	ld d,l
	ex de,hl
	push hl
	
	ld a,31
	.bcall "VDU.WriteByte"
	
	pop hl
	.bcall "VDU.WriteWord"
	ret

; ==========================================================================
; GETCSR
; --------------------------------------------------------------------------
; Return the current text cursor coordinates.
; --------------------------------------------------------------------------
; Outputs:    DE: Horizontal position of cursor (0 is the left-hand column).
;             HL: Vertical position of cursor (0 is the top row).
; Destroyed:  AF, DE, HL.
; ==========================================================================
GETCSR:
	ld a,(VDU.Console.MinCol)
	ld e,a
	ld a,(VDU.Console.CurCol)
	sub e
	ld e,a
	ld d,0
		
	ld a,(VDU.Console.MinRow)
	ld l,a
	ld a,(VDU.Console.CurRow)
	sub l
	ld l,a
	ld h,0
	
	ret

; ==========================================================================
; PUTIME
; --------------------------------------------------------------------------
; Update the elapsed time clock.
; --------------------------------------------------------------------------
; Inputs:     DEHL: Time to load (centiseconds)
; Destroyed:  AF, DE, HL.
; ==========================================================================
PUTIME:
	di
	ld (TIME+0),hl
	ld (TIME+2),de
	ei
	ret

; ==========================================================================
; GETIME
; --------------------------------------------------------------------------
; Read the elapsed time clock.
; --------------------------------------------------------------------------
; Outputs:    DEHL: Current value of elapsed time (centiseconds)
; Destroyed:  AF, DE, HL.
; ==========================================================================
GETIME:
	di
	ld hl,(TIME+0)
	ld de,(TIME+2)
	ei
	ret

; ==========================================================================
; PUTIMS
; --------------------------------------------------------------------------
; Update the real-time clock.
; --------------------------------------------------------------------------
; Inputs:     Time string stored in string accumulator.
;             DE: addresses byte following last byte of string
;             (i.e. E = string length)
; Destroyed:  AF, DE, HL.
; ==========================================================================
PUTIMS
	jp SORRY

; ==========================================================================
; GETIMS
; --------------------------------------------------------------------------
; Read the real-time clock.
; --------------------------------------------------------------------------
; Outputs:    Time string stored in string accumulator
;             DE addresses byte following last byte of string
;             (i.e. E = string length)
; Destroyed:  AF, DE, HL.
; ==========================================================================
GETIMS
	jp SORRY

; ==========================================================================
; CLRSCN
; --------------------------------------------------------------------------
; Clear the screen.
; --------------------------------------------------------------------------
; Destroyed:  AF, DE, HL.
; ==========================================================================
CLRSCN:
	ld a,12
	.bcall "VDU.WriteByte"
	ret

; ==========================================================================
; OSCALL
; --------------------------------------------------------------------------
; Intercept a CALL or USR to &FFxx
; --------------------------------------------------------------------------
; Inputs:     IY: Contains destination address of CALL or USR (=&FFxx).
;             IX: Addresses "static" variables, i.e. A%=(IX+4), X%=(IX+96).
;             (SP+2): "return address" if interception carried out.
; Outputs:    HLH'L' contains 32-bit integer result
; Destroyed:  Everything.
; ==========================================================================
OSCALL:
	pop hl ; Dud
	
	ld h,(ix+4*('A'-'@'))
	ld l,(ix+4*('F'-'@'))
	
	push hl
	pop af
	
	ld b,(ix+4*('B'-'@'))
	ld c,(ix+4*('C'-'@'))
	ld d,(ix+4*('D'-'@'))
	ld e,(ix+4*('E'-'@'))
	ld h,(ix+4*('H'-'@'))
	ld l,(ix+4*('L'-'@'))

	ld iyh,$40
	jp (iy)

; ==========================================================================
; BASIC functions
; --------------------------------------------------------------------------
; On input IY points to the program text immediately following the function 
; token (note that the token for POINT includes the opening  parenthesis). 
; Functions should exit with a RET instruction, with the following registers
; loaded:
;            A = 0  (indicates numeric value)
;            C = 0  (indicates integer value)
;       HLH'L' = 32-bit integer value returned
;           IY = program text pointer
; ==========================================================================

; ==========================================================================
; ADVAL
; --------------------------------------------------------------------------
; Read analogue-digital convertor etc.
; ==========================================================================
ADVAL:
	call Basic.BBCBASIC_ITEMI
	bit 7,h
	jr nz,ADVAL.Negative

ADVAL.Positive:
	; Check that MSB = 0
	ld a,h
	or l
	exx
	push bc
	
	jr nz,ADVAL.Return0
	
	; Check that H = 0
	ld a,h
	or a
	jr nz,ADVAL.Return0
	
	; Now we can actually perform the ADVAL reading!
	ld a,l
	
	or a  \ jr z,ADVAL.JoystickButtons
	dec a \ jr z,ADVAL.Joystick1X
	dec a \ jr z,ADVAL.Joystick1Y
	dec a \ jr z,ADVAL.Joystick2X
	dec a \ jr z,ADVAL.Joystick2Y
	
	jr ADVAL.Return0


ADVAL.Negative:
	; Check that MSB = -1
	ld a,h
	and l
	inc a
	exx
	push bc
	
	jr nz,ADVAL.Return0
	
	; Check that H = -1
	ld a,h
	inc a
	jr nz,ADVAL.Return0
	
	ld a,l
	
	inc a \ jr z,ADVAL.Return0     ; Number of characters in keyboard buffer.
	inc a \ jr z,ADVAL.SerialIn    ; Number of characters in serial input buffer.
	inc a \ jr z,ADVAL.Return0     ; Number of characters in serial output buffer.
	inc a \ jr z,ADVAL.Return0     ; Number of characters in printer output buffer.
	inc a \ jr z,ADVAL.SoundBuffer ; Number of free spaces in sound channel 0 buffer.
	inc a \ jr z,ADVAL.SoundBuffer ; Number of free spaces in sound channel 1 buffer.
	inc a \ jr z,ADVAL.SoundBuffer ; Number of free spaces in sound channel 2 buffer.
	inc a \ jr z,ADVAL.SoundBuffer ; Number of free spaces in sound channel 3 buffer.
	inc a \ jr z,ADVAL.Return0     ; Number of free spaces in speech buffer.
	
	jr ADVAL.Return0
	
ADVAL.Return0:
	ld hl,0
ADVAL.Return:
	pop bc
	exx
	xor a
	ld c,a
	ld h,a
	ld l,a
	ret

ADVAL.JoystickButtons:
	ld hl,0
	
	in a,($DD) ; Bit 2 = B.TL
	rrca
	rrca
	rrca
	ccf
	rl l
	
	in a,($DC) ; Bit 4 = A.TL
	rlca
	rlca
	rlca
	rlca
	ccf
	rl l
	
	jr ADVAL.Return

ADVAL.Joystick1X:
	in a,($DC) ; Bits 2,3 = A.Left,A.Right
	ld b,2
	jr ADVAL.ReturnJoystickBits

ADVAL.Joystick1Y:
	in a,($DC) ; Bits 0,1 = A.Up,A.Down
	ld b,0
	jr ADVAL.ReturnJoystickBits

ADVAL.Joystick2X:
	in a,($DD) ; Bits 0,0 = B.Left,B.Right
	ld b,0
	jr ADVAL.ReturnJoystickBits

ADVAL.Joystick2Y:
	in a,($DC) ; Bits 6,7 = B.Up,B.Down
	ld b,6
	; Fall-through

ADVAL.ReturnJoystickBits:
	; Shift the value by B
	inc b
	dec b
	jr z,+
-:	rrca
	djnz -
+:	
	ld c,0
	ld h,c
	ld l,c
	
	rrca
	adc hl,bc
	rrca
	sbc hl,bc
	inc hl
	
	; Now HL is either 0, 1, or 2.
	
	; Scale up to 0, 16384, 32768.
	srl l \ rr h
	srl l \ rr h
	
	; Scale up to 65535, 32767, 0.
	add hl,hl
	dec hl
	adc hl,bc
	
	jr ADVAL.Return

ADVAL.SerialIn:
	ld hl,(Serial.SerialReadBuffer.Count)
	ld h,0
	jr ADVAL.Return

ADVAL.SoundBuffer:
	; l = -5 to -8
	ld a,-5
	sub l
	; *32
.if Sound.ChannelSize != 32
.fail "Sound channel size is expected to be 32"
.endif
	rrca
	rrca
	rrca
	ld c,a
	ld b,0
	ld hl,Sound.Channels+Sound.Channel.State
	add hl,bc
	ld a,(hl)
	
	cpl ; We have the queue length, so invert for free space.
	and %00001100
	
	bit 7,(hl)
	jr nz,+
	add a,4 ; If the channel is free, that's an implicit extra free space in the queue.
+:	ld l,a
	ld h,0
	jr ADVAL.Return

; ==========================================================================
; POINT
; --------------------------------------------------------------------------
; Read the colour of a screen pixel.
; ==========================================================================
POINT:
	call Basic.BBCBASIC_EXPRI
	exx
	push hl
	call Basic.BBCBASIC_COMMA
	call Basic.BBCBASIC_EXPRI
	exx
	pop de
	call Basic.BBCBASIC_BRAKET
	
	ex de,hl
	
	.bcall "VDU.GetPixel"
	
	ld l,a
	add a,a
	sbc a,a
	ld h,a
	exx
	ld h,a
	ld l,a

	xor a
	ld c,a
	ret
	

; ==========================================================================
; BASIC statements
; --------------------------------------------------------------------------
; On completion, statements must jump to XEQ with IY pointing to the program 
; text (i.e. just past the statement). The stack pointer (SP) should be
; unchanged from when the statement was entered. All other registers may be 
; destroyed.
; ==========================================================================

; ==========================================================================
; CLG
; --------------------------------------------------------------------------
; Clear graphics window to graphics background colour.
; ==========================================================================
CLG:
	ld a,16
	.bcall "VDU.WriteByte"
	jp Basic.BBCBASIC_XEQ

; ==========================================================================
; COLOUR
; --------------------------------------------------------------------------
; Change text foreground or background colour.
; ==========================================================================
COLOUR:
	; Get the first argument.
	call Basic.BBCBASIC_EXPRI
	exx
	
-:	ld a,(iy)
	cp ','
	jr z,COLOUR.AtLeastTwoArguments
	cp ' '
	jr nz,COLOUR.SingleArgument
	inc iy
	jr -

COLOUR.AtLeastTwoArguments:

	push hl
	inc iy
	
	call Basic.BBCBASIC_EXPRI
	exx
	
-:	ld a,(iy)
	cp ','
	jr z,COLOUR.AtLeastThreeArguments
	cp ' '
	jr nz,COLOUR.TwoArguments
	inc iy
	jr -

COLOUR.AtLeastThreeArguments
	
	; COLOUR l,r,g,b
	push hl ; Push R to stack.
	inc iy
	
	call Basic.BBCBASIC_EXPRI
	exx
	push hl ; Push G to stack.
	
	call Basic.BBCBASIC_COMMA
	call Basic.BBCBASIC_EXPRI
	exx
	
	       ; hl = B
	pop bc ; bc = G
	ld b,l ; bc = G,B
	
	pop de ; bc = R
	ld d,e
	ld e,16 ; bc = 16,R
	
	pop hl  ; hl = L
	ld h,l
	ld l,19 ; hl = 19,L
	
	.bcall "VDU.WriteWord"
	ld l,e \ ld h,d
	.bcall "VDU.WriteWord"
	ld l,c \ ld h,b
	.bcall "VDU.WriteWord"
		
	jp Basic.BBCBASIC_XEQ

COLOUR.TwoArguments:
	
	; COLOUR l,p
	pop de
	ex de,hl
	ld h,e
	
	ld a,19
	.bcall "VDU.WriteByte"
	.bcall "VDU.WriteWord"
	
	ld b,3
-:	xor a
	.bcall "VDU.WriteByte"
	djnz -
	jp Basic.BBCBASIC_XEQ
	

COLOUR.SingleArgument:
	
	; COLOUR c
	ld h,l
	ld l,17
	
	.bcall "VDU.WriteWord"
	jp Basic.BBCBASIC_XEQ

; ==========================================================================
; DRAW
; --------------------------------------------------------------------------
; Draw a line.
; ==========================================================================
DRAW:
	ld hl,5
	jp PLOT.CommandInHL

; ==========================================================================
; ENVEL
; --------------------------------------------------------------------------
; Define a pitch and amplitude envelope.
; ==========================================================================
ENVEL:
	call Basic.BBCBASIC_EXPRI ;Get first parameter (envelope number)
	exx
	
	; Is it in range?
	ld a,l
	dec a
	cp Sound.EnvelopeCount
	jp nc,Sorry
	
	di
	
	call Sound.GetEnvelopeAddressOffset
	ld ix,Sound.Envelopes
	add ix,de

	ld b,13
-:	push bc
	push ix
	call Basic.BBCBASIC_COMMA
	call Basic.BBCBASIC_EXPRI
	exx
	pop ix
	pop bc
	ld (ix),l
	inc ix
	djnz -
	
	ei
	jp Basic.BBCBASIC_XEQ
	
; ==========================================================================
; GCOL
; --------------------------------------------------------------------------
; Change graphics colour and plotting action.
; ==========================================================================
GCOL:
	call Basic.BBCBASIC_EXPRI
	exx
-:	ld a,(iy)
	cp ','
	jr z,GCOLWithComma
	cp ' '
	jr nz,+
	inc iy
	jr -
+:	xor a
	jr FoundGCOL
GCOLWithComma:
	push hl
	call Basic.BBCBASIC_COMMA
	call Basic.BBCBASIC_EXPRI
	exx
	pop de
	ld a,e
FoundGCOL:
	push af
	ld a,18
	.bcall "VDU.WriteByte"
	pop af
	.bcall "VDU.WriteByte"
	ld a,l
	.bcall "VDU.WriteByte"
	jp Basic.BBCBASIC_XEQ

; ==========================================================================
; MODE
; --------------------------------------------------------------------------
; Change screen mode.
; ==========================================================================
MODE:
	call Basic.BBCBASIC_EXPRI
    exx
	ld h,l
	ld l,22
	.bcall "VDU.WriteWord"
	jp Basic.BBCBASIC_XEQ

; ==========================================================================
; MOVE
; --------------------------------------------------------------------------
; Move graphics cursor.
; ==========================================================================
MOVE:
	ld hl,4
	jp PLOT.CommandInHL

; ==========================================================================
; PLOT
; --------------------------------------------------------------------------
; Plot a graphics shape.
; ==========================================================================
PLOT:
	call Basic.BBCBASIC_EXPRI
	exx
	call Basic.BBCBASIC_COMMA
	
PLOT.CommandInHL:
	push hl
	
	call Basic.BBCBASIC_EXPRI
	exx
	push hl
	
	call Basic.BBCBASIC_COMMA
	
	call Basic.BBCBASIC_EXPRI
	exx
	
	; At this point, we can load the data directly into the VDU buffer.
	
	ld (VDUQ(3, 5)),hl ; Y
	pop hl
	ld (VDUQ(1, 5)),hl ; X
	pop hl
	
	ld h,l
	ld l,25 ; PLOT
	.bcall "VDU.WriteWord"
	
	ld hl,(VDUQ(1, 5)) ; X
	.bcall "VDU.WriteWord"
	
	ld hl,(VDUQ(3, 5)) ; Y
	.bcall "VDU.WriteWord"
	
	jp Basic.BBCBASIC_XEQ

; ==========================================================================
; SOUND
; --------------------------------------------------------------------------
; Make a sound.
; ==========================================================================
SOUND:
	
	call Basic.BBCBASIC_EXPRI ;Get first parameter (channel)
	exx
	
	ld a,h
	
	cp $20
	jp z,Sorry ; &20xx, word, library, 0 = Watford Speech
	
	and $F0
	jr z,SOUND.Internal
	cp $10
	jr z,SOUND.Internal
	
	ld a,h
	
	inc a
	jp z,Sorry ; &FFxx, command, 0, 0 = Speech system
	
	inc a
	jr z,SOUND.MIDI ; &FExx, command, note, velocity = MIDI control
	
SOUND.Internal:
	
	push hl
	
	call Basic.BBCBASIC_COMMA
	
	call Basic.BBCBASIC_EXPRI ;Get second parameter (amplitude)
	exx
	push hl
	
	call Basic.BBCBASIC_COMMA
	
	call Basic.BBCBASIC_EXPRI ;Get third parameter (pitch)
	exx
	push hl
	
	call Basic.BBCBASIC_COMMA
	
	call Basic.BBCBASIC_EXPRI ;Get fourth parameter (duration)
	exx
	
	ld a,l ; a = duration
	pop de ; de = pitch
	pop hl ; hl = amplitude
	pop bc ; bc = channel
	
	
-:	push ix
	call Sound.QueueCommand
	pop ix
	ei
	
	jp z,Basic.BBCBASIC_XEQ
	
	halt
	
	push af
	push hl
	call TRAP
	pop hl
	pop af
	
	jr -

Sound.MIDI:
	; HL = &FEFF (-257): Send to current channel.
	; HL = &FEFE (-258): Send to raw channel.
	call Basic.BBCBASIC_COMMA
	
	call Basic.BBCBASIC_EXPRI ; command
	exx
	push hl
	
	call Basic.BBCBASIC_COMMA
	
	call Basic.BBCBASIC_EXPRI ; note
	exx
	push hl
	
	call Basic.BBCBASIC_COMMA
	
	call Basic.BBCBASIC_EXPRI ; velocity
	exx
	
	; TODO: Send MIDI.
	
	jp Basic.BBCBASIC_XEQ

; ==========================================================================
; Operating System calls
; --------------------------------------------------------------------------
; Additional routines not covered above that are provided for compatibility
; with programs written for other platforms.
; ==========================================================================

; ==========================================================================
; OSBYTE
; --------------------------------------------------------------------------
; Various OS routines with parameters passed in L and H.
; --------------------------------------------------------------------------
; Inputs:     A: OSBYTE routine number.
;             L: OSBYTE parameter (X)
;             H: OSBYTE parameter (Y)
; ==========================================================================
OSBYTE:

	; A = OSBYTE routine
	; L = low byte of the parameter (X)
	; H = high byte of the parameter (Y)
	
	; Routines &A6..&FF take the form:
	; New Value = (<old value> AND H) EOR L
	; Return old value in L.

	or a
	jp z,OSBYTE.ReadHostOS
	cp 4
	jp z,OSBYTE.CursorEditing
	cp 11
	jp z,OSBYTE.KeyboardAutoRepeatDelay
	cp 12
	jp z,OSBYTE.KeyboardAutoRepeatRate
	
	cp 124
	jp z,ClearEscape
	cp 125
	jp z,SetEscape
	cp 126
	jp z,AcknowledgeEscape
	
	cp 137
	jp z,OSBYTE.SwitchCassetteRelay
	cp 139
	jp z,OSBYTE.SetFileSystemOptions

	; Sound suppression and bell
	cp 210
	jr nz,OSBYTE.NotSoundSuppression
	
	; We can only modify the low bit of the status value.
	ld a,l
	or a
	ld l,0
	jr z,+
	inc l
+:	ld a,h
	or %11111110
	ld h,a
	ld a,210
	jr OSBYTE.ChangeSound
	
OSBYTE.NotSoundSuppression:
	cp 211
	jr c,OSBYTE.NotSoundBell
	cp 214+1
	jr nc,OSBYTE.NotSoundBell

OSBYTE.ChangeSound:
	push de
	ld de,Sound.Status-210
	call OSBYTE.ModifyMemory
	pop de
	
	; If it's sound suppression, make sure we silence any playing sounds!
	cp 210
	ret nz
	
	ld a,(Sound.Status)
	bit Sound.Status.SoundSuppressed,a
	jr z,+
	push hl
	call Sound.Silence
	pop hl
+:	ld a,l
	and 1
	ld l,a
	ld a,210
	ret
	
OSBYTE.NotSoundBell:
	
	cp 200
	jr z,OSBYTE.EscapeErrorDisable
	
	cp 229
	jr z,OSByte.EscapeKeyDisable

	ret

OSBYTE.ReadHostOS:	
	ld a,l
	or a
	jr nz,+
	
	ld a,247
	call Basic.BBCBASIC_EXTERR
	.db "OS SEGA 0.01", 0
	
+:	ld l,8
	xor a
	ret

OSBYTE.CursorEditing:
	di
	
	ld a,(Host.Flags)
	bit 0,l
	jr z,OSBYTE.TranslateCursorKeys
OSBYTE.FixCursorKeys
	set CursorKeysFixed,a
	jr OSBYTE.FunctionKeyExit
OSBYTE.TranslateCursorKeys:
	res CursorKeysFixed,a
OSBYTE.FunctionKeyExit:
	ld (Host.Flags),a

	ld a,l
	or a
	ld a,(VDU.Console.ConsoleFlags)
	jr z,OSBYTE.EnableCursorEditing

OSBYTE.DisableCursorEditing:
	set VDU.Console.CursorEditingDisabled,a
	jr OSBYTE.CursorEditingExit

OSBYTE.EnableCursorEditing:
	res VDU.Console.CursorEditingDisabled,a
	
OSBYTE.CursorEditingExit:
	ld (VDU.Console.ConsoleFlags),a
	ld a,4
	ei
	ret
	

OSBYTE.EscapeErrorDisable:
	push de
	push bc
	ld de,Flags
	ld c,1<<EscapeErrorDisabled
	call OSBYTE.ModifyFlag
	pop bc
	pop de
	ret

OSBYTE.EscapeKeyDisable:
	push de
	push bc
	ld de,Flags
	ld c,1<<EscapeKeyDisabled
	call OSBYTE.ModifyFlag
	pop bc
	pop de
	ret

OSBYTE.KeyboardAutoRepeatDelay:
	push af
	push hl
	
	; If H is non-zero, skip.
	ld a,h
	or a
	jr nz,OSBYTE.SendNewKeyboardRate
	
	; Convert HL's time in centiseconds to time in 1/4 seconds.
	.bcall "VDU.Graphics.DivideBy5"
	.bcall "VDU.Graphics.DivideBy5"
	ld a,l
	or a
	jr z,+
	dec a
	adc a,0
+:	cp 4
	jr c,+
	ld a,3
+:	
	rrca
	rrca
	rrca
	and %01100000
	ld l,a
	
	ld a,(KeyboardRate)
	and %00011111
	or l
	ld (KeyboardRate),a

OSBYTE.SendNewKeyboardRate:
	ld a,$F3
	call AT.SendSafeByte
	ld a,(KeyboardRate)
	call AT.SendSafeByte
	pop hl
	pop af
	ret

OSBYTE.KeyboardAutoRepeatRate:
	push af
	push hl

	; If H is non-zero, skip.
	ld a,h
	or a
	jr nz,OSBYTE.SendNewKeyboardRate
	
	ld a,l
	or a
	jr nz,+
	
	ld a,%0101011
	ld (KeyboardRate),a
	jr OSBYTE.SendNewKeyboardRate

+:	ld hl,TypematicRates
	ld bc,31*256
-:	cp (hl)
	jr z,+
	jr c,+
	inc c
	inc hl
	djnz -
+:	
	ld a,(KeyboardRate)
	and %01100000
	or c
	ld (KeyboardRate),a
	jr OSBYTE.SendNewKeyboardRate

TypematicRates:
	.db round(3.333333333)
	.db round(3.745318352)
	.db round(4.166666667)
	.db round(4.587155963)
	.db round(4.830917874)
	.db round(5.405405405)
	.db round(5.847953216)
	.db round(6.25)
	.db round(6.666666667)
	.db round(7.518796992)
	.db round(8.333333333)
	.db round(9.174311927)
	.db round(10)
	.db round(10.86956522)
	.db round(11.62790698)
	.db round(12.5)
	.db round(13.33333333)
	.db round(14.92537313)
	.db round(16.66666667)
	.db round(18.18181818)
	.db round(20)
	.db round(21.73913043)
	.db round(23.25581395)
	.db round(25)
	.db round(27.02702703)
	.db round(30.3030303)
	.db round(33.33333333)
	.db round(37.03703704)
	.db round(40)
	.db round(43.47826087)
	.db round(47.61904762)
	.db round(50)

OSBYTE.SwitchCassetteRelay:
	push af
	call Tape.SetMotorState
	pop af
	ret

OSBYTE.SetFileSystemOptions:
	ld a,h
	inc l
	dec l
	jr z,OSBYTE.ResetFileSystemOptions
	dec l
	jr z,OSBYTE.SetFileSystemMessageOptions
	dec l
	jr z,OSBYTE.SetFileSystemErrorOptions
	jp CLI.BadCommand


OSBYTE.ResetFileSystemOptions:
	ld a,%00000101
	ld (File.Options),a
	ld a,139
	ret

OSBYTE.SetFileSystemMessageOptions:
	cp 3
	jp nc,CLI.BadCommand
	ld a,(File.Options)
	and %11111100
	or h
	ld (File.Options),a
	ld a,139
	ret

OSBYTE.SetFileSystemErrorOptions:
	cp 3
	jp nc,CLI.BadCommand
	ld a,(File.Options)
	and %11110011
	sla h
	sla h
	or h
	ld (File.Options),a
	ld a,139
	ret

; ==========================================================================
; OSBYTE.ModifyMemory
; --------------------------------------------------------------------------
; Modifies memory based on OSBYTE routine number and H,L values.
; --------------------------------------------------------------------------
; Inputs:     DE: Pointer to memory to modify minus the routine number.
;             A: OSBYTE routine number.
;             L: New value to EOR over the value in memory.
;             H: Mask to AND over the existing value.
; Outputs:    L: The old value (X)
;             H: The next value in memory.
; Destroyed:  None.
; ==========================================================================
OSBYTE.ModifyMemory:
	push af
	push bc
	push de
	
	ex de,hl	
	ld c,a
	ld b,0
	add hl,bc
	
	; Read and modify.
	ld a,(hl)
	and d
	xor e
	
	; Fetch back old value and store new one.
	ld e,(hl)
	ld (hl),a
	
	; Read next value in memory.
	inc hl
	ld d,(hl)
	ex de,hl
	
	pop de
	pop bc
	pop af
	ret

; ==========================================================================
; OSBYTE.ModifyFlag
; --------------------------------------------------------------------------
; Modifies a single flag value.
; --------------------------------------------------------------------------
; Inputs:     DE: Address of the flag in memory.
;             C: Bitmask of the flag.
;             A: OSBYTE routine number.
;             L: New value to EOR over the value in memory.
;             H: Mask to AND over the existing value.
; Outputs:    L: The old value (X)
; Destroyed:  None.
; ==========================================================================

OSBYTE.ModifyFlag:
	push af
	push bc
	push de
	
	ex de,hl
	
	ld a,(hl)
	and c
	jr z,+
	ld a,1
+:	ld b,a

	and d
	xor e
	ld a,c
		
	jr z,+
	
	or (hl)
	jr ++
	
+:	cpl
	and (hl)
	
++:	ld e,b
	ld (hl),a
	
	ex de,hl
	
	pop de
	pop bc
	pop af
	ret

; ==========================================================================
; OSWORD
; --------------------------------------------------------------------------
; Various OS routines with parameters pointed to by HL.
; --------------------------------------------------------------------------
; Inputs:     A: OSWORD routine number.
;             HL: Pointer to parameters.
; ==========================================================================
OSWORD:
	ret

; Big list of file-system "todo"s...
OSFSC:
OSFIND:
OSGBPB:
OSARGS:
OSFILE:
	jp Sorry

; ==========================================================================
; GetSafeScratchMemory
; --------------------------------------------------------------------------
; Gets a safe pointer to a free block of memory for temporary usage.
; --------------------------------------------------------------------------
; Inputs:     BC: Amount of memory required.
; Outputs:    F: Carry set if there's not enough space.
;             HL: Address of allocated memory.
; Destroyed:  AF, BC HL.
; Interrupts: Enabled.
; ==========================================================================
GetSafeScratchMemory:
	push de
	
	; Get the free memory pointer in DE.
	; If it's < $C000 move it up to $8000 so it's always in work RAM.
	; (If it's < $C000 in cartridge RAM, which can be paged out!)
	ld de,(Basic.BBCBASIC_FREE)
	ld a,d
	cp $C0
	jr nc,+
	ld de,$C000
+:	
	
	; Where is SP?
	ld hl,0
	add hl,sp
	
	; Is there enough room between DE and SP?
	or a
	sbc hl,de
	jr c,+
	
	; HL = free memory, is that bigger than BC?
	sbc hl,bc

+:	ex de,hl
	pop de
	ret

; ==========================================================================
; GetSafeScratchMemoryHL
; --------------------------------------------------------------------------
; Gets a safe pointer to a free block of memory for temporary usage. This
; is similar to GetSafeScratchMemory but takes the amount of free memory
; in HL.
; --------------------------------------------------------------------------
; Inputs:     HL: Amount of memory required.
; Outputs:    F: Carry set if there's not enough space.
;             HL: Address of allocated memory.
; Destroyed:  AF, HL.
; Interrupts: Enabled.
; ==========================================================================
GetSafeScratchMemoryHL:
	push bc
	ld b,h
	ld c,l
	call GetSafeScratchMemory
	pop bc
	ret

; ==========================================================================
; GetSafeScratchMemoryDE
; --------------------------------------------------------------------------
; Gets a safe pointer to a free block of memory for temporary usage. This
; is similar to GetSafeScratchMemory but takes the amount of free memory
; in DE and returns the address in DE.
; --------------------------------------------------------------------------
; Inputs:     DE: Amount of memory required.
; Outputs:    F: Carry set if there's not enough space.
;             DE: Address of allocated memory.
; Destroyed:  AF, HL.
; Interrupts: Enabled.
; ==========================================================================
GetSafeScratchMemoryDE:
	ex de,hl
	call GetSafeScratchMemoryHL
	ex de,hl
	ret

; ==========================================================================
; Sorry
; --------------------------------------------------------------------------
; Triggers the "Sorry" error.
; ==========================================================================
Sorry:
	xor a
	call Basic.BBCBASIC_EXTERR
	.db "Sorry", 0

; ==========================================================================
; DeviceFault
; --------------------------------------------------------------------------
; Triggers the "Device fault" error.
; ==========================================================================
DeviceFault:
	ld a,202
	call Basic.BBCBASIC_EXTERR
	.db "Device fault", 0

; ==========================================================================
; BadString
; --------------------------------------------------------------------------
; Triggers the "Bad string" error.
; ==========================================================================
BadString:
	ld a,253
	call Basic.BBCBASIC_EXTERR
	.db Tokens.Bad, "string", 0

.endmodule