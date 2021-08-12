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
CursorKeysDisabled = 2 ; OSBYTE 4
EscapeKeyDisabled = 3
EscapeErrorDisabled = 4

OSLINE.Override = allocVar(2)
OSWRCH.Override = allocVar(2)

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSINIT ===
;
;   Initialise operating system and filing system.
;
; OUTPUTS:
;   REGISTERS
;   * DE - initial value of HIMEM.
;   * HL - initial value of PAGE.
;   * F  - Zero flag set = don't boot.
;        - Zero flag reset = CHAIN file named in string accumulator.
;
; DESTROYED:
;   REGISTERS:
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
OSINIT:
	
	; Reset the clock.
	ld hl,0
	ld (TIME+0),hl
	ld (TIME+2),hl
	
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

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSRDCH ===
;
;   Wait for a keypress at the console keyboard.
;
; OUTPUTS:
;   REGISTERS
;   * A  - ASCII code of key pressed.
;
; DESTROYED:
;   REGISTERS:
;   * AF
;
;@doc:end
;------------------------------------------------------------------------------- 
OSRDCH:
	call VDU.BeginBlinkingCursor
	call KeyboardBuffer.CheckKeyboardByPolling
	
-:	call KeyboardBuffer.GetKey
	
	jr z,+              ; No key.
	jp m,+              ; Ignore extended keys.
	jr OSRDCH.GotKey

+:	push af
	call VDU.DrawBlinkingCursor
	pop af
	jr -

OSRDCH.GotKey:
	push af
	call VDU.EndBlinkingCursor	
	pop af
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSKEY ===
;
;   Wait a specified maximum time for a keypress at the console.
;
; INPUTS:
;   REGISTERS
;   * HL - time limit (centiseconds). If HL = 0 return immediately.
;
; OUTPUTS:
;   REGISTERS
;   * F  - Carry flag reset on time-out, set if a key was detected.
;   * A  - ASCII code of key pressed.
;
; DESTROYED:
;   REGISTERS:
;   * AF, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
OSKEY:
	push bc
	push de
		
	bit 7,h
	jr z,OSKEY.NotNegative
	
	inc h
	jr nz,OSKEY.Return0
	
	ld a,l
	or a
	ld a,'?'
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
	
	call KeyboardBuffer.CheckKeyboardByPolling
	
	; Calculate the end time-1 (easier to check for <0 instead of <=0 later)
	ld de,(TIME)
	dec de
	add hl,de

	ei
OSKEY.Loop:
	call KeyboardBuffer.GetKey
	
	jr z,OSKEY.NoKey
	jp m,OSKey.NoKey
	
	jr OSKEY.GotKey

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
	call VDU.DrawBlinkingCursor
	halt
	jr OSKEY.Loop

OSKEY.TimedOut:
	scf ; Timed out, so pretend we dont have a key.
	
OSKEY.GotKey:
	ccf
	
	push af
	call VDU.EndBlinkingCursor
	pop af
	
	pop de
	pop bc
	ei
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSLINE ===
;
;   Read a line, terminated by RETURN, from the console.
;
; INPUTS:
;   REGISTERS
;   * HL - pointer to destination RAM for entered line. A maximum of 256 bytes
;          are available for the line, including the CR terminator.
;
; OUTPUTS:
;   REGISTERS
;   * A  - Must be zero.
;
; DESTROYED:
;   REGISTERS:
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
OSLINE:

	ld bc,(OSLINE.Override)
	ld a,b
	or a
	jr z,+
	push bc
	ret

+:
	call KeyboardBuffer.CheckKeyboardByPolling
	call VDU.BeginBlinkingCursor	
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
	call VDU.DrawBlinkingCursor
	call KeyboardBuffer.GetKey
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
	call VDU.PutChar
	jr OSLINE.Loop
+:

	; Check if it's in the range 32..127.
	cp 32
	jr c,OSLINE.Loop
	cp 128
	jr nc,OSLINE.Loop
	
	; Store the character in E.
	ld e,a
	
	; Is this going to be an insert or overwrite command?
	ld a,(VDU.Console.Flags)
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
	call VDU.EndBlinkingCursor
	pop af
	
	; Print the character.
	call VDU.PutChar
	jr OSLINE.Loop

OSLINE.InsertCharacter
	; Check if we've got enough space.
	ld a,c
	or a
	jr z,OSLINE.Loop

OSLINE.EnoughSpace:	
	
	push af
	call VDU.EndBlinkingCursor
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
	call VDU.PutChar
	
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
	call nz,VDU.PutMap
	jp OSLINE.Loop

OSLINE.RepaintToEnd.HasCharacters:

	; We need to repaint...
	push bc
	push hl
	ld b,a
	
	; Redraw the rest of the characters.
	push bc
-:	ld a,(hl)
	call VDU.PutChar
	inc hl
	djnz -
	
	ld a,e
	cp '\r'
	jr nz,+
	ld a,127
+:	or a
	call nz,VDU.PutMap
	
	; Move the cursor back to where it should be.
	pop bc
-:	call VDU.CursorLeft
	djnz -
	
	pop hl
	pop bc
	jp OSLINE.Loop

OSLINE.Return:
	
	; Move to the end.
	call OSLINE.End
	
	call VDU.EndBlinkingCursor
	
	ld a,'\r'
	call VDU.PutChar
	
	xor a
	ret

OSLINE.ExtendedKey:
	
	cp Keyboard.KeyCode.Home
	call z,OSLINE.Home
	
	cp Keyboard.KeyCode.End
	call z,OSLINE.End
	
	cp Keyboard.KeyCode.Left
	call z,OSLINE.Left
	
	cp Keyboard.KeyCode.Right
	call z,OSLINE.Right
	
	jp OSLINE.Loop

OSLINE.Delete:
	; Same as moving right once, then backspacing.
	
	; Are we at the end?
	ld a,d
	cp b
	jp z,OSLINE.Loop
	
	call VDU.EndBlinkingCursor
	call VDU.CursorRight
	inc hl
	inc d
	
	; Run-on to Backspace handler.

OSLINE.Backspace:
	; Are we at the start of the string? (Offset from start = 0)?
	ld a,d
	or a
	jp z,OSLINE.Loop
	
	call VDU.EndBlinkingCursor
	
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
	call VDU.CursorLeft
	
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

	call VDU.EndBlinkingCursor

-:	call VDU.CursorLeft
	dec hl
	dec d
	jr nz,-
	
+:	pop af
	ret
	
OSLINE.End:
	push af
	; Are we already at the end?
	ld a,d
	cp b
	jr z,+

	call VDU.EndBlinkingCursor

-:	call VDU.CursorRight
	inc hl
	inc d
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
	
	call VDU.EndBlinkingCursor
	call VDU.CursorLeft
	dec hl
	dec d
	
	
+:	pop af
	ret

OSLINE.Right:
	push af
	
	; Are we already at the end?
	ld a,d
	cp b
	jr z,+
	
	call VDU.EndBlinkingCursor
	call VDU.CursorRight
	inc hl
	inc d
	
	
+:	pop af
	ret

OSLINE.Clear:
	
	; Is the line already clear?
	ld a,b
	or a
	jp z,OSLINE.Loop
	
	push af
	call VDU.EndBlinkingCursor
	pop af
	
	; Move to the end of the line.
	sub d
	jr z,+
	
-:	push af
	call VDU.CursorRight
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
	call VDU.PutChar
	djnz -
	
	jp OSLINE.Loop

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSLINE.Prefilled ===
;
;   Read a line, terminated by RETURN, from the console.
;   The input buffer pointed to by HL may be pre-filled.
;
; INPUTS:
;   REGISTERS
;   * HL - pointer to destination RAM for entered line. A maximum of 256 bytes
;          are available for the line, including the CR terminator.
;
; OUTPUTS:
;   REGISTERS
;   * A  - Must be zero.
;
; DESTROYED:
;   REGISTERS:
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
OSLINE.Prefilled:
	call KeyboardBuffer.CheckKeyboardByPolling
	call VDU.BeginBlinkingCursor
	ld bc,255
	ld d,0
-:	ld a,(hl)
	cp '\r'
	jp z,OSLINE.Loop
	call VDU.PutLiteralChar ; In case there are any control codes embedded in the line.
	call VDU.Console.FlushPendingScroll
	inc d
	inc hl
	inc b
	dec c
	jr -

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSASCI ===
;
;   Outputs a character using OSWRCH.
;   ASCII CR (13) is converted to a LF, CR sequence (13, 10).
;
; INPUTS:
;   REGISTERS
;   * A  - character to output.
;
;@doc:end
;------------------------------------------------------------------------------- 
OSASCI:
	call VDU.PutChar
	ei
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.PROMPT ===
;
;   Print the normal BASIC prompt character.
;
; INPUTS:
;   REGISTERS
;   * F  - Carry reset for "long" prompt, set for "short" prompt.
;
; DESTROYED:
;   REGISTERS:
;   * AF, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
PROMPT:
	ld a,'>'
	; Fall-through to OSWRCH

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSWRCH ===
;
;   Send a character to the console output device (screen).
;
; INPUTS:
;   REGISTERS
;   * A  - character to output.
;
;@doc:end
;------------------------------------------------------------------------------- 
OSWRCH:
	push hl
	push af
	
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
	
	call VDU.WriteByte
	ei
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.LTRAP ===
;
;   Test for an operator abort when LISTing (ESCape).
;
; DESTROYS:
;   REGISTERS
;   * AF
;
;@doc:end
;------------------------------------------------------------------------------- 
LTRAP:
;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.TRAP ===
;
;   Test for an operator abort when running a program (ESCape).
;
; DESTROYS:
;   REGISTERS
;   * AF, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
TRAP:
	ei
	
	call KeyboardBuffer.CheckKeyboardWithInterrupt
	
	call CheckEscape
	
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
	
	call VDU.Console.EndBlinkingCursor
	
	ld a,17 ; Escape
	call Basic.BBCBASIC_EXTERR
	.db "Escape", 0

TrapFileTransfers:
	ei
	
	call KeyboardBuffer.CheckKeyboardWithInterrupt
	call VDU.DrawBlinkingCursor
	
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

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.RESET ===
;
;   Reset system prior to outputting an error message.
;
; DESTROYED:
;   REGISTERS:
;   * AF
;
;@doc:end
;------------------------------------------------------------------------------- 
RESET:

	push hl
	push de
	push bc
	
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
	
	pop bc
	pop de
	pop hl
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSLOAD ===
;
;   Load a program file into RAM.
;
; INPUTS:
;   REGISTERS
;   * HL - points to a filename terminated by CR.
;   * DE - address in RAM to load file to.
;   * BC - maximum file size that can be loaded.
;
; OUTPUTS:
;   REGISTERS
;   * F  - Carry reset for insufficient room error.
;
; DESTROYED:
;   REGISTERS:
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
OSLOAD:
	call VDU.BeginBlinkingCursor
	call PCLink2.GetFile
	push af
	call VDU.EndBlinkingCursor
	pop af
	jr nz,OSLOAD.Error
	jp c,TRAP.Escape
	scf
	ret
	
OSLOAD.Error:
	; What sort of error?
	jp nc,DeviceFault
	ccf
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSSAVE ===
;
;   Save an area of RAM to a program file.
;
; INPUTS:
;   REGISTERS
;   * HL - points to a filename terminated by CR.
;   * DE - address in RAM at which the data to save starts.
;   * BC - length of the data to save (bytes).
;
;
; DESTROYED:
;   REGISTERS:
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
OSSAVE:
	call VDU.BeginBlinkingCursor
	call PCLink2.SendFile
	push af
	call VDU.EndBlinkingCursor
	pop af
	jp nz,DeviceFault
	jp c,TRAP.Escape
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSCALL ===
; 
;   Intercept a CALL or USR to &FFxx.
;
; INPUTS:
;   REGISTERS
;   * IY     - contains destination address of CALL or USR (=&FFxx)
;   * IX     - IX addresses "static" variables, i.e. A%=(IX+4), X%=(IX+96) etc.
;   * (SP+2) - "return address" if interception carried out
;
; OUTPUTS:
;   REGISTERS
;   * HLHL' - 32-bit integer result (USR only).
;
; DESTROYED:
;   REGISTERS
;   * Everything.
;
;@doc:end
;------------------------------------------------------------------------------- 
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

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSSHUT ===
; 
;   Close a file previously opened with OSOPEN.
;
; INPUTS:
;   REGISTERS
;   * E  - file handle (channel number) to close.
;        - If E is zero, then all open files (if any) are closed.
;
; DESTROYED:
;   REGISTERS
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
OSSHUT:
	ld a,e
	or a
	ret z
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSCLI ===
; 
;   Execute a "star" command.
;
; INPUTS:
;   REGISTERS
;   * HL - addresses the "star" command in RAM, terminated by CR.
;
; DESTROYED:
;   REGISTERS
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
OSCLI = CLI.Execute

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.MODE ===
; 
;   Change screen mode.
;
;@doc:end
;------------------------------------------------------------------------------- 
MODE:
	call Basic.BBCBASIC_EXPRI
    exx
	ld h,l
	ld l,22
	call VDU.WriteWord
	jp Basic.BBCBASIC_XEQ

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.PUTSCR ===
; 
;   Move the text cursor to a given location.
;
; INPUTS:
;   REGISTERS
;   * DE - horizontal position of cursor (0 is the left-hand column)
;   * HL - vertical position of cursor (0 is the top row)
;
; DESTROYED:
;   REGISTERS
;   * AF, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
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
	call VDU.WriteByte
	
	pop hl
	call VDU.WriteWord
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.GETCSR ===
; 
;   Return the current text cursor coordinates.
;
; OUTPUTS:
;   REGISTERS
;   * DE - horizontal position of cursor (0 is the left-hand column)
;   * HL - vertical position of cursor (0 is the top row)
;
; DESTROYED:
;   REGISTERS
;   * AF, DE, HL
;
;------------------------------------------------------------------------------- 
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

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.PUTIME ===
; 
;   Update the elapsed time clock.
;
; INPUTS:
;   REGISTERS
;   * DEHL - time to load (centiseconds).
;
; DESTROYED:
;   REGISTERS
;   * AF, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
PUTIME:
	di
	ld (TIME+0),hl
	ld (TIME+2),de
	ei
	ret


;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.GETIME ===
; 
;   Read the elapsed time clock.
;
; OUTPUTS:
;   REGISTERS
;   * DEHL - current value of elapsed time (centiseconds).
;
; DESTROYED:
;   REGISTERS
;   * AF, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
GETIME:
	di
	ld hl,(TIME+0)
	ld de,(TIME+2)
	ei
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.CLRSCN ===
; 
;   Clears the screen.
;
; DESTROYED:
;   REGISTERS
;   * AF, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
CLRSCN:
	ld a,12
	jp VDU.WriteByte

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.ADVAL ===
; 
;   Analogue-to-digital conversion.
;
;@doc:end
;------------------------------------------------------------------------------- 
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

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.POINT ===
;
;   Read the colour of a screen pixel.
;
;@doc:end
;------------------------------------------------------------------------------- 
POINT:
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.COLOUR ===
; 
;   Change text foreground or background colour.
;
;@doc:end
;------------------------------------------------------------------------------- 
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
	
	call VDU.WriteWord
	ld l,e \ ld h,d
	call VDU.WriteWord
	ld l,c \ ld h,b
	call VDU.WriteWord
		
	jp Basic.BBCBASIC_XEQ

COLOUR.TwoArguments:
	
	; COLOUR l,p
	pop de
	ex de,hl
	ld h,e
	
	ld a,19
	call VDU.WriteByte
	call VDU.WriteWord
	
	ld b,3
-:	xor a
	call VDU.WriteByte
	djnz -
	jp Basic.BBCBASIC_XEQ
	

COLOUR.SingleArgument:
	
	; COLOUR c
	ld h,l
	ld l,17
	
	call VDU.WriteWord
	jp Basic.BBCBASIC_XEQ

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.CLG ===
; 
;   Clear graphics window to graphics background colour.
;
;@doc:end
;------------------------------------------------------------------------------- 
CLG:
	ld a,16
	call VDU.WriteByte
	jp Basic.BBCBASIC_XEQ

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.DRAW ===
; 
;   Draw a line.
;
;@doc:end
;------------------------------------------------------------------------------- 
DRAW:
	ld hl,5
	jp PLOT.CommandInHL

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.GCOL ===
; 
;   Change graphics colour and plotting action
;
;@doc:end
;------------------------------------------------------------------------------- 
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
	call VDU.WriteByte
	pop af
	call VDU.WriteByte
	ld a,l
	call VDU.WriteByte
	jp Basic.BBCBASIC_XEQ

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.PUTIMS ===
; 
;   Read the real-time clock.
;
; INPUTS:
;   MEMORY
;   * ACC$ - Time string stored in string accumulator
;   REGISTERS
;   * DE - the address of the byte following the last byte of the string.
;
; DESTROYED:
;   REGISTERS
;   * AF, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
PUTIMS
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.GETIMS ===
; 
;   Read the real-time clock.
;
; OUTPUTS:
;   MEMORY
;   * ACC$ - Time string stored in string accumulator
;   REGISTERS
;   * DE - the address of the byte following the last byte of the string.
;
; DESTROYED:
;   REGISTERS
;   * AF, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
GETIMS
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.MOVE ===
; 
;   Move graphics cursor.
;
;@doc:end
;------------------------------------------------------------------------------- 
MOVE:
	ld hl,4
	jp PLOT.CommandInHL

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.PLOT ===
; 
;   Plot a shape.
;
;@doc:end
;------------------------------------------------------------------------------- 
PLOT
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
	call VDU.WriteWord
	
	ld hl,(VDUQ(1, 5)) ; X
	call VDU.WriteWord
	
	ld hl,(VDUQ(3, 5)) ; Y
	call VDU.WriteWord
	
	jp Basic.BBCBASIC_XEQ

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.ENVEL ===
; 
;   Define a pitch and amplitude envelope.
;
;@doc:end
;------------------------------------------------------------------------------- 
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

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.SOUND ===
; 
;   Make a sound.
;
;@doc:end
;------------------------------------------------------------------------------- 
SOUND:
	
	call Basic.BBCBASIC_EXPRI ;Get first parameter (channel)
	exx
	
	ld a,h
	
	cp $20
	jr z,Sorry ; &20xx, word, library, 0 = Watford Speech
	
	and $F0
	jr z,SOUND.Internal
	cp $10
	jr z,SOUND.Internal
	
	ld a,h
	
	inc a
	jr z,Sorry ; &FFxx, command, 0, 0 = Speech system
	
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

SORRY:
	xor a
	call Basic.BBCBASIC_EXTERR
	.db "Sorry",0

DeviceFault:
	ld a,202
	call Basic.BBCBASIC_EXTERR
	.db "Device fault", 0

ReportError
	ld a,(hl)
	inc hl
	push hl
	jp Basic.BBCBASIC_EXTERR

OSBYTE:

	; A = OSBYTE routine
	; L = low byte of the parameter (X)
	; H = high byte of the parameter (Y)
	
	; Routines &A6..&FF take the form:
	; New Value = (<old value> AND H) EOR L
	; Return old value in L.

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

OSBYTE.CursorEditing:
	di
	ld a,(VDU.Console.Flags)

	bit 0,l ; New value
	ld l,a  ; Old value
	jr z,OSBYTE.CursorEditingEnable

OSBYTE.CursorEditingDisable:
	set VDU.Console.CursorEditingDisabled,a
	jr OSBYTE.CursorEditingReturn
	
OSBYTE.CursorEditingEnable:
	res VDU.Console.CursorEditingDisabled,a
	
OSBYTE.CursorEditingReturn:
	ld (VDU.Console.Flags),a
	
	bit VDU.Console.CursorEditingDisabled,l
	ld l,0
	jr z,+
	inc l
+:
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
	call VDU.Graphics.DivideBy5
	call VDU.Graphics.DivideBy5
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


; ---------------------------------------------------------
; OSBYTE.ModifyMemory -> Modifies memory based on OSBYTE
; routine number and H,L values.
; ---------------------------------------------------------
; Inputs:   de = pointer to memory to modify MINUS the
;                routine number.
;           a = OSBYTE routine number.
;           l = new value to EOR over value in memory.
;           h = mask to AND over the existing value.
; Outputs:  l = old value.
;           h = value of next value in memory.
; Destroys: None.
; ---------------------------------------------------------
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

; ---------------------------------------------------------
; OSBYTE.ModifyFlag -> Modifies a single flag value.
; ---------------------------------------------------------
; Inputs:   de = address of the flag.
;           c = bitmask of the flag.
;           l = new value to EOR over value in memory.
;           h = mask to AND over the existing value.
; Outputs:  l = old value.
; Destroys: None.
; ---------------------------------------------------------
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

OSWORD:
	ret

; Big list of file-system "todo"s...
OSFSC:
OSFIND:
OSGBPB:
OSOPEN:
OSARGS:
OSFILE:
OSBGET:
OSBPUT:
OSSTAT:
GETPTR:
PUTPTR:
GETEXT:
	jp SORRY

.endmodule