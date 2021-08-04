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

Flags = allocVar(1)
Pause = 0
Overwrite = 1

TrapKeyboardTimer = allocVar(1)

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
	
	ld hl,0
	ld (TIME+0),hl
	ld (TIME+2),hl
	
	ld (OSLINE.Override),hl
	ld (OSWRCH.Override),hl
	
	xor a
	ld (TrapKeyboardTimer),a
	ld (Flags),a
	
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
	push hl
	ld hl,EmptyChar
-:	call ReadChar
	jp m,-
	pop hl
	ret

EmptyChar:
	.db ' '

ReadChar:
	push bc
	push hl
	
	call InitKeyLoop

-:	call RunKeyLoop
	jr c,-
	jp p,ReadCharPrintable
	
	push af
	
	cp Keyboard.KeyCode.Insert
	jr nz,+
	ld a,(Flags)
	xor 1<<Overwrite
	ld (Flags),a
	
	pop af
	jr -

+:	pop af

ReadCharPrintable:
	call EndKeyLoop
	
	pop hl
	pop bc
	ei
	ret

InitKeyLoop:
	ld a,(TIME)
	ld b,a
	ld c,1
	ret

RunKeyLoop:
	push bc
	
	; If C has been modified to 0, it indicates we want to skip the cursor on the initial loop.
	ld a,c
	or a
	jr nz,+
	pop bc
	ld c,255
	push bc
	jr NoFlashCursor
+:
	
	; Is the cursor flashing?
	ld a,(TIME)
	sub b
	and %00100000
	jr nz,CursorNotVisible
	
	; What is the cursor?
	ld a,(Flags)
	bit Overwrite,a
	ld a,'_' ; Insert mode cursor
	jr z,ShowKeyCursor
	ld a,127 ; Overwrite mode cursor 
	jr ShowKeyCursor

CursorNotVisible:
	ld a,(hl) ; Current character
ShowKeyCursor:
	cp c
	jr z,NoFlashCursor
	pop bc
	ld c,a
	push bc
	push af
	
	ei
	halt
	
	call TrapConsoleButtons
	call VDU.Console.FlushPendingScroll
	
	pop af
	cp '\r'
	jr nz,+
	ld a,' '
+:	call VDU.PutMap

NoFlashCursor:
	call Keyboard.GetKey
	
	pop bc
	ei
	jr nz,NoKey   ; no key at all
	jr c,NoKey    ; key released
	jp p,GotKey   ; printable key
	
	push af
	cp Keyboard.KeyCode.Escape
	jr z,KeyLoopEscape
	pop af
	scf
	ccf
	ret

KeyLoopEscape:
	call EndKeyLoop
	pop af
	ld a,17 ; Escape
	call Basic.BBCBASIC_EXTERR
	.db "Escape", 0

NoKey:
	scf
	ret

GotKey:
	scf
	ccf
	ret

EndKeyLoop:
	push af
	ld a,c
	or a
	jr z,++
	inc a
	jr z,++
	ld a,(hl)
	cp '\r'
	jr nz,+
	ld a,' '
+:	call VDU.PutMap
++:	pop af
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
	
	; Calculate the end time-1 (easier to check for <0 instead of <=0 later)
	ld de,(TIME)
	dec de
	add hl,de
	
	call InitKeyLoop

	ld c,0 ; Skip the flashing cursor on the first loop.
	
-:	push hl
	ld hl,EmptyChar
	call RunKeyLoop
	pop hl
	jr nz,+ ; No keys at all.
	jr c,+  ; Released key.
	jp p,GotTimedKey  ; It's a printable key.
	
	cp Keyboard.KeyCode.Insert
	jr nz,+
	
	; Toggle insert/overwrite.
	ld a,(Flags)
	xor 1<<Overwrite
	ld (Flags),a
	
	; Has the timer expired?
+:	push hl
	ld de,(TIME)
	or a
	sbc hl,de
	ex de,hl
	pop hl
	
	; Check if timer has gone negative.
	bit 7,d
	jr z,-

KeyTimedOut:
	scf ; Timed out, so pretend we dont have a key.
	
GotTImedKey:
	ccf
	
	push hl
	ld hl,EmptyChar
	call EndKeyLoop
	pop hl
	
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

+:	call InitKeyLoop
	
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
	call ReadChar
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

	; Check if it's in the range 32..127.
	cp 32
	jr c,OSLINE.Loop
	cp 128
	jr nc,OSLINE.Loop
	
	; Store the character in E.
	ld e,a
	
	; Is this going to be an insert or overwrite command?
	ld a,(Flags)
	bit Overwrite,a
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
	
	; Print the character.
	call VDU.PutChar
	jr OSLINE.Loop

OSLINE.InsertCharacter
	; Check if we've got enough space.
	ld a,c
	or a
	jr z,OSLINE.Loop

OSLINE.EnoughSpace:	
	
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
	jp z,OSLINE.Loop
	
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
	ld a,' '
+:	or a
	call nz,VDU.PutMap
	
	; Move the cursor back to where it should be.
	pop bc
-:	call VDU.Console.CursorLeft
	djnz -
	
	pop hl
	pop bc
	jp OSLINE.Loop

OSLINE.Return:
	
	; Move to the end.
	call OSLINE.End
	
	; Finish up and return.
	call EndKeyLoop
	
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
	
	call VDU.Console.CursorRight
	inc hl
	inc d
	
	; Run-on to Backspace handler.

OSLINE.Backspace:
	; Are we at the start of the string? (Offset from start = 0)?
	ld a,d
	or a
	jp z,OSLINE.Loop
	
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
	call VDU.Console.CursorLeft
	
	dec hl
	dec d

	ld e,' ' ; Blank off the end of the backspaced line.
	jp OSLINE.RepaintToEnd

OSLINE.Home:
	push af
	; Are we already at the start?
	ld a,d
	or a
	jr z,+

-:	call VDU.Console.CursorLeft
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

-:	call VDU.Console.CursorRight
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
	
	call VDU.Console.CursorLeft
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
	
	call VDU.Console.CursorRight
	inc hl
	inc d
	
+:	pop af
	ret

OSLINE.Clear:
	
	; Is the line already clear?
	ld a,b
	or a
	jp z,OSLINE.Loop
	
	; Move to the end of the line.
	sub d
	jr z,+
	
-:	push af
	call VDU.Console.CursorRight
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
	call VDU.Console.CursorLeft
	ld a,' '
	call VDU.PutMap
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
	call InitKeyLoop
	ld bc,255
	ld d,0
-:	ld a,(hl)
	cp '\r'
	jp z,OSLINE.Loop
	call VDU.PutChar
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
	call TrapConsoleButtons
	ei
	
	ld a,(TrapKeyboardTimer)
	or a
	ret nz
	ld a,20
	ld (TrapKeyboardTimer),a
	
	push bc
	push de
	push hl
	
	call Keyboard.GetKey
	
	jr nz,+ ; No key
	jr c,+  ; It's being released
	jp p,+  ; It's a printable key
	
	cp Keyboard.KeyCode.Escape
	jr nz,+
	
	ld a,17 ; Escape
	call Basic.BBCBASIC_EXTERR
	.db "Escape", 0
	ret
	
+:	pop hl
	pop de
	pop bc
	ret

TrapConsoleButtons:
	; Is reset pressed?
	in a,($DD)
	bit 4,a
	jp z,$0000
	
	; Is pause pressed?
	ld a,(Flags)
	bit Pause,a
	ret z
	
	res Pause,a
	ld (Flags),a
	
	ld a,17 ; Escape
	call Basic.BBCBASIC_EXTERR
	.db "Escape", 0
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
	ld a,(Flags)
	res Pause,a
	ld (Flags),a
	
	push hl
	push de
	push bc
	
	call Sound.Silence
	
	call Video.SynchroniseRegisters
	
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
OSLOAD
	call PCLink2.GetFile
	jp nz,Sorry
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
OSSAVE
	call PCLink2.SendFile
	jp nz,Sorry
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSOPEN ===
; 
;   Open a file for reading or writing.
;
; INPUTS:
;   REGISTERS
;   * HL - addresses a file descriptor (filename) terminated by CR.
;   * AF - A=-1, NZ, NC = OPENOUT
;        - A=0, Z, C = OPENIN
;        - A=1, NZ, C = OPENUP
;
; OUTPUTS:
;   * A  - is the file "handle" (channel number) allocated, or 0 on error.
;
; DESTROYED:
;   REGISTERS
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
OSOPEN
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSBGET ===
; 
;   Read a single byte from an open file.
;
; INPUTS:
;   REGISTERS
;   * E  - file handle (channel number). Zero constitutes an error.
;
; OUTPUTS:
;   * A  - is the byte read from the file.
;
; DESTROYED:
;   REGISTERS
;   * AF, BC
;
;@doc:end
;------------------------------------------------------------------------------- 
OSBGET
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSBPUT ===
; 
;   Write a single byte to an open file.
;
; INPUTS:
;   REGISTERS
;   * E  - file handle (channel number). Zero constitutes an error.
;   * A  - is the byte to be written to the file.
;
; DESTROYED:
;   REGISTERS
;   * AF, BC
;
;@doc:end
;------------------------------------------------------------------------------- 
OSBPUT
	jp SORRY

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
OSCALL
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSSTAT ===
; 
;   Read the status of an open file.
;
; INPUTS:
;   REGISTERS
;   * E  - file handle (channel number). Zero constitutes an error.
;
; OUTPUTS:
;   REGISTERS
;   * F  - 	If at the end-of-file, return Z (zero flag set).
;
; DESTROYED:
;   REGISTERS
;   * AF, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
OSSTAT
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.GETPTR ===
; 
;   Read the sequential pointer of an open file.
;
; INPUTS:
;   REGISTERS
;   * E  - file handle (channel number). Zero constitutes an error.
;
; OUTPUTS:
;   REGISTERS
;   * DEHL - 32-bit pointer (zero corresponds to the first byte in the file).
;
; DESTROYED:
;   REGISTERS
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
GETPTR
	jp SORRY
	
;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.PUTPTR ===
; 
;   Update the sequential pointer of an open file.
;
; INPUTS:
;   REGISTERS
;   * A  - file handle (channel number). Zero constitutes an error.
;   * DEHL - 32-bit pointer (zero corresponds to the first byte in the file).
;
; DESTROYED:
;   REGISTERS
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
PUTPTR
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.GETEXT ===
; 
;   Return the length of an open file.
;
; INPUTS:
;   REGISTERS
;   * E  - file handle (channel number). Zero constitutes an error.
;
; OUTPUTS:
;   REGISTERS
;   * DEHL - 32-bit pointer (zero corresponds to the first byte in the file).
;
; DESTROYED:
;   REGISTERS
;   * AF, BC, DE, HL
;
;@doc:end
;------------------------------------------------------------------------------- 
GETEXT
	jp SORRY

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
OSSHUT
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

ReportError
	ld a,(hl)
	inc hl
	push hl
	jp Basic.BBCBASIC_EXTERR

.endmodule