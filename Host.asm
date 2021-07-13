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

TrapKeyboardTimer = allocVar(1)

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
OSINIT
	
	ld hl,0
	ld (TIME+0),hl
	ld (TIME+2),hl
	
	xor a
	ld (TrapKeyboardTimer),a
	ld (Flags),a
	
	ld de, $DFF0 ; HIMEM
	ld hl, $C400 ; PAGE
	
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
	push bc
	
	call InitKeyLoop

-:	call RunKeyLoop
	jr c,-

	call EndKeyLoop
	
	pop bc
	ei
	ret


InitKeyLoop:
	ld a,(TIME)
	ld b,a
	ld c,'X'
	ret

RunKeyLoop:
	push bc
	
	; If C has been modified to 0, it indicates we want to skip the cursor on the initial loop.
	ld a,c
	or a
	jr nz,+
	pop bc
	ld c,'X'
	push bc
	jr NoFlashCursor
+:
	
	ld a,(TIME)
	sub b
	and %00010000
	ld a,127
	jr z,+
	ld a,'_'
+:	cp c
	jr z,NoFlashCursor
	pop bc
	ld c,a
	push bc
	push af
	
	ei
	halt
	
	call TrapConsoleButtons
	pop af
	call VDU.PutMap

NoFlashCursor:
	call Keyboard.GetKey
	
	pop bc
	ei
	jr nz,NoKey   ; no key at all
	jr c,NoKey    ; key released
	jp p,GotKey   ; printable key
	
	cp Keyboard.KeyCode.Escape
	jr nz,NoKey
	
	ld a,17 ; Escape
	call Basic.BBCBASIC_EXTERR
	.db "Escape", 0

NoKey:
	scf
	ret

GotKey:
	or a
	ret

EndKeyLoop:
	push af
	ld a,c
	or a
	jr z,+
	ld a,' '
	call VDU.PutMap
+:	pop af
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

-:	call RunKeyLoop
	jr nc,GotTimedKey ; We have a key!
	
	; Has the timer expired?
	push hl
	ld de,(TIME)
	or a
	sbc hl,de
	ex de,hl
	pop hl
	
	; Check if timer has gone native.
	bit 7,d
	jr nz,KeyTimedOut

KeyTimedOut:
	scf ; Timed out, so pretend we dont 
	
GotTImedKey:
	ccf
	call EndKeyLoop
	
	pop de
	pop bc
	ei
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.OSLINE ===
;
;   Read a line, terminated by RETURN, from the Vdu.Text.
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
	ld bc,255 ; B = current length (0), C = maximum length (excluding \r terminator).

OSLINE.Loop:
	call OSRDCH
	
	cp '\b'
	jr z,OSLINE.Backspace
	cp 127
	jr z,OSLINE.Backspace

	ld e,a
	
	ld a,c
	or a
	jr nz,OSLINE.EnoughSpace
	
	; We're out of space... but there's always space for \r, so check
	ld a,e
	cp '\r'
	jr nz,OSLINE.Loop

OSLINE.EnoughSpace:	
	ld a,e
	ld (hl),a
	inc hl
	
	inc b
	dec c
	
	push af
	call OSWRCH
	pop af
	cp '\r'
	jr nz,OSLINE.Loop
	
	ld a,'\n'
	call OSWRCH
	xor a
	ret

OSLINE.Backspace:
	ld a,b
	or a
	jr z,OSLINE.Loop
	
	dec b
	inc c
	
	dec hl
	ld (hl),'\r'
	
	call VDU.CursorLeft
	
	jr OSLINE.Loop

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
	push af
	call VDU.PutChar
	pop af
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
	jp OSWRCH

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
	
	ld a,10
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
RESET
	ld a,(Flags)
	res Pause,a
	ld (Flags),a
	push bc
	call Video.SynchroniseRegisters
	ld a,' '
	call VDU.PutMap
	pop bc
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

	push de
	call PCLink2.GetFile
	pop hl
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
;@doc:end
;------------------------------------------------------------------------------- 
OSSAVE
	jp SORRY

------------------------------------------------------------------------------- 
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
;------------------------------------------------------------------------------- 
OSCALL
	ret
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
;------------------------------------------------------------------------------- 
OSCLI
	
	ld a,(hl)
	cp 'S'
	jp z,Sync
	cp 'H'
	jp z,Hello
	cp 'G'
	jp z,Goodbye
	cp 'V'
	jp z,ListDevices
	cp 'D'
	jp z,ListDirectories
	cp 'F'
	jp z,ListFiles
	cp 'T'
	jp z,Terminal
	
	ret

Sync:
	ld hl,Sync.Text
	call VDU.PutString
	call PCLink2.Sync
	ld hl,Sync.OK
	jr z,+
	ld hl,Sync.Failed
+:	call VDU.PutString
	ret

Sync.Text:
	.db "Sync...",0
Sync.OK:
	.db "OK\n",0
Sync.Failed:
	.db "Failed\n",0

Hello:
	ld hl,Hello.Text
	call VDU.PutString
	ld a,'?'
	call VDU.PutChar
	call VDU.NewLine
	
	call PCLink2.Hello
	ret nz

	ld hl,Hello.Text
	call VDU.PutString
	ld a,'!'
	call VDU.PutChar
	call VDU.NewLine
	ret
	
Hello.Text:
.db "Hello",0

Goodbye:
	ld hl,Goodbye.Text
	call VDU.PutString
	ld a,'?'
	call VDU.PutChar
	call VDU.NewLine
	
	call PCLink2.Goodbye
	ret nz
	
	ld hl,Goodbye.Text
	call VDU.PutString
	ld a,'!'
	call VDU.PutChar
	call VDU.NewLine
	ret
	
Goodbye.Text
.db "Goodbye",0

ListDevices:
	ld hl,ListDevices.Text
	call VDU.PutString
	call PCLink2.ListDevices
	ld hl,Sync.OK
	jr z,+
	ld hl,Sync.Failed
+:	call VDU.PutString
	ret
ListDevices.Text:
.db "List Devices...",0

ListDirectories:
	push hl
	ld hl,ListDirectories.Text
	call VDU.PutString
	pop hl
	inc hl
	call PCLink2.ListDirectories
	ld hl,Sync.OK
	jr z,+
	ld hl,Sync.Failed
+:	call VDU.PutString
	ret
ListDirectories.Text:
.db "List Directories...",0

ListFiles:
	push hl
	ld hl,ListFiles.Text
	call VDU.PutString
	pop hl
	inc hl
	call PCLink2.ListFiles
	ld hl,Sync.OK
	jr z,+
	ld hl,Sync.Failed
+:	call VDU.PutString
	ret
ListFiles.Text:
.db "List Files...",0

Terminal:
	ld hl,SerialTerminal
	call VDU.PutString

Terminal.Loop:
	ld a,127
	call VDU.PutMap
	call Serial.GetByte
	push af
	ld a,' '
	call VDU.PutMap
	pop af
	jr z,Terminal.GotByte
	
	ei
	halt

	in a,($DD)
	bit 4,a
	jr nz,Terminal.Loop
-:	in a,($DD)
	bit 4,a
	ret nz
	jr -
	ret

Terminal.GotByte:
	
	push af
	call Serial.SendByte
	pop af
	
	push af
	call VDU.PutChar
	pop af
	cp '\r'
	jr nz,Terminal.Loop
	
	ld a,'\n'
	call Serial.SendByte
	
	call VDU.NewLine
	
	jr Terminal.Loop

SerialTerminal:
	.db "Testing serial port...\n", 0

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.MODE ===
; 
;   Change screen mode.
;
;@doc:end
;------------------------------------------------------------------------------- 
MODE
	call Basic.BBCBASIC_EXPRI
    exx
	ld a,l
	
	; Check the mode number is in range
	cp VDU.Modes.Count
	jp nc,Basic.BBCBASIC_XEQ
	
	call VDU.SetMode
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
	ld a,d
	or a
	ret nz
	ld a,h
	or a
	ret nz
	
	ld a,(VDU.MinCol)
	ld d,a
	ld a,(VDU.MaxCol)
	sub d
	cp e
	ret z
	ret c
	
	ld a,(VDU.MinRow)
	ld h,a
	ld a,(VDU.MaxRow)
	sub h
	cp l
	ret z
	ret c
	
	ld a,(VDU.MinCol)
	add a,e
	ld (VDU.CurCol),a
	
	ld a,(VDU.MinRow)
	add a,l
	ld (VDU.CurRow),a
	
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
GETCSR
	ld de,(VDU.CurCol)
	ld d,0
	ld hl,(VDU.CurRow)
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
;------------------------------------------------------------------------------- 
PUTIME
	ld (TIME+0),hl
	ld (TIME+2),de
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
GETIME
	ld hl,(TIME+0)
	ld de,(TIME+2)
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
CLRSCN
	ret

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.ADVAL ===
; 
;   Analogue-to-digital conversion (mouse).
;
;@doc:end
;------------------------------------------------------------------------------- 
ADVAL
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.POINT ===
;
;   Read the colour of a screen pixel.
;
;@doc:end
;------------------------------------------------------------------------------- 
POINT
	jp SORRY

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.COLOUR ===
; 
;   Change text foreground or background colour.
;
;------------------------------------------------------------------------------- 
COLOUR
	call Basic.BBCBASIC_EXPRI
	exx
	ld a,l
	
	call VDU.SetTextColour
	jp Basic.BBCBASIC_XEQ

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.CLG ===
; 
;   Clear graphics window to graphics background colour.
;
;------------------------------------------------------------------------------- 
CLG
	jp SORRY


;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.DRAW ===
; 
;   Draw a line.
;
;------------------------------------------------------------------------------- 
DRAW
	call Basic.BBCBASIC_EXPRI
	exx
	push hl
	
	call Basic.BBCBASIC_COMMA
	call Basic.BBCBASIC_EXPRI
	exx
	
	pop de
	ex de,hl
	call VDU.EnqueueGraphicsCursor
	
	call VDU.PlotLine
	
	jp Basic.BBCBASIC_XEQ

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.GCOL ===
; 
;   Change graphics colour and plotting action
;
;@doc:end
;------------------------------------------------------------------------------- 
GCOL
	call Basic.BBCBASIC_EXPRI
	exx
	ld a,l
	
	call VDU.SetGraphicsColour
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
;------------------------------------------------------------------------------- 
MOVE
	call Basic.BBCBASIC_EXPRI
	exx
	push hl
	
	call Basic.BBCBASIC_COMMA
	call Basic.BBCBASIC_EXPRI
	exx
	
	pop de
	ex de,hl
	
	call VDU.EnqueueGraphicsCursor
	
	jp Basic.BBCBASIC_XEQ

;------------------------------------------------------------------------------- 
;@doc:routine 
; 
; === Host.PLOT ===
; 
;   Plot a shape.
;
;------------------------------------------------------------------------------- 
PLOT
	jp SORRY

SORRY
	xor a
	call Basic.BBCBASIC_EXTERR
	.db "Sorry",0

ReportError
	ld a,(hl)
	inc hl
	push hl
	jp Basic.BBCBASIC_EXTERR

.endmodule