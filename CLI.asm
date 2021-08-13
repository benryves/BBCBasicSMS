.module CLI

TempPtr = PCLink2.TempPtr

; ---------------------------------------------------------
; Execute -> Executes the *COMMAND.
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
; Destroys: Everything.
; ---------------------------------------------------------
Execute:
	call SkipWhitespace
	ld a,(hl)
	cp '|'
	ret z
	ld de,Commands
	call DispatchCommand
	ret c
BadCommand:
	ld a,254
	call Basic.BBCBASIC_EXTERR
	.db Tokens.Bad, "command", 0
TooBig:
	ld a,20
	call Basic.BBCBASIC_EXTERR
	.db "Too big", 0
NoRoom:
	xor a
	call Basic.BBCBASIC_EXTERR
	.db Tokens.No, "room", 0

; ---------------------------------------------------------
; SkipWhitespace -> Skip past the whitespace in a command.
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
; Outputs:  hl = pointer to next character.
;           z = if at end of the command, nz if not.
; Destroys: af.
; ---------------------------------------------------------
SkipWhitespace:
	ld a,(hl)
	or a
	ret z
	cp '\r'
	ret z
	cp ' '
	jr z,+
	cp '\t'
	jr z,+
	ret
+:	inc hl
	jr SkipWhitespace

; ---------------------------------------------------------
; SkipWhitespaceAndComma -> Skip past whitespace and a
; single optional comma in a command. This is useful for
; *FX for example as arguments can take the form
; *FX 1,2 or *FX 1 2.
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
; Outputs:  hl = pointer to next character.
;           z = if at end of the command, nz if not.
; Destroys: af.
; ---------------------------------------------------------
SkipWhitespaceAndComma:
	call SkipWhitespace
	ret z
	; Fall-through

; ---------------------------------------------------------
; SkipComma -> Skip past a single comma in a command.
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
; Outputs:  hl = pointer to next character.
;           nz = set.
; Destroys: af.
; ---------------------------------------------------------
	ld a,(hl)
	cp ','
	ret nz
	inc hl
	or a ; Ensure NZ as we're not at the end of the statement.
	ret

; ---------------------------------------------------------
; CheckCommandEnd -> Ensure we've reached the end of the
;                    command, raise an error if not.
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
; Destroys: af.
; ---------------------------------------------------------
CheckCommandEnd:
	call SkipWhitespace
	or a
	scf
	ret z
	cp '\r'
	scf
	ret z
	jp BadCommand

; ---------------------------------------------------------
; CheckCommandInteractive -> Check that the command is
;     being run interactively and not as part of a BASIC
;     program.
; ---------------------------------------------------------
; Inputs:   iy = Program pointer.
; Destroys: af.
; ---------------------------------------------------------
CheckCommandInteractive:
	ld a,iyh
	cp Basic.BBCBASIC_BUFFER >> 8
	ret z
	jp BadCommand
	
; ---------------------------------------------------------
; DispatchCommand -> Execute a command based on its name.
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
;           de = pointer to command table.
; Outputs:  hl = pointer to next character in the command.
; Destroys: af.
; ---------------------------------------------------------
DispatchCommand:
	; Have we reached the end of the command?
	ld a,(de)
	or a
	ret z
	
	push hl
	push de
	
	; A = length of the command block.
	ld c,a
	sub 3
	ld b,a
-:	inc de
	ld a,(de)
	xor (hl)
	and %11011111
	jr nz,DispatchCommandNoMatch
	inc hl
	djnz -
	
	; The command name matches!
	pop af ; \_ We won't be needing these any more.
	pop af ; /
	
	inc de
	ld a,(de)
	ld c,a
	inc de
	ld a,(de)
	ld b,a
	
	; JP (BC)
	push bc
	ret

DispatchCommandNoMatch:

	; Try the next command in the list.
	pop hl
	ld b,0
	add hl,bc
	ex de,hl
	pop hl
	jr DispatchCommand

.function osclicommand(name, target)
	.db strlength(name) + 3
	.db name
	.dw target
.endfunction

; ---------------------------------------------------------
; GetDecimalWord -> Gets a value between 0..65565.
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
; Outputs:  hl = pointer to next character in the command.
;           de = parsed value.
; Destroys: af, de.
; ---------------------------------------------------------
GetDecimalWord:
	ld de,0
	call SkipWhitespace
	ld a,(hl)
	or a
	jp z,BadCommand
	cp '\r'
	jp z,BadCommand
	cp ' '
	jp z,BadCommand
	cp ','
	jp z,BadCommand

-:	ld a,(hl)
	or a
	ret z
	cp '\r'
	ret z
	cp ' '
	ret z
	cp ','
	ret z
	
	; Parse the value.
	cp '0'
	jp c,BadCommand
	cp '9'*1+1
	jp nc,BadCommand
	sub '0'
	
	inc hl
	
	ex de,hl

	push bc
	
	; Multiply existing value by 10.
	add hl,hl
	jp c,TooBig
	ld c,l \ ld b,h
	add hl,hl
	jp c,TooBig
	add hl,hl
	jp c,TooBig
	add hl,bc
	jp c,TooBig
	
	; Add the parsed value.
	ld b,0 \ ld c,a
	add hl,bc
	jp c,TooBig
	
	ex de,hl
	
	pop bc
	
	jr -

; ---------------------------------------------------------
; GetDecimalWord -> Gets a value between 0..255
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
; Outputs:  hl = pointer to next character in the command.
;           a  = parsed value.
; Destroys: af.
; ---------------------------------------------------------
GetDecimalByte:
	push de
	call GetDecimalWord
	ld a,d
	or a
	jp nz,TooBig
	ld a,e
	pop de
	ret

Terminal:
	call CheckCommandEnd
	ld hl,SerialTerminal
	call VDU.PutString
	
	call VDU.Console.BeginBlinkingCursor

Terminal.Loop:
	call Serial.GetByte
	jr z,Terminal.GotByte
	
	ei
	halt
	call Host.TrapFileTransfers
	jr c,Terminal.Loop
	
	call VDU.Console.EndBlinkingCursor
	scf
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
	
	jr Terminal.Loop

SerialTerminal:
	.db "Testing serial port...\r", 0

Catalogue:
	call SkipWhitespace
	
	; Is there an argument?
	ld a,(hl)
	or a
	jr z,Catalogue.NoArgument
	cp '\r'
	jr nz,Catalogue.FoundArgument
Catalogue.NoArgument:
	; List devices if there is no arguments.
	push ix
	push hl
	ld ix,Catalogue.PrintDirectoryName
	call PCLink2.ListDevices
	pop hl
	pop ix
	jp nz,Host.DeviceFault
	scf
	ret
	
Catalogue.FoundArgument:
	
	; Start by listing directories.
	push ix
	push hl
	ld ix,Catalogue.PrintDirectoryName
	call PCLink2.ListDirectories
	pop hl
	pop ix
	jp nz,Host.DeviceFault
	
	; Then list files.
	push ix
	push hl
	ld ix,Catalogue.PrintFileName
	call PCLink2.ListFiles
	pop hl
	pop ix
	jp nz,Host.DeviceFault
	
	; We have success!
	scf
	ret

Catalogue.PrintStartOfDirectoryName:
	call Catalogue.PrintStartOfItem
	ld a,'['
	jp VDU.PutChar

Catalogue.PrintDirectoryName:
	cp PCLink2.ListItems.StartOfItem
	jr z,Catalogue.PrintStartOfDirectoryName
	cp PCLink2.ListItems.EndOfItem
	jr z,Catalogue.PrintEndOfDirectoryName
	; Fall-through 
Catalogue.PrintFileName:
	cp PCLink2.ListItems.StartOfItem
	jr z,Catalogue.PrintStartOfItem
	cp PCLink2.ListItems.EndOfItem
	jr z,Catalogue.PrintEndOfItem
	cp PCLink2.ListItems.EndOfList
	jr z,Catalogue.PrintEndOfList
	
	; It must be a regular character.
	jp VDU.PutChar

Catalogue.PrintStartOfItem:
	ld a,(VDU.Console.MinCol)
	ld c,a
	ld a,(VDU.Console.MaxCol)
	sub c
	sub 18 
	jr nc,+
	xor a
+:	ld c,a ; C = maximum cursor column relative to viewport.
	
	ld a,(VDU.Console.MinCol)
	ld b,a
	ld a,(VDU.Console.CurCol)
	sub b
	ld b,a ; B = current cursor column relative to the vieport.
	
	xor a
-:	cp b
	jr nc,+
	add a,18
	cp c
	jr c,-
	; We've moved back to the left column, so trigger a newline.
	call VDU.Console.NewLine
	xor a
+:	
	; A = our new desired cursor column relative to the viewport.
	push bc
	ld b,a
	ld a,(VDU.Console.MinCol)
	add a,b
	ld (VDU.Console.CurCol),a
	pop bc
	
	ld a,' '
	call VDU.PutChar
	call VDU.PutChar
	
	ret
	
Catalogue.PrintEndOfDirectoryName:
	ld a,']'
	call VDU.PutChar
	; Fall-through.
Catalogue.PrintEndOfItem:
	ret

Catalogue.PrintEndOfList:
	jp VDU.Console.NewLine


Edit:
	; We cannot use *EDIT inside a BASIC program!
	call CheckCommandInteractive
	; Is there enough room to edit the line?
	push hl
	push de
	ld hl,0
	ld de,(Basic.BBCBASIC_FREE)
	or a
	sbc hl,de	
	add hl,sp
	ld de,384
	or a
	sbc hl,de
	jp c,NoRoom
	pop de
	pop hl
	
	; Check there's a valid line number after *EDIT
	push hl
	call GetDecimalWord
	call CheckCommandEnd
	pop hl
	
	; Scrolling uses BASIC's free memory, so ensure that we're not about to scroll.
	call VDU.Console.FlushPendingScroll
	
	; Store the *EDIT line number in BASIC's free memory.
	ld de,(Basic.BBCBASIC_FREE)
	ld (TempPtr),de
-:	ld a,(hl)
	ldi
	cp '\r'
	jr nz,-
	
	; Override OSLINE with our "L.<line>" routine.
	ld hl,Edit.OSLine.List
	ld (Host.OSLINE.Override),hl
	scf
	ret

Edit.OSLINE.List:
	; Pretend we typed "L."
	ld (hl),'L'
	inc hl
	ld (hl),'.'
	inc hl
	
	; Copy the line number
	ex de,hl
	ld hl,(Basic.BBCBASIC_FREE)
-:	ld a,(hl)
	ldi
	cp '\r'
	jr nz,-
	
	; We'll want to capture the LISTed program line.
	ld hl,Edit.OSWRCH.List.FirstChar
	ld (Host.OSWRCH.Override),hl
	
	; We want the next OSLINE to edit the captured LISTed line.
	ld hl,Edit.OSLINE.Edit
	ld (Host.OSLINE.Override),hl
	
	xor a
	ret

Edit.OSWRCH.List.FirstChar:	
	cp '>' ; is it the prompt?
	jr nz,+

	; If the first character of the LISTed line is the > prompt, there is no line to edit.
	push hl
	ld hl,0
	ld (Host.OSWRCH.Override),hl
	ld (Host.OSLINE.Override),hl
	pop hl
	ret

+:	push hl
	ld hl,Edit.OSWRCH.List
	ld (Host.OSWRCH.Override),hl
	pop hl
Edit.OSWRCH.List:
	push hl
	ld hl,(TempPtr)
	ld (hl),a
	inc hl
	ld (TempPtr),hl
	pop hl
	ret

Edit.OSLINE.Edit:
	; Now, we finally perform our editing duties!
	ld de,0
	ld (Host.OSLINE.Override),de
	ld (Host.OSWRCH.Override),de
	
	push hl
	
	; Make sure our captured line always ends in a CR.
	ld hl,(TempPtr)
	ld (hl),'\r'
	
	pop de
	push de
	
	; Read LISTed line from memory.
	ld hl,(Basic.BBCBASIC_FREE)
	call SkipWhitespace
-:	ld a,(hl)
	ldi
	cp '\r'
	jr nz,-
	
	; Now edit the line.
	pop hl
	jp Host.OSLINE.Prefilled

Serial:
	call SkipWhitespace
	
	ld de,Serial.Commands
	call DispatchCommand
	
	jp nc,BadCommand
	
	call SkipWhitespace
	cp ','
	jp nz,CheckCommandEnd
	inc hl
	jr Serial

Serial.BaudRate:
	call SkipWhitespace
	cp '='
	jr nz,+
	inc hl
	call SkipWhitespace
+:	call GetDecimalWord
	push hl
	ld l,e
	ld h,d
	call Serial.SetRate
	pop hl
	scf
	ret z
	or a
	ret

Serial.Commands:
	osclicommand("BAUDRATE", Serial.BaudRate)
	osclicommand("BAUD", Serial.BaudRate)
	.db 0

; *FX A     = OSBYTE A, L=0, H=0
; *FX A,L   = OSBYTE A, L, H=0
; *FX A,L,H = OSBYTE A, L, H
FX:
	
	; Get the first argument.
	call GetDecimalByte
	push af
	ld de,0
	call SkipWhitespaceAndComma
	jr z,FXGotArguments
	
	; Get the second argument.
	call GetDecimalByte
	ld e,a
	call SkipWhitespaceAndComma
	jr z,FXGotArguments
	
	; Get the third argument.
	call GetDecimalByte
	ld d,a
	
FXGotArguments:
	
	; Ensure there's nothing further to handle.
	call CheckCommandEnd
	
	pop af
	ex de,hl
	
	; *FX A,HL
	call Host.OSBYTE
	
	scf
	ret

Tape:

	call SkipWhitespace
	ld a,(hl)
	and %11011111
	cp 'B'
	jr z,TapeBlock
	push af
	
	ld hl,Basic.BBCBASIC_BUFFER
	
-:	call Tape.GetByte
	jr z,-
	ld (hl),a
	inc l
	jr nz,-

DoneTape:

	pop af
	cp 'T'
	ld hl,Basic.BBCBASIC_BUFFER
	
	jr z,Tape.ShowText
	

--:	ld b,12	
-:	ld a,(hl)
	call VDU.PutHexByte
	ld a, ' '
	call VDU.PutChar
	inc l
	jr z,FinishedTapeDump
	djnz -
	call VDU.Console.NewLine
	jr --
	jr FinishedTapeDump

Tape.ShowText:
	
-:	ld a,(hl)
	call VDU.PutLiteralChar
	inc l
	jr z,FinishedTapeDump
	jr -

FinishedTapeDump:
	call VDU.Console.NewLine
	
	ld hl,0
	ld de,Basic.BBCBASIC_BUFFER
	
-:	ld a,(de)
	ld c,a
	ld b,0
	add hl,bc
	inc e
	jr nz,-
	
	call VDU.PutHexWord
	call VDU.Console.NewLine
	
	ld hl,Basic.BBCBASIC_BUFFER
	ld (hl),'\r'
	
	scf
	ret

TapeBlock:

	push ix

TapeBlockLoop:

	ld de,(Basic.BBCBASIC_FREE)
	ld hl,256
	add hl,de
	
	call Tape.GetBlock
	scf
	jp z,Host.DeviceFault
	
	ld hl,(Basic.BBCBASIC_FREE)
	inc h
	call VDU.PutString
	
	ld a,' '
	call VDU.PutChar
	
	ld l,(ix+Tape.Header.BlockNumber+0)
	ld h,(ix+Tape.Header.BlockNumber+1)
	call VDU.PutDecimalWord
	
	ld a,' '
	call VDU.PutChar
	
	ld l,(ix+Tape.Header.CRC+0)
	ld h,(ix+Tape.Header.CRC+1)
	call VDU.PutHexWord
	call VDU.Console.NewLine
	
	ld a,(ix+Tape.Header.BlockFlag)
	add a,a
	jr nc,TapeBlockLoop
	
	pop ix
	
	scf
	ret

Commands:
	osclicommand("TERM", Terminal)
	osclicommand("CAT", Catalogue)
	osclicommand("DIR", Catalogue)
	osclicommand(".", Catalogue)
	osclicommand("EDIT", Edit)
	osclicommand("SERIAL", Serial)
	osclicommand("FX", FX)
	osclicommand("TAPE", Tape
	.db 0

.endmodule