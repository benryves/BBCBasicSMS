.module CLI
; ==========================================================================
; Execute
; --------------------------------------------------------------------------
; Executes the *COMMAND.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to command line.
; Destroyed:  Everything.
; ==========================================================================
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

; ==========================================================================
; SkipWhitespace
; --------------------------------------------------------------------------
; Skip past the whitespace in a command.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to command line.
; Outputs:    HL: Pointer to next character.
;             F: Z = if at end of the command, NZ if not.
; Destroyed:  AF.
; ==========================================================================
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

; ==========================================================================
; SkipWhitespaceAndComma
; --------------------------------------------------------------------------
; Skip past whitespace and a single optional comma in a command.
; This is useful for *FX for example as arguments can take the form *FX 1,2
; or *FX 1 2.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to command line.
; Outputs:    HL: Pointer to next character.
;             F: Z = if at end of the command, NZ if not.
; Destroyed:  AF.
; ==========================================================================
SkipWhitespaceAndComma:
	call SkipWhitespace
	ret z
	; Fall-through to SkipComma

; ==========================================================================
; SkipComma
; --------------------------------------------------------------------------
; Skip past a single comma in a command.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to command line.
; Outputs:    HL: Pointer to next character.
;             F: NZ set.
; Destroyed:  AF.
; ==========================================================================
SkipComma:
	ld a,(hl)
	cp ','
	ret nz
	inc hl
	or a ; Ensure NZ as we're not at the end of the statement.
	ret

; ==========================================================================
; CheckCommandEnd
; --------------------------------------------------------------------------
; Ensure we've reached the end of the command, raise an error if not.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to command line.
; Destroyed:  AF.
; ==========================================================================
CheckCommandEnd:
	call SkipWhitespace
	or a
	scf
	ret z
	cp '\r'
	scf
	ret z
	jp BadCommand

; ==========================================================================
; CheckCommandInteractive
; --------------------------------------------------------------------------
; Check that the command is being run interactively and not as part of a
; BASIC program.
; --------------------------------------------------------------------------
; Inputs:     IY: Program pointer.
; Destroyed:  AF.
; ==========================================================================
CheckCommandInteractive:
	ld a,iyh
	cp Basic.BBCBASIC_BUFFER >> 8
	ret z
	jp BadCommand
	
; ==========================================================================
; DispatchCommand
; --------------------------------------------------------------------------
; Execute a command based on its name.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to command line.
;             DE: Pointer to command table.
; Outputs:    HL: Pointer to next character in the command.
; Destroyed:  AF.
; ==========================================================================
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

; ==========================================================================
; GetDecimalWord
; --------------------------------------------------------------------------
; Gets a value between 0..65565.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to command line.
; Outputs:    HL: Pointer to next character in the command.
;             DE: Parsed value.
; Destroyed:  AF.
; ==========================================================================
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

; ==========================================================================
; GetDecimalWord
; --------------------------------------------------------------------------
; Gets a value between 0..6556255.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to command line.
; Outputs:    HL: Pointer to next character in the command.
;             A: Parsed value.
; Destroyed:  AF.
; ==========================================================================
GetDecimalByte:
	push de
	call GetDecimalWord
	ld a,d
	or a
	jp nz,TooBig
	ld a,e
	pop de
	ret

; ==========================================================================
; GetOnOff
; --------------------------------------------------------------------------
; Gets a value that is either "ON" or "OFF".
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to command line.
; Outputs:    HL: Pointer to next character in the command.
;             F: Carry set if "on", carry reset if  "off".
; Destroyed:  AF.
; ==========================================================================
GetOnOff:
	call SkipWhitespace
	ld a,(hl)
	inc hl
	and %11011111
	cp 'O'
	jp nz,BadCommand
	ld a,(hl)
	inc hl
	and %11011111
	cp 'F'
	jr z,GetOnOff.Off
	cp 'N'
	jp nz,BadCommand

GetOnOff.On:
	scf
	ret

GetOnOff.Off:
	ld a,(hl)
	inc hl
	and %11011111
	cp 'F'
	jp nz,BadCommand
	or a
	ret

Terminal:
	call CheckCommandEnd
	ld hl,SerialTerminal
	.bcall "VDU.PutString"
	
	.bcall "VDU.Console.BeginBlinkingCursor"

Terminal.Loop:
	call Serial.GetByte
	jr z,Terminal.GotByte
	
	ei
	halt
	call Host.TrapFileTransfers
	jr c,Terminal.Loop
	
	.bcall "VDU.Console.EndBlinkingCursor"
	scf
	ret

Terminal.GotByte:
	
	push af
	call Serial.SendByte
	pop af
	
	push af
	.bcall "VDU.PutChar"
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
	
	ld a,(Host.Flags)
	and 1<<Host.TapeFS
	jp nz,Catalogue.Tape
	
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
	.bcall "VDU.PutChar"
	ret

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
	.bcall "VDU.PutChar"
	ret

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
	.bcall "VDU.Console.NewLine"
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
	.bcall "VDU.PutChar"
	.bcall "VDU.PutChar"
	
	ret
	
Catalogue.PrintEndOfDirectoryName:
	ld a,']'
	.bcall "VDU.PutChar"
	; Fall-through.
Catalogue.PrintEndOfItem:
	ret

Catalogue.PrintEndOfList:
	.bcall "VDU.Console.NewLine"
	ret

Catalogue.Tape:
	call Tape.Catalogue
	scf
	ret

Edit:
	; We cannot use *EDIT inside a BASIC program!
	call CheckCommandInteractive
	; Is there enough room to edit the line?
	push hl
	push de
	ld hl,0
	ld de,384
	call Host.GetSafeScratchMemoryDE
	jp c,NoRoom
	ld (TempPtr),de
	pop de
	pop hl
	
	; Check there's a valid line number after *EDIT
	push hl
	call GetDecimalWord
	call CheckCommandEnd
	pop hl
	
	; Store the *EDIT line number in BASIC's free memory.
	ld de,(TempPtr)
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
	ld hl,(TempPtr)
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
	ld hl,384
	call Host.GetSafeScratchMemoryHL
	jp c,NoRoom
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
	ld a,(Host.Flags)
	or 1<<Host.TapeFS
	ld (Host.Flags),a
	
	
	ld a,(hl)
	or a
	jr z,+
	cp '\r'
	jr z,+
	
	ld de,TapeSubCommands
	call DispatchCommand
	
	call CheckCommandEnd
	
+:	scf
	ret

Tape300:
Tape1200:
	jp Host.Sorry

TapeSubcommands:
	osclicommand("3", Tape300)
	osclicommand("12", Tape1200)
	.db 0

PCLink2:
	ld a,(Host.Flags)
	and ~(1<<Host.TapeFS)
	ld (Host.Flags),a
	scf
	ret

Escape:
	call SkipWhitespace
	ld a,(hl)
	or a
	jr z,Escape.NoOnOff
	cp '\r'
	jr z,Escape.NoOnOff
	
	call GetOnOff
	jr Escape.GotOnOff
	
Escape.NoOnOff:
	scf
Escape.GotOnOff:

	push af
	call CheckCommandEnd
	pop af

	ld a,1
	sbc a,0
	ld l,a
	ld h,%11111110
	ld a,200
	call Host.OSBYTE
	scf
	ret

Commands:
	osclicommand("TERM", Terminal)
	osclicommand("CAT", Catalogue)
	osclicommand("DIR", Catalogue)
	osclicommand(".", Catalogue)
	osclicommand("EDIT", Edit)
	osclicommand("E.", Edit)
	osclicommand("SERIAL", Serial)
	osclicommand("FX", FX)
	osclicommand("TAPE", Tape)
	osclicommand("PCLINK", PCLink2)
	osclicommand("ESC", Escape)
	.db 0

.endmodule