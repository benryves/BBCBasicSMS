.module CLI

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

; ---------------------------------------------------------
; SkipWhitespace -> Skip past the whitespace in a command.
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
; Outputs:  hl = pointer to next character.
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
; CheckCommandEnd -> Ensure we've reached the end of the
;                    command, raise an error if not.
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
; Destroys: Everything.
; ---------------------------------------------------------
CheckCommandEnd:
	call SkipWhitespace
	or a
	ret z
	cp '\r'
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

Terminal:
	call CheckCommandEnd
	ld hl,SerialTerminal
	call VDU.PutString

Terminal.Loop:
	ld a,127
	call VDU.PutMap
	
	call Serial.GetByte
	push af
	
	ld a,' '
	call z,VDU.PutMap
	
	pop af
	jr z,Terminal.GotByte
	
	ei
	halt

	in a,($DD)
	bit 4,a
	jr nz,Terminal.Loop
	
	ld a,127
	call VDU.PutMap
	
-:	in a,($DD)
	bit 4,a
	scf
	ret nz
	jr -

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
	jr nz,Catalogue.Error
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
	jr nz,Catalogue.error
	
	; Then list files.
	push ix
	push hl
	ld ix,Catalogue.PrintFileName
	call PCLink2.ListFiles
	pop hl
	pop ix
	jr nz,Catalogue.error
	
	; We have success!
	scf
	ret

Catalogue.Error:
	ld a,202
	call Basic.BBCBASIC_EXTERR
	.db "Device fault", 0

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
	
Commands:
	osclicommand("TERM", Terminal)
	osclicommand("CAT", Catalogue)
	osclicommand("DIR", Catalogue)
	osclicommand(".", Catalogue)
	.db 0

.endmodule