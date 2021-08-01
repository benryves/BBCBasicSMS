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
; DispatchCommand -> 
; ---------------------------------------------------------
; Inputs:   hl = pointer to command line.
;           de = pointer to command table.
; Outputs:  hl = pointer to next character.
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


Sync:
	call CheckCommandEnd
	ld hl,Sync.Text
	call VDU.PutString
	call PCLink2.Sync
	ld hl,Sync.OK
	jr z,+
	ld hl,Sync.Failed
+:	call VDU.PutString
	scf
	ret

Sync.Text:
	.db "Sync...",0
Sync.OK:
	.db "OK\r",0
Sync.Failed:
	.db "Failed\r",0

Hello:
	call CheckCommandEnd
	ld hl,Hello.Text
	call VDU.PutString
	ld a,'?'
	call VDU.PutChar
	call VDU.Console.NewLine
	
	call PCLink2.Hello
	scf
	ret nz

	ld hl,Hello.Text
	call VDU.PutString
	ld a,'!'
	call VDU.PutChar
	call VDU.Console.NewLine
	scf
	ret
	
Hello.Text:
.db "Hello",0

Goodbye:
	call CheckCommandEnd
	ld hl,Goodbye.Text
	call VDU.PutString
	ld a,'?'
	call VDU.PutChar
	call VDU.Console.NewLine
	
	call PCLink2.Goodbye
	scf
	ret nz
	
	ld hl,Goodbye.Text
	call VDU.PutString
	ld a,'!'
	call VDU.PutChar
	call VDU.Console.NewLine
	scf
	ret
	
Goodbye.Text
.db "Goodbye",0

ListDevices:
	call CheckCommandEnd
	ld hl,ListDevices.Text
	call VDU.PutString
	call PCLink2.ListDevices
	ld hl,Sync.OK
	jr z,+
	ld hl,Sync.Failed
+:	call VDU.PutString
	scf
	ret
ListDevices.Text:
.db "List Devices...",0

ListDirectories:
	call SkipWhitespace
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
	scf
	ret
ListDirectories.Text:
.db "List Directories...",0

ListFiles:
	call SkipWhitespace
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
	scf
	ret
ListFiles.Text:
.db "List Files...",0

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

Commands:
	osclicommand("SYNC", Sync)
	osclicommand("HELLO", Hello)
	osclicommand("GOODBYE", Goodbye)
	osclicommand("LISTDEVICES", ListDevices)
	osclicommand("LISTDIRS", ListDirectories)
	osclicommand("LISTFILES", ListFiles)
	osclicommand("TERM", Terminal)
	.db 0

.endmodule