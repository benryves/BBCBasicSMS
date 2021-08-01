.module CLI

Execute:

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
	.db "OK\r",0
Sync.Failed:
	.db "Failed\r",0

Hello:
	ld hl,Hello.Text
	call VDU.PutString
	ld a,'?'
	call VDU.PutChar
	call VDU.Console.NewLine
	
	call PCLink2.Hello
	ret nz

	ld hl,Hello.Text
	call VDU.PutString
	ld a,'!'
	call VDU.PutChar
	call VDU.Console.NewLine
	ret
	
Hello.Text:
.db "Hello",0

Goodbye:
	ld hl,Goodbye.Text
	call VDU.PutString
	ld a,'?'
	call VDU.PutChar
	call VDU.Console.NewLine
	
	call PCLink2.Goodbye
	ret nz
	
	ld hl,Goodbye.Text
	call VDU.PutString
	ld a,'!'
	call VDU.PutChar
	call VDU.Console.NewLine
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

.endmodule