.module PCLink2

ListItems.StartOfItem = 2 ; STX
ListItems.EndOfItem   = 3 ; ETX
ListItems.EndOfList   = 4 ; EOF

; ---------------------------------------------------------
; Sync -> Synchronises the PC LINK 2 protocol.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
Sync:

	call Serial.EmptyReadBuffer

	; Send five "5"s.
	ld bc,$0505

-:	push bc
	ld a,c
	call Serial.SendByte
	pop bc
	ret nz
	djnz -
	
	; Send one "6".
	ld a,$06
	call Serial.SendByte
	ret nz
	
	; Wait for a "5".
	ld b,16

-:	push bc
	call Serial.GetByte
	pop bc
	ret nz
	
	cp 5
	jr z,+
	
	djnz -
	
	; We only try 16 times.
	ret
	
+:	

	; Get a "6".
-:	push bc
	call Serial.GetByte
	pop bc
	ret nz
	
	; Are we still getting fives?
	cp 5
	jr nz,+
	djnz -
	
+:	
	; If it's not a "5", it must be a "6"...
	cp 6
	ret nz
	
	; If there's still a character in the serial buffer,
	; We'll allow it if it's a NUL.
-:	ld a,(Serial.SerialReadBuffer.Count)
	or a
	ret z
	
	call Serial.GetByte
	ret nz
	
	or a
	ret nz
	jr -

; ---------------------------------------------------------
; SendAcknowledgedByte -> Sends and checks acknowledgement.
; ---------------------------------------------------------
; Inputs:   a = byte to send.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
SendAcknowledgedByte:
	
	call Serial.SendByteImmediately
	ret nz
	
	call Serial.GetSingleByte
	ret nz
	
	or a
	ret

; ---------------------------------------------------------
; SendDataByte -> Sends a data byte.
; ---------------------------------------------------------
; This function acknowledges every raw byte and generates
; escape codes as required.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
SendDataByte:
	; Special control codes that can be unescaped.
	cp '\t'
	jr z,SendAcknowledgedByte
	cp '\r'
	jr z,SendAcknowledgedByte
	cp '\n'
	jr z,SendAcknowledgedByte
	
	; Is it in the range $20..$7F
	cp $20
	jr c,SendDataByte.Escaped
	cp $80
	jr c,SendAcknowledgedByte

SendDataByte.Escaped:
	
	ld c,a
	
	ld a,$1B ; ESC
	push bc
	call SendAcknowledgedByte
	pop bc
	ret nz
	
	ld a,'B' ; Binary
	push bc
	call SendAcknowledgedByte
	pop bc
	ret nz
	
	; Send the most significant nybble.
	ld a,c
	srl a
	srl a
	srl a
	srl a
	call EncodeHexNybble
	push bc
	call SendAcknowledgedByte
	pop bc
	ret nz
	
	; Send the least significant nybble.
	ld a,c
	and $0F
	call EncodeHexNybble
	jp SendAcknowledgedByte

EncodeHexNybble:
	cp 10
	jr c,+
	add a,'A'-10
	ret
+:	add a,'0'
	ret

; ---------------------------------------------------------
; GetAcknowledgedByte -> Gets a byte and acknowledges it.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  z on success, nz on failure.
;           a = received byte.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
GetAcknowledgedByte:

	call Serial.GetSingleByte
	ret nz
	
	push af
	
	xor a
	call Serial.SendByteImmediately
	jr nz,+
	
	pop af
	ret

+:	pop af
	ld a,$5A ; Non-zero error
	or a
	ret


; ---------------------------------------------------------
; GetDataByte -> Gets a data byte.
; ---------------------------------------------------------
; This function acknowledges every raw byte and handles
; escape codes.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  z on success, nz on failure.
;           a = received byte.
;           c = set if it's an escape character.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
GetDataByte:
	
	; Get the initial byte.
	call GetAcknowledgedByte
	ret nz
	
	cp $1B ; ESC
	jr z,GetDataByte.Escaped

	cp a ; Set zero, clear the carry flag
	ret

GetDataByte.Escaped:
	
	; Get the data byte.
	call GetAcknowledgedByte
	ret nz
	
	cp 'B'
	jr z,GetDataByte.Hex
	
	cp a ; Set zero
	scf  ; Set carry
	
	ret

GetDataByte.Hex:
	
	; Get the most significant nybble.
	call GetAcknowledgedByte
	ret nz
	
	; Convert from hex to decimal.
	call ParseHexNybble
	ret nz
	
	add a,a
	add a,a
	add a,a
	add a,a
	ld b,a
	
	; Get the least significant nybble.
	push bc
	call GetAcknowledgedByte
	pop bc
	ret nz
	
	; Convert from hex to decimal.
	call ParseHexNybble
	ret nz
	
	; Combine with the most significant nybble.
	add a,b
	
	cp a ; Set zero, clear carry.
	ret

ParseHexNybble:

	; a will be either '0'-'9' or 'A'-'F'.
	
	cp '0'
	ret c
	cp '9'*1+1
	jr nc,ParseHexNybble.NotDecimal
	
	sub '0'
	cp a
	ret

ParseHexNybble.NotDecimal:

	cp 'A'
	ret c
	cp 'F'*1+1
	jr c,ParseHexNybble.Alpha
	
	dec a
	ret

ParseHexNybble.Alpha:
	
	sub 'A'-10
	cp a
	ret


; ---------------------------------------------------------
; SendEscapeCode -> Sends an escape sequence
; ---------------------------------------------------------
; Inputs:   a = escaped character to send.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
SendEscapeCode:
	ld c,a
	push bc
	
	ld a,$1B ; ESC
	call SendAcknowledgedByte
	
	pop bc
	ret nz

	ld a,c ; The escaped character
	jp SendAcknowledgedByte

; ---------------------------------------------------------
; GetEscapeCode -> Gets an escape sequence
; ---------------------------------------------------------
; Inputs:   None
; Outputs:  z on success, nz on failure.
;           a = received code.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
GetEscapeCode:

	call GetAcknowledgedByte
	ret nz
	
	cp $1B ; ESC
	ret nz
	
	call GetAcknowledgedByte
	ret

; ---------------------------------------------------------
; Hello -> Asks if the computer is there and responding.
; ---------------------------------------------------------
; Inputs:   None
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
Hello:
	call Sync
	ret nz
		
	ld a,'A' ; Are you there?
	call SendEscapeCode
	ret nz
	
	call GetEscapeCode
	ret nz
	
	cp 'Y' ; Yes, I'm here!
	ret

; ---------------------------------------------------------
; Goodbye -> Ends a session.
; ---------------------------------------------------------
; Inputs:   None
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
Goodbye:
	call Sync
	ret nz
	
	ld a,'Q' ; Quit
	call SendEscapeCode
	ret nz
	
	call GetEscapeCode
	ret nz
	
	cp 'Y' ; Yes, I'm here!
	ret


; ---------------------------------------------------------
; ListDevices -> Shows a list of devices.
; ---------------------------------------------------------
; Inputs:   ix = callback to print characters with.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
ListDevices:
	ld c,'H'
	ld hl,0
	jr ListItems

; ---------------------------------------------------------
; ListDirectories -> Shows a list of directories.
; ---------------------------------------------------------
; Inputs:   hl = pointer to full path string.
;           ix = callback to print characters with.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
ListDirectories:
	ld c,'D'
	jr ListItems

; ---------------------------------------------------------
; ListFiles -> Shows a list of files.
; ---------------------------------------------------------
; Inputs:   hl = pointer to full path string.
;           ix = callback to print characters with.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
ListFiles:
	ld c,'N'
	; Fall-through

; ---------------------------------------------------------
; ListFiles -> Shows a list of items.
; ---------------------------------------------------------
; Inputs:   c = type of item do list.
;           hl = pointer to path or 0 to omit.
;           ix = callback to print characters with.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
ListItems:
	; Sync.
	push hl
	push bc
	call Sync
	pop bc
	pop hl
	ret nz
	
	; Send the list request.
	push hl
	push bc
	ld a,c
	call SendEscapeCode
	pop bc
	pop hl
	ret nz

	; Is there a path to send?
	ld a,h
	or l
	jr z,ListItems.NoPath
	
ListItems.SendPath:

	ld a,(hl)
	or a
	jr z,ListItems.PathSent
	cp '\r'
	jr z,ListItems.PathSent
	
	push hl
	call SendDataByte
	pop hl
	ret nz
	
	inc hl
	jr ListItems.SendPath
	
ListItems.PathSent:
	
	; Item count = 0.
	ld bc,0
	ld (TempPtr),bc
	
	; End the path with a 'Z' escape code.
	ld a,'Z'
	call SendEscapeCode
	ret nz

ListItems.NoPath:
ListItems.Loop:
	call GetDataByte
	ret nz
	
	jr nc,List.NotEscapeCode
	
	cp 'Z' ; End of list.
	jr nz,+
	
	ld bc,(TempPtr)
	ld a,b
	or c
	jr z,ListItems.EmptyList
	
	ld a,ListItems.EndOfItem
	ld hl,ListItems.EmptyList
	push hl
	jp (ix)
	
ListItems.EmptyList:
	ld a,ListItems.EndOfList
	ld hl,ListItems.ReturnEndOfList
	push hl
	jp (ix)
	
ListItems.ReturnEndOfList:
	xor a ; Set z.
	ret
	
+:
	
	cp 'N' ; Item name.
	ret nz
	
	
	ld bc,(TempPtr)
	ld a,b
	or c
	inc bc
	ld (TempPtr),bc
	
	jr z,ListItems.FirstItem
	
	ld a,ListItems.EndOfItem
	ld hl,ListItems.FirstItem
	push hl
	jp (ix)
ListItems.FirstItem:
	ld a,ListItems.StartOfItem
	ld hl,ListItems.Loop
	push hl
	jp (ix)

List.NotEscapeCode:
	ld bc,(TempPtr)
	ld hl,ListItems.Loop
	push hl
	jp (ix)
	
; ---------------------------------------------------------
; GetFile -> Gets a file.
; ---------------------------------------------------------
; Inputs:   hl = pointer to file name NUL/CR terminated.
;           de = pointer to RAM to store the file in.
;           bc = maximum file size that can be loaded.
; Outputs:  nz if there was a protocol/receive error.
;           if no error, c set if transfer was cancelled.
;           if error, c is set if the error is "No room".
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
GetFile:
	ld (TempPtr),de
	ld (TempCapacity),bc
	
	; Sync.
	push hl
	call Sync
	pop hl
	jp nz,GetFile.ProtocolError
	
	; Send the "get file" request.
	push hl
	ld a,'G'
	call SendEscapeCode
	pop hl
	jp nz,GetFile.ProtocolError

	; Send the path.
	
GetFile.SendPath:

	ld a,(hl)
	or a
	jr z,GetFile.PathSent
	cp '\r'
	jr z,GetFile.PathSent
	
	push hl
	call SendDataByte
	pop hl
	jr nz,GetFile.ProtocolError
	
	inc hl
	jr GetFile.SendPath
	
GetFile.PathSent:
	
	; End the path with a 'Z' escape code.
	ld a,'Z'
	call SendEscapeCode
	jr nz,GetFile.ProtocolError
	
	; Start reading the file.

GetFile.ReceiveFileLoop:

	ld a,(TempCapacity)
	and %00000111
	scf
	call z,Trap
	jr c,GetFile.CanCarry
	
	call Serial.GetSingleByte
	scf
	ccf
	ret nz
	ld a,1
	call Serial.SendByteImmediately
	xor a
	scf
	ret


GetFile.CanCarry:

	call GetDataByte
	jr nz,GetFile.ProtocolError
	
	jr nc,GetFile.NotEscapeCode
	
	cp 'Z' ; End of file.
	jr nz,GetFile.ProtocolError
	
	xor a ; Set z, clear carry.
	ret

GetFile.NotEscapeCode:

	ld e,a
	
	ld bc,(TempCapacity)
	ld a,b
	or c
	jr z,GetFile.SizeError
	dec bc
	ld (TempCapacity),bc
	
	ld hl,(TempPtr)
	ld (hl),e
	inc hl
	ld (TempPtr),hl
	
	jr GetFile.ReceiveFileLoop

GetFile.SizeError:
	
	; Send a bad ACK.
	call Serial.GetByte
	jr nz,+
	
	ld a,1 ; Out of room
	call Serial.SendByteImmediately
	jr nz,+
	
	or 1 ; Force NZ

+:
	scf
	ret
	
GetFile.ProtocolError:
	scf
	ccf
	ret

; ---------------------------------------------------------
; SendFile -> Sends a file.
; ---------------------------------------------------------
; Inputs:   hl = pointer to file name NUL/CR terminated.
;           de = pointer to RAM to get the file from.
;           bc = size of the file to send.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
SendFile:

	ld (TempPtr),de
	ld (TempSize),bc
	
	; Sync.
	push hl
	call Sync
	pop hl
	ret nz
	
	; Send the "send file" request.
	push hl
	ld a,'S'
	call SendEscapeCode
	pop hl
	ret nz
	
	; Send the path.
	
	push hl
	ld a,'N'
	call SendEscapeCode
	pop hl
	ret nz
	
SendFile.SendPath:

	ld a,(hl)
	or a
	jr z,SendFile.PathSent
	cp '\r'
	jr z,SendFile.PathSent
	
	push hl
	call SendDataByte
	pop hl
	ret nz
	
	inc hl
	jr SendFile.SendPath

SendFile.PathSent:
	
	; Now start sending the file with an F escape code.
	ld a,'F'
	call SendEscapeCode
	ret nz
	
	; Start sending the file data.
	
	ld hl,(TempPtr)
	ld bc,(TempSize)
	
	; Send each byte in turn.
-:	push hl
	push bc
	
	push hl
	ld a,c
	and %00000111
	scf
	call z,Trap
	pop hl
	jr c,SendFile.CanCarry
	
	pop bc
	pop hl
	xor a
	scf
	ret

SendFile.CanCarry:
	
	ld a,(hl)
	call SendDataByte
	
	pop bc
	pop hl
	ret nz
	
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,-
	
	
	; End of file.
	ld a,'E'
	call SendEscapeCode
	ret nz
	
	; Only one file, so end with a 'Z' escape code.
	ld a,'Z'
	call SendEscapeCode
	ret nz
	
	xor a
	ret

.endmodule