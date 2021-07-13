.module PCLink2

TempPtr = allocVar(2)
TempCapacity = allocVar(2)
TempChecksum = allocVar(2)
TempSize = allocVar(2)

QueuedEscapeByte.Received = allocVar(1)
QueuedEscapeByte.Value = allocVar(1)

; ---------------------------------------------------------
; Sync -> Synchronises the PC LINK 2 protocol.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
Sync:

	call Serial.EmptyReadBuffer
	
	xor a
	ld (QueuedEscapeByte.Received),a

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
	
	call Serial.SendByte
	ret nz
	
	call Serial.GetByte
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
	
	; Is there an escaped byte in the queue?
	ld a,(QueuedEscapeByte.Received)
	or a
	jr z,+
	
	xor a
	ld (QueuedEscapeByte.Received),a
	ld a,(QueuedEscapeByte.Value)
	cp a
	ret

+:	call Serial.GetByte
	ret nz
	
	push af
	
	xor a
	call Serial.SendByte
	jr nz,+
	
	pop af
	ret

+:	pop af
	ld a,$27 ; Non-zero error
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
	
	ld a,'#'
	call VDU.PutChar
	
	; Get the data byte.
	call GetAcknowledgedByte
	ret nz
	
	cp 'B'
	jr z,GetDataByte.Hex
	
	cp 'Z' ; EOF
	jr GetDataByte.PermittedEscape
	
	; If we get this far, it's an escape code, but not one we recognise...
	; Store it for later and return a literal $1B.
	ld (QueuedEscapeByte.Value),a
	
	call PutHexByte
	ld a,'~'
	call VDU.PutChar
	
	ld a,$1B
	ld (QueuedEscapeByte.Received),a
	cp a
	ret z
	

GetDataByte.PermittedEscape:
	
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
	
	sub 'A'
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
; Inputs:   None
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
	
	; End the path with a 'Z' escape code.
	ld a,'Z'
	call SendEscapeCode
	ret nz

ListItems.NoPath:
	
-:	call GetDataByte
	ret nz
	
	jr nc,List.NotEscapeCode
	
	cp 'Z' ; End of list.
	jr nz,+
	call VDU.NewLine
	xor a ; Set z.
	ret
+:
	
	cp 'N' ; Item name.
	ret nz
	call VDU.NewLine
	jr -

List.NotEscapeCode:
	
	call VDU.PutChar
	jr -
	
; ---------------------------------------------------------
; GetFile -> Gets a file.
; ---------------------------------------------------------
; Inputs:   hl = pointer to path and pattern or 0 to omit.
;           de = pointer to RAM to store the file in.
;           bc = maximum file size that can be loaded.
; Outputs:  z on success, nz on failure.
;           c is 
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
GetFile:
	ld (TempPtr),de
	ld (TempCapacity),bc
	ld bc,0
	ld (TempChecksum),bc
	ld (TempSize),bc
	
	; Sync.
	push hl
	call Sync
	pop hl
	jp nz,GetFile.ProtocolError
	
	; Send the list request.
	push hl
	ld a,'G'
	call SendEscapeCode
	pop hl
	jp nz,GetFile.ProtocolError

	; Is there a path to send?
	
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

	call GetDataByte
	
	jr nz,GetFile.ProtocolError
	
	jr nc,GetFile.NotEscapeCode ; <- Are we 
	
	cp 'Z' ; End of file.
	jr nz,GetFile.ProtocolError
	
	call GetFile.CompareChecksum ; Debugging
	
	xor a ; Set z.
	ret

GetFile.NotEscapeCode:

	ld e,a
	ld d,0
	ld hl,(TempChecksum)
	add hl,de
	ld (TempChecksum),hl
	
	ld bc,(TempCapacity)
	
	ld a,b
	or c
	jr z,GetFile.SizeError
	
	ld hl,(TempPtr)
	ld (hl),e
	
	inc hl
	dec de
	ld (TempPtr),hl
	ld (TempCapacity),hl
	
	ld hl,(TempSize)
	inc hl
	ld (TempSize),hl
	
	jr GetFile.ReceiveFileLoop

GetFile.SizeError:
	
	; Send a bad ACK.
	call Serial.GetByte
	jr nz,+
	
	ld a,1 ; Out of room
	call Serial.SendByte
	jr nz,+
	
	or 1 ; Force NZ

+:
	scf
	ret
	
GetFile.ProtocolError:
	scf
	ccf
	ret

GetFile.CompareChecksum:
	ld a,'S'
	call VDU.PutChar
	ld a,'='
	call VDU.PutChar
	ld hl,(TempSize)
	call PutHexWord
	call VDU.NewLine
	
	ld a,'C'
	call VDU.PutChar
	ld a,'='
	call VDU.PutChar
	ld hl,(TempChecksum)
	call PutHexWord
	call VDU.NewLine
	
	; Is the received data the same size as the checksum test file?
	ld hl,(TempSize)
	ld de,ChecksumTestFile.Size
	or a
	sbc hl,de
	ld a,h
	or l
	ret nz
	
	; Start from the beginning of the file.
	ld hl,(TempPtr)
	ld de,-ChecksumTestFile.Size
	add hl,de
	ld de,ChecksumTestFile
	
	ld bc,ChecksumTestFile.Size
	
-:	ld a,(de)
	cp (hl)
	jr z,+
	
	push hl
	push de
	push bc
	
	ld a,'E'
	call VDU.PutChar
	ld a,'@'
	call VDU.PutChar
	
	ld hl,(TempSize)
	pop bc
	push bc
	or a
	sbc hl,bc
	call PutHexWord
	
	ld a,' '
	call VDU.PutChar
	ld a,':'
	call VDU.PutChar
	ld a,')'
	call VDU.PutChar
	ld a,'='
	call VDU.PutChar
	
	pop bc
	pop de
	push de
	push bc
	
	ld a,(de)
	call PutHexByte
	
	ld a,' '
	call VDU.PutChar
	ld a,':'
	call VDU.PutChar
	ld a,'('
	call VDU.PutChar
	ld a,'='
	call VDU.PutChar
	
	pop bc
	pop de
	pop hl
	push hl
	push de
	push bc
	
	ld a,(hl)
	call PutHexByte
	
	call VDU.NewLine
	
	pop bc
	pop de
	pop hl

+:	inc hl
	inc de
	dec bc
	ld a,b
	or c
	jr nz,-
	
	ret

ChecksumTestFile:
.incbin "Programs/CHECKSUM.BBC"
ChecksumTestFile.End:
ChecksumTestFile.Size = ChecksumTestFile.End - ChecksumTestFile

.endmodule