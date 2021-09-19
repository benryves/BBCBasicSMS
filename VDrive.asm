; ==========================================================================
; VDrive
; --------------------------------------------------------------------------
; Provides access to files on a Vinculum device (e.g. VDrive/VMusic)
; ==========================================================================
.module VDrive

.module Commands

; Monitor commands
ShortCommandSet = $10
ExtendedCommandSet = $11
MonitorAscii = $90
MonitorBinary = $91
FirmwareVersion = $13
EchoUppercase = $45
EchoLowercase = $65

; Disk commands
ListDirectory = $01
ChangeDirectory = $02
ReadFile = $04
DeleteDirectory = $05
MakeDirectory = $06
DeleteFile = $07
WriteFileData = $08
OpenFileWriting = $09
CloseFile = $0A
ReadFileData = $0B
Rename = $0C
OpenFileReading = $0E
Seek = $28
GetFreeSpaceUnder4GB = $12
GetFreeSpace = $93
DisplayInformationUnder4GB = $0F
DisplayInformation = $94
DisplaySerialNumber = $2D
DisplayVolumeLabel = $2E
ListFileInformation = $2E

.endmodule

; ==========================================================================
; Reset
; --------------------------------------------------------------------------
; Resets the VDrive system to its default state.
; --------------------------------------------------------------------------
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
Reset:
	call Serial.Reset
	ret

; ==========================================================================
; SendCommandByte
; --------------------------------------------------------------------------
; Sends a single command byte followed by CR.
; --------------------------------------------------------------------------
; Inputs:     A: Command byte to send.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
SendCommandByte:
	call Serial.SendByte
	ld a,'\r'
	jp Serial.SendByte

; ==========================================================================
; SendCommandString
; --------------------------------------------------------------------------
; Sends a command byte followed by a space, a string parameter, then CR.
; --------------------------------------------------------------------------
; Inputs:     A: Command byte to send.
;             HL: Pointer to string argument to send.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
SendCommandString:
	push hl
	call Serial.SendByte
	ld a,' '
	call Serial.SendByte
	
-:	pop hl
	ld a,(hl)
	inc hl
	or a
	jr z,+
	cp '\r'
	jr z,+
	push hl
	call Serial.SendByte
	jr -
	
+:	ld a,'\r'
	jp Serial.SendByte

; ==========================================================================
; CheckCommandResponseByte
; --------------------------------------------------------------------------
; Gets a command response byte, checks that is it valid, then checks that it
; is followed by CR.
; --------------------------------------------------------------------------
; Inputs:     A: Command response byte to check for.
; Outputs:    F: Z set if the response byte matched, NZ if not.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
CheckCommandResponseByte:
	call GetAndCheckByte
	jr nz,FlushSerialToCR
	
	ld a,'\r'
	call GetAndCheckByte
	ret z
	
	; Fall-through to FlushSerialToCR

; ==========================================================================
; FlushSerialToCR
; --------------------------------------------------------------------------
; Read from the serial port until CR or no data is received.
; --------------------------------------------------------------------------
; Destroyed:  BC, DE, HL.
; ==========================================================================
FlushSerialToCR:
	push af
-:	ld de,1
	call Serial.GetByteWithTimeout
	jr nz,+
	cp '\r'
	jr nz,-
+:	pop af
	ret

; ==========================================================================
; GetByteSkipCR
; --------------------------------------------------------------------------
; Read from the serial port, but ignore the byte if it's CR.
; --------------------------------------------------------------------------
; Outputs:    F: Z if a non-CR byte was received, NZ otherwise.
;             A: The non-CR byte that was read.
; Destroyed:  BC, DE, HL.
; ==========================================================================
GetByteSkipCR:
	call Serial.GetByte
	ret nz
	cp '\r'
	jr z,GetByteSkipCR
	cp a
	ret

; ==========================================================================
; GetAndCheckByte
; --------------------------------------------------------------------------
; Gets a single byte from the serial port and checks the value.
; --------------------------------------------------------------------------
; Inputs:     A: Command byte to check.
; Outputs:    F: Z if the expected value was received, NZ if not.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
GetAndCheckByte:
	push af
	call Serial.GetByte
	pop bc
	ret nz
	cp b
	ret

; ==========================================================================
; CheckEcho
; --------------------------------------------------------------------------
; Checks to see if the drive is responding to echo requests.
; --------------------------------------------------------------------------
; Inputs:     A: Echo type ('E' or 'e').
; Outputs:    F: Z if the echo was received, NZ if not.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
CheckEcho:
	push af
	
	push af
	call SendCommandByte
	pop af
	call CheckCommandResponseByte
	
	jr z,GotFirstEcho
	pop bc
	ret

GotFirstEcho:

	; VDAP2 firmware seems to have a bug where it responds to echo twice.
	ld de,1
	call Serial.GetByteWithTimeout
	jr nz,NoDoubleEcho
	
	pop bc
	cp b
	ret nz
	
	call FlushSerialToCR
	ret

NoDoubleEcho:
	pop bc
	xor a
	ret

; ==========================================================================
; Sync
; --------------------------------------------------------------------------
; Synchronises the VDrive device, ready to send a command.
; --------------------------------------------------------------------------
; Outputs:    F: Z set on success, NZ on failure.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
Sync:
	; Hopefully one sync attempt will be enough.
	call SingleSyncAttempt
	ret z
	
	.bcall "VDU.BeginBlinkingCursor"
	
	ld b,10
SyncRepeatedAttempt:
	push bc
	call SingleSyncAttempt
	pop bc
	jr nz,SyncFailed
	.bcall "VDU.EndBlinkingCursor"
	xor a
	ret

SyncFailed:
	push bc
	.bcall "VDU.DrawBlinkingCursor"
	ei
	halt
	call Host.CheckEscape
	pop bc
	djnz SyncRepeatedAttempt
	.bcall "VDU.EndBlinkingCursor"
	xor a
	dec a
	ret

; ==========================================================================
; SingleSyncAttempt
; --------------------------------------------------------------------------
; Performs a single synchronisation attempt.
; --------------------------------------------------------------------------
; Outputs:    F: Z set on success, NZ on failure.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
SingleSyncAttempt:
	ld a,'\r'
	call Serial.SendByte
	
	; Flush any incoming data.
-:	ld de,1
	call Serial.GetByteWithTimeout
	jr z,-
	
	; Send E for Echo and check we get E back.
	
	ld a,'E'
	call CheckEcho
	ret nz

	; Switch to the short command set and check we get a > prompt back.
	ld a,Commands.ShortCommandSet
	call SendCommandByte
	ld a,'>'
	call CheckCommandResponseByte
	ret

; ==========================================================================
; SyncOrDeviceFault
; --------------------------------------------------------------------------
; Synchronises the VDrive device, ready to send a command, or triggers a
; device fault error if the VDrive could not be synchronised.
; --------------------------------------------------------------------------
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
SyncOrDeviceFault:
	call Sync
	ret z
	jp Host.DeviceFault

; ==========================================================================
; GetFileSize
; --------------------------------------------------------------------------
; Gets a size of a file from the VDrive.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to file name NUL/CR terminated.
; Outputs:    F: NZ if there was an error.
;             DEHL: Size of the file if there was no error.
; Destroyed:  AF, BC, DE, HL.
; Interrupts: Disabled.
; ==========================================================================
GetFileSize:
	push hl
	call Sync
	pop hl
	ret nz
	
	ld a,Commands.ListDirectory
	push hl
	call SendCommandString
	
	; Check we get the supplied file name back.
	call GetByteSkipCR
	
-:	pop hl
	ret nz
	
	cp ' '
	jr z,GotFileName
	
	call File.NormaliseFilenameCharacter
	ld b,a
	
	ld a,(hl)
	call File.NormaliseFilenameCharacter
	cp b
	
	inc hl
	jp nz,FlushSerialToCR
	
	push hl
	call Serial.GetByte
	jr -

GotFileName:
	
	; After the file name, four bytes of file size data.
	ld b,4
	ld hl,VDU.TempTile
	
-:	push hl
	push bc
	call Serial.GetSingleByte
	pop bc
	pop hl
	ret nz
	ld (hl),a
	inc hl
	djnz -
	
	call Serial.GetSingleByte
	ret nz
	cp '\r'
	jp nz,FlushSerialToCR
	
	ld hl,(VDU.TempTile+0)
	ld de,(VDU.TempTile+2)
	ret

; ==========================================================================
; GetFile
; --------------------------------------------------------------------------
; Gets a file from the VDrive.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to file name NUL/CR terminated.
;             DE: Pointer to RAM to store the file in.
;             BC: Maximum file size that can be loaded.
; Outputs:    F: NZ if there was a protocol/receive error.
;                If no error, C is set if the transfer was cancelled.
;                If there is an error, C is set if the error is "No room".
; Destroyed:  AF, BC, DE, HL.
; Interrupts: Disabled.
; ==========================================================================
GetFile:
	ld (TempPtr),hl
	ld (TempCapacity),bc
	ld (TempSize),de
	
	; Get the file size.
	ld hl,(TempPtr)
	call GetFileSize
	jr z,+
	scf
	ccf
	ret
+:

	; Ensure that files are under 64KB.
	ld a,d
	or e
	jr z,+
	scf
	ret
+:
	
	; Do we have enough room?
	ld de,(TempCapacity)
	or a
	ex de,hl
	sbc hl,de
	ret c
	
	; Store the actual size.
	ld (TempCapacity),de
	
	; Start reading the file.
	call Sync
	ret nz
	
	ld a,Commands.ReadFile
	ld hl,(TempPtr)
	call SendCommandString
	
	; Retrieve the size and capacity.
	ld hl,(TempSize)
	ld bc,(TempCapacity)
	
-:	push hl
	push bc
	call Serial.GetByte
	pop bc
	pop hl
	jr z,+
	
	scf
	ccf
	ret
	
+:	ld (hl),a
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,-
	
	ret

.endmodule