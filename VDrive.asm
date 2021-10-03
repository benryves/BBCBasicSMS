; ==========================================================================
; VDrive
; --------------------------------------------------------------------------
; Provides access to files on a Vinculum device (e.g. VDrive/VMusic)
; --------------------------------------------------------------------------
; Pin connections are similar to the RS-232 adaptor:
;
; SMS        VDrive
; 1 Up   <- 2 RTS#
; 2 Down <- 5 TxD
; 5 Vcc  -- 3 5V0
; 7 TH   -> 4 RxD
; 8 GND  -- 1 GND
; 9 TR   -> 6 CTS#
;
; SMS pins 3 (Left), 4 (Right) and 6 (TL) are not connected.
; VDrive pins 7 (NC) and 8 (RI#/WU) are not connected.
; VDrive jumper must be in default UART position.
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
; SendCommandInt
; --------------------------------------------------------------------------
; Sends a command byte followed by a space, a 32-bit integer, then CR.
; --------------------------------------------------------------------------
; Inputs:     A: Command byte to send.
;             DEHL: 32-bit integer to send.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
SendCommandInt:
	push hl
	push de
	
	call Serial.SendByte
	ld a,' '
	call Serial.SendByte
	
	pop af
	push af
	call Serial.SendByte
	pop de
	ld a,e
	call Serial.SendByte
	
	pop af
	push af
	call Serial.SendByte
	pop hl
	ld a,l
	call Serial.SendByte
	
	ld a,'\r'
	jp Serial.SendByte
	

; ==========================================================================
; CheckForPrompt
; --------------------------------------------------------------------------
; Checks that we've received the prompt.
; --------------------------------------------------------------------------
; Outputs:    F: Z set if the response was a '>' prompt, NZ if not.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
CheckForPrompt:
	ld a,'>'
	
	; Fall-through to CheckCommandResponseByte

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
	push bc
	call Serial.GetByteWithTimeout
	pop bc
	jr nz,+
	cp '\r'
	jr z,+
	ld b,c
	ld c,a
	jr -
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
; Inputs:     A: Response byte to check.
; Outputs:    F: Z if the expected value was received, NZ if not.
;             C: The value that was received.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
GetAndCheckByte:
	push af
	call Serial.GetByte
	pop bc
	ld c,a
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
	call CheckForPrompt
	ret nz
	
	; Switch to binary mode.
	ld a,Commands.MonitorBinary
	call SendCommandByte
	call CheckForPrompt
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
; TriggerDirectoryError
; --------------------------------------------------------------------------
; Triggers a directory-related error based on an error code.
; --------------------------------------------------------------------------
; Inputs:     BC: Error code.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
TriggerDirectoryError:
	ld hl,'C'*256+'F'*1
	or a
	sbc hl,bc
	jr nz,TriggerError
	ld bc,'F'*256+'I'*1
	
	; Fall-through to TriggerError

; ==========================================================================
; TriggerError
; --------------------------------------------------------------------------
; Triggers an error based on an error code.
; --------------------------------------------------------------------------
; Inputs:     BC: Error code.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
TriggerError:
	ld hl,Errors

	ld a,(hl)
-:	inc hl
	cp b
	ld a,(hl)
	inc hl
	
	jr nz,+
	cp c
	jr nz,+
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	jp (hl)

+:	inc hl
	inc hl
	ld a,(hl)
	or a
	jr nz,-
	jp Host.DeviceFault

Errors:
	.db "BC" \ .dw Host.DeviceFault
	.db "CF" \ .dw File.FileNotFound
	.db "DF" \ .dw File.DiskFull
	.db "FI" \ .dw File.BadDirectory
	.db "RO" \ .dw File.AccessDenied
	.db "FO" \ .dw File.TooManyOpenFiles
	.db "NE" \ .dw File.BadDirectory
	.db "FN" \ .dw File.FileNotFound
	.db "ND" \ .dw File.DiskFault
	.db 0

; ==========================================================================
; Catalogue
; --------------------------------------------------------------------------
; Displays a file and directory listing.
; --------------------------------------------------------------------------
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
Catalogue:
	call SyncOrDeviceFault
	
	; Request a directory listing.
	ld a,Commands.ListDirectory
	call SendCommandByte
	
	; CR before the file listing.
	ld a,'\r'
	call GetAndCheckByte
	jr z,+
	call FlushSerialToCR
	jp TriggerError
+:	

-:	call Serial.GetByte
	jp nz,Host.DeviceFault
	
	; Is it the end of the file list?
	cp '>'
	jp z,FlushSerialToCR
	
	.bcall "VDU.PutChar"
	jr -
	
	ret

; ==========================================================================
; ValidateDirectoryName
; --------------------------------------------------------------------------
; Checks that a directory name is valid.
; --------------------------------------------------------------------------
; Inputs:     HL: The directory name to validate.
; Outputs:    F: Z set if the directory name is valid, NZ if it is not.
; Destroyed:  AF.
; ==========================================================================
ValidateDirectoryName:
	ld a,(hl)
	cp '.'
	jr z,ValidateDirectoryName.Dot
	cp '/'
	jr z,ValidateDirectoryName.Slash
	jr ValidateFilename

ValidateDirectoryName.Dot:
	push hl
	push bc

	; Check for a maximum of 2 dots (+terminator).
	ld b,2+1
-:	ld a,(hl)
	call File.NormaliseFilenameCharacter
	or a
	jr z,+
	cp '.'
	jr nz,+
	inc hl
	djnz -
	dec b	
+:
	pop bc
	pop hl
	ret

ValidateDirectoryName.Slash:
	push hl
	inc hl
	ld a,(hl)
	call File.NormaliseFilenameCharacter
	or a ; Slash MUST be followed by a terminator.
	pop hl
	ret

; ==========================================================================
; ValidateDirectoryNameOrBadDirectory
; --------------------------------------------------------------------------
; Checks that a directory name is valid, or triggers "Bad directory" if not.
; --------------------------------------------------------------------------
; Inputs:     HL: The directory name to validate.
; Destroyed:  AF.
; ==========================================================================
ValidateDirectoryNameOrBadDirectory:
	call ValidateDirectoryName
	ret z
	jp File.BadName

; ==========================================================================
; ValidateFilename
; --------------------------------------------------------------------------
; Checks that a filename is valid.
; --------------------------------------------------------------------------
; Inputs:     HL: The filename to validate.
; Outputs:    F: Z set if the file name is valid, NZ if it is not.
; Destroyed:  AF.
; ==========================================================================
ValidateFilename:
	
	; Quick check for filenames that start with a dot.
	; Technically seem to be allowed as hidden files, but not documented!
	ld a,(hl)
	cp '.'
	jr nz,+
	or a
	ret

+:	push hl
	push bc
	
	; Check the part before the extension.
	ld b,8+1
	call ValidateFilenamePart
	jr z,+
	
	; There's an error. Was it because of a '.'?
	cp '.'
	jr nz,+
	
	; Check the extension, if there was a '.'
	inc hl
	ld b,3+1
	call ValidateFilenamePart
+:
	
	pop bc
	pop hl
	ret

ValidateFilenamePart:
	; The filename part cannot be empty.
	ld a,(hl)
	call File.NormaliseFilenameCharacter
	or a
	jr nz,+
	ld b,a
	inc a
	ret
+:
	; Check each character up to the maximum length.
-:	ld a,(hl)
	
	call File.NormaliseFilenameCharacter
	or a
	ret z
	
	; Check that the character is valid.
	push hl
	push bc
	ld hl,ValidCharacters
	ld bc,ValidCharacters.Count
	cpir
	pop bc
	pop hl
	ret nz
	
	inc hl
	djnz -
	
	; If we get this far, the part is too long.
	dec b
	ret

ValidCharacters:
.db "$%'-_@~`!(){}^#&ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
ValidCharacters.Count = $-ValidCharacters

; ==========================================================================
; ValidateFilenameOrBadName
; --------------------------------------------------------------------------
; Checks that a filename is valid, and triggers a "Bad name" error if not.
; --------------------------------------------------------------------------
; Inputs:     HL: The filename to validate.
; Destroyed:  AF.
; ==========================================================================
ValidateFilenameOrBadName:
	call ValidateFilename
	ret z
	jp File.BadName

; ==========================================================================
; ChangeDirectory
; --------------------------------------------------------------------------
; Changes the current directory.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to new directory name NUL/CR terminated.
; Destroyed:  AF, BC, DE, HL.
; Interrupts: Disabled.
; ==========================================================================
ChangeDirectory:
	call ValidateDirectoryNameOrBadDirectory
	call SyncOrDeviceFault
	
	ld a,Commands.ChangeDirectory
	call SendCommandString
	call CheckForPrompt
	ret z
	jp TriggerDirectoryError

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
	push bc
	call GetByteSkipCR
	pop bc
	ld c,a
	
-:	pop hl
	ret nz
	
	ld b,c
	ld c,a
	
	cp ' '
	jr z,GotFileName
	
	call File.NormaliseFilenameCharacter
	ld d,a
	
	ld a,(hl)
	call File.NormaliseFilenameCharacter
	cp d
	
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
	call Serial.GetByte
	pop bc
	pop hl
	ret nz
	ld (hl),a
	inc hl
	djnz -
	
	call Serial.GetByte
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
	
	call ValidateFilenameOrBadName
	
	ld (TempPtr),hl
	ld (TempCapacity),bc
	ld (TempSize),de
	
	; Get the file size.
	ld hl,(TempPtr)
	call GetFileSize
	jp nz,TriggerError

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

; ==========================================================================
; WriteFile
; --------------------------------------------------------------------------
; Writes a file to the VDrive.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to the file name NUL/CR terminated.
;             DE: Pointer to RAM to get the file data from.
;             BC: Size of the file to write.
; Destroyed:  AF, BC, DE, HL.
; Interrupts: Disabled.
; ==========================================================================
WriteFile:
	
	call ValidateFilenameOrBadName
	
	; We'll need to remember the file location and size.
	ld (TempCapacity),hl
	ld (TempPtr),de
	ld (TempSize),bc
	
	call SyncOrDeviceFault
	
	; Open the file for writing.
	ld a,Commands.OpenFileWriting
	ld hl,(TempCapacity)
	call SendCommandString
	call CheckForPrompt
	jp nz,TriggerError
	
	; Seek to the start of the file.
	ld a,Commands.Seek
	ld de,0
	ld hl,0
	call SendCommandInt
	call CheckForPrompt
	jp nz,TriggerError
	
	; How much data are we writing?
	ld hl,(TempSize)
	ld a,l
	or h
	jr z,WriteFile.Empty
	ld de,0
	ld a,Commands.WriteFileData
	call SendCommandInt
	
	; Write the file data.
	ld de,(TempPtr)
	ld bc,(TempSize)
	
-:	ld a,(de)
	inc de
	push de
	push bc
	call Serial.SendByte
	pop bc
	pop de
	dec bc
	ld a,b
	or c
	jr nz,-

WriteFile.Empty:

	; Close the file.
	ld a,Commands.CloseFile
	ld hl,(TempCapacity)
	call SendCommandString
	call CheckForPrompt
	ret z
	jp TriggerError


; ==========================================================================
; FileOpen
; --------------------------------------------------------------------------
; Opens a file.
; --------------------------------------------------------------------------
; Inputs:     IX: Pointer to the file variable data, where
;             IX+0: LSB of pointer to block storage data.
;             IX+1: MSB of pointer to block storage data.
;             IX+2: File system.
;             IX+3: File status (0:closed, 1:OPENOUT, 2:OPENIN, 3:OPENUP).
;             HL: Pointer to CR-terminated filename.
; Outputs:    The file should be opened, if not IX+3 should be set to 0.
; Destroyed:  AF, HL.
; ==========================================================================
FileOpen:
	call ValidateFilename
	jr z,FileOpen.FilenameApproved
	
	ld (ix+3),0
	jp File.BadName

FileOpen.FilenameApproved:
	
	; Data storage for opened files:
	; 16 bytes of filename (CR-terminated)
	; 4 bytes for current EXT#
	; 4 bytes for current PTR#
	; 4 bytes for block address currently fetched into RAM
	; 256 bytes block data storage
	
	ld e,(ix+0)
	ld d,(ix+1)
	ld bc,16
	push hl
	ldir
	
	; Clear EXT# and PTR#
	xor a
	ld (de),a
	ld l,e
	ld h,d
	inc de
	ld bc,8
	ldir
	
	; Set block address to -1
	dec a
	ld (hl),a
	ld bc,4
	ldir
	
	pop hl

	bit 1,(ix+3)
	jr nz,FileOpen.Read
	bit 0,(ix+3)
	jr nz,FileOpen.Write
	ld (ix+3),0
	ret

FileOpen.Read:
	; OPENIN or OPENUP require that the file already exists.
	call GetFileSize
	jr z,+
	ld (ix+3),0
	ret
+:	; DEHL = file size
	push hl
	ld l,(ix+0)
	ld h,(ix+1)
	ld bc,16+3
	add hl,bc
	ld (hl),d
	dec hl
	ld (hl),e
	dec hl
	pop de
	ld (hl),d
	dec hl
	ld (hl),e
	ret

FileOpen.Write:
	; OPENOUT will create a new blank file.
	
	; Pretend the write fails.
	ld a,(ix+3)
	push af
	ld (ix+3),0
	
	; Write a dummy blank file.
	ld bc,0
	call WriteFile
	
	; Now restore the file handle.
	pop af
	ld (ix+3),a
	ret



; ==========================================================================
; FileClose
; --------------------------------------------------------------------------
; Close a file previously opened with FileOpen.
; --------------------------------------------------------------------------
; Inputs:     IX: Pointer to the file variable data, where
;             IX+0: LSB of pointer to block storage data.
;             IX+1: MSB of pointer to block storage data.
;             IX+2: File system.
;             IX+3: File status (0:closed, 1:OPENOUT, 2:OPENIN, 3:OPENUP).
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
FileClose:
	bit 1,(ix+3)
	jr nz,FileClose.Read

FileClose.Write:
	ld (ix+3),0
	ret

FileClose.Read:
	ld (ix+3),0
	ret

; ==========================================================================
; GetLength
; --------------------------------------------------------------------------
; Return the length of an open file.
; --------------------------------------------------------------------------
; Inputs:     IX: Pointer to the file variable data, where
;             IX+0: LSB of pointer to block storage data.
;             IX+1: MSB of pointer to block storage data.
;             IX+2: File system.
;             IX+3: File status (0:closed, 1:OPENOUT, 2:OPENIN, 3:OPENUP).
; Outputs:    DEHL: File size (bytes).
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
FileGetLength:
	push ix
	ld l,(ix+0)
	ld h,(ix+1)
	push hl
	pop ix
	ld l,(ix+16+0)
	ld h,(ix+16+1)
	ld e,(ix+16+2)
	ld d,(ix+16+3)
	pop ix
	ret

; ==========================================================================
; FileGetPointer
; --------------------------------------------------------------------------
; Read the sequential pointer of an open file.
; --------------------------------------------------------------------------
; Inputs:     IX: Pointer to the file variable data, where
;             IX+0: LSB of pointer to block storage data.
;             IX+1: MSB of pointer to block storage data.
;             IX+2: File system.
;             IX+3: File status (0:closed, 1:OPENOUT, 2:OPENIN, 3:OPENUP).
; Outputs:    DEHL: the 32-bit pointer.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
FileGetPointer:
	push ix
	ld l,(ix+0)
	ld h,(ix+1)
	push hl
	pop ix
	ld l,(ix+16+4+0)
	ld h,(ix+16+4+1)
	ld e,(ix+16+4+2)
	ld d,(ix+16+4+3)
	pop ix
	ret

; ==========================================================================
; SetPointer
; --------------------------------------------------------------------------
; Update the sequential pointer of an open file.
; --------------------------------------------------------------------------
; Inputs:     IX: Pointer to the file variable data, where
;             IX+0: LSB of pointer to block storage data.
;             IX+1: MSB of pointer to block storage data.
;             IX+2: File system.
;             IX+3: File status (0:closed, 1:OPENOUT, 2:OPENIN, 3:OPENUP).
;             DEHL: the new 32-bit pointer.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
FileSetPointer:
	
	push ix
	ld c,(ix+0)
	ld b,(ix+1)
	push bc
	pop ix
	
	; DEHL must be <= file size
	ld a,(ix+16+3)
	cp d
	jr c,FileSetPointer.IncreasingSize
	ld a,(ix+16+2)
	cp e
	jr c,FileSetPointer.IncreasingSize
	ld a,(ix+16+1)
	cp h
	jr c,FileSetPointer.IncreasingSize
	ld a,(ix+16+0)
	cp l
	jr c,FileSetPointer.IncreasingSize
	
	; We're not increasing the size, just moving the pointer.
	ld (ix+16+4+0),l
	ld (ix+16+4+1),h
	ld (ix+16+4+2),e
	ld (ix+16+4+3),d
	pop ix
	ret
	
FileSetPointer.IncreasingSize:
	pop ix
	bit 1,(ix+0)
	jp z,File.ReadOnly ; EOF would seem more sensible...
	
	; TODO: Allow file size to be increased.
	jp Host.DeviceFault

; ==========================================================================
; FileIsEOF
; --------------------------------------------------------------------------
; Determines whether an open file pointer is at the end-of-file.
; --------------------------------------------------------------------------
; Inputs:     IX: Pointer to the file variable data, where
;             IX+0: LSB of pointer to block storage data.
;             IX+1: MSB of pointer to block storage data.
;             IX+2: File system.
;             IX+3: File status (0:closed, 1:OPENOUT, 2:OPENIN, 3:OPENUP).
; Outputs:    F: Z if at the end-of-file, NZ if not.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
FileIsEOF:
	ld l,(ix+0)
	ld h,(ix+1)
	ld bc,16
	add hl,bc
	ld e,l
	ld d,h
	ld bc,4
	add hl,bc
	
	; HL->PTR
	; DE->EXT
	ld b,4
-:	ld a,(de)
	cp (hl)
	ret nz
	inc hl
	inc de
	djnz -
	ret


.endmodule