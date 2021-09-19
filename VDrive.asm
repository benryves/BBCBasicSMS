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

.endmodule