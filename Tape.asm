; ==========================================================================
; Tape
; --------------------------------------------------------------------------
; Implements the tape filing system.
; ==========================================================================
.module Tape

; Basic format:
; - 0 bit = one cycle of 1200Hz.
; - 1 bit = two cycles of 2400Hz.

; Z80 CPU Frequency = 3546893 Hz (PAL), 3579540 Hz (NTSC)
; Assume it's 3563217

; 4800 :  742
; 2400 : 1485
; 1200 : 2969

; Data byte:
; - 0 dl....dh 1

; Pins:
; MIC-> = 7 Port B TH ($3F.7)
; <-EAR = 2 Port B Down ($DC.7)
; MOTOR = 9 Port B TR ($3F.6)

Options   = allocVar(1)

InputPort = $DC
InputBit  = 7

OutputPort = $3F
OutputBit  = 7
MotorBit   = 6

FullWaveLengthThreshold = 45

Header.LoadAddress      = 0
Header.ExecutionAddress = 4
Header.BlockNumber      = 8
Header.DataBlockLength  = 10
Header.BlockFlag        = 12
Header.NextFileAddress  = 13
Header.CRC              = 17
Header.DataCRC          = 19

LastBlockNumber     = $DDFE
LastBlockFlag       = $DDFD
LastBlockName       = $DDF0
LoadBlockNumber     = $DDEE
LoadBlockName       = $DDE3
LoadBlockError      = $DDE2
LoadBlockReport     = $DDE0

; ==========================================================================
; Reset
; --------------------------------------------------------------------------
; Resets the tape system to its default state.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; Interrupts: Enabled.
; ==========================================================================
Reset:
	
	di
	ld a,(IOControl)
	
	; Weakly pull the data output and motor outputs high.
	or (1 << OutputBit) | (1 << MotorBit) | (1 << (OutputBit - 4)) | (1 << (MotorBit - 4))
	
	ld (IOControl),a
	out ($3F),a
	
	ld a,%00000101
	ld (Options),a
	
	ei
	ret

; ==========================================================================
; SetMotorState
; --------------------------------------------------------------------------
; Switch the tape motor on or off.
; --------------------------------------------------------------------------
; Inputs:     L: 0 to switch the motor off, 1 to switch it on.
;             H: 0 for write operations, 1 for read operations.
; Destroyed:  AF.
; Interrupts: Enabled.
; ==========================================================================
SetMotorState:
	ld a,l
	or a
	jr z,MotorOff
	; Fall-through to MotorOn.

; ==========================================================================
; MotorOn
; --------------------------------------------------------------------------
; Switch the tape motor on.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; Interrupts: Enabled.
; ==========================================================================
MotorOn:
	di
	ld a,(IOControl)
	and ~((1 << MotorBit) | (1 << (MotorBit - 4)))
 	ld (IOControl),a
	out ($3F),a
	ei
	ret

; ==========================================================================
; MotorOff
; --------------------------------------------------------------------------
; Switch the tape motor off.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; Interrupts: Enabled.
; ==========================================================================
MotorOff:
	di
	ld a,(IOControl)
	or (1 << MotorBit) | (1 << (MotorBit - 4))
	ld (IOControl),a
	out ($3F),a
	ei
	ret

; ==========================================================================
; GetWaveLength
; --------------------------------------------------------------------------
; Counts the length of a complete wave cycle.
; --------------------------------------------------------------------------
; Outputs:    F: Z if there was a timeout, NZ if the wave was timed.
;             B: Length of the wave.
; Destroyed:  AF, BC.
; Interrupts: Disabled.
; ==========================================================================
GetWaveLength:
	di
	ld b,0
	
	; Wait for the level to go low.
-:	in a,(InputPort)
	bit InputBit,a
	jr z,+
	inc b
	jr nz,-
	
	; Time out.
	ret
	
+:	ld c,a
	ld b,0
	
	; Get the first half of the wave.
	
-:	in a,(InputPort)
	xor c
	and 1 << InputBit
	jr nz,+
	inc b
	jr nz,-
	
	; Time out.
	ret
	
+:
	
	; Get the second half of the wave.
	
	ld a,c
	cpl
	ld c,a

-:	in a,(InputPort)
	xor c
	and 1 << InputBit
	jr nz,+
	inc b
	jr nz,-
	
	; Time out.
	ret
	
+:
	
	; Convert bit length to bit value.
	ld a,b
	cp FullWaveLengthThreshold
	ret nz
	
	inc a ; Force NZ if Z before.
	ret

; ==========================================================================
; GetBit
; --------------------------------------------------------------------------
; Gets a bit from the tape.
; --------------------------------------------------------------------------
; Outputs:    F: Z if there was a timeout, NZ if the bit was received.
;                C is the bit value.
; Destroyed:  AF, BC.
; Interrupts: Disabled.
; ==========================================================================
GetBit:

	call GetWaveLength
	ret z
	ret nc ; If it's a 0 bit, it's 1x 1200Hz.
	call GetWaveLength
	ret

; ==========================================================================
; GetByte
; --------------------------------------------------------------------------
; Gets a byte from the tape.
; --------------------------------------------------------------------------
; Outputs:    F: Z if there was a timeout, NZ if the bit was received.
;             A: The received byte value.
; Destroyed:  AF, BC.
; Interrupts: Disabled.
; ==========================================================================
GetByte:
	
	ld b,0
	
-:	push bc
	call GetBit
	pop bc
	
	ret z ; No bit
	
	jr nc,GotStartBit
	
	inc b
	ret z
	jr -

GotStartBit:

	; Fetch 8 data bits.
	ld bc,8<<8
	
-:	push bc
	call GetBit
	pop bc
	ret z
	
	rr c
	djnz -

	; We now have our byte!
	; Get the stop bit.
	
	push bc
	call GetBit
	pop bc
	ret z
	
	ld a,c
	ret c
	
	; If it's not a 1 bit, it's not a stop bit!
	cp c
	ret

; ==========================================================================
; GetBlock
; --------------------------------------------------------------------------
; Gets a complete block from the tape.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to storage for the block's header.
;             DE: Pointer to storage for the block data.
; Outputs:    F: Z if there was an error.
;             IX: Address of the block's data structure.
; Destroyed:  AF, BC, DE, HL.
; Interrupts: Disabled.
; ==========================================================================
GetBlock:
	
	; Wait for the synchronisation byte.
	ld b,0
-:	push bc
	call GetByte
	pop bc
	jr nz,+
	djnz -
	
	xor a ; Timed out
	ret

+:	; All blocks must start with the synchronisation byte.
	cp $2A
	jr nz,-
	
	; Now read the header!

	; Variable length filename, NUL terminated.
	ld b,11
-:	push bc
	call GetByte
	pop bc
	ret z
	ld (hl),a
	inc hl
	or a
	jr z,+
	djnz -
	
	xor a ; File name > 10 characters
	ret
	
+:	
	
	; Set IX to point to the header data after the file name.	
	push hl
	pop ix
	
	; 19 bytes of further data.
	ld b,19
	
-:	push bc
	call GetByte
	pop bc
	ret z
	ld (hl),a
	inc hl
	djnz -

	; Read the data block length.
	ld c,(ix+Header.DataBlockLength+0)
	ld b,(ix+Header.DataBlockLength+1)
	
	ld a,b
	or c
	jr z,NoData
	
	; BC can now be between 1..256
	dec bc
	ld a,b
	or a
	jr z,NotTooMuchData

TooMuchData:
	xor a
	ret	

NoData:
	inc a ; Set NZ
	ret

NotTooMuchData:
	inc bc ; Restore BC back to its 1..256 range.

ReadData:
	push bc
	call GetByte
	pop bc
	ret z
	ld (de),a
	inc de
	dec bc
	ld a,b
	or c
	jr nz,ReadData
	
	; Get the CRC
	call GetByte
	ret z
	ld (ix+Header.DataCRC+0),a
	
	call GetByte
	ret z
	ld (ix+Header.DataCRC+1),a
	
	ret
	
	; CRC routine from http://regregex.bbcmicro.net/crc-code.htm
	
        ; CRC-16/XMODEM for 8080/Z80
        ; On entry HL = old CRC, A = byte
        ; On exit HL = new CRC, A,B,C undefined

                                ; Ts  M/code    8080 assembly
crc16_xmodem_f:
        XOR     H               ;  4  AC        XRA     H
        LD      B,A             ;  4  47        MOV     B,A
        LD      C,L             ;  4  4D        MOV     C,L
        RRCA                    ;  4  0F        RRC
        RRCA                    ;  4  0F        RRC
        RRCA                    ;  4  0F        RRC
        RRCA                    ;  4  0F        RRC
        LD      L,A             ;  4  6F        MOV     L,A
        AND     0FH             ;  7  E6 0F     ANI     0FH
        LD      H,A             ;  4  67        MOV     H,A
        XOR     B               ;  4  A8        XRA     B
        LD      B,A             ;  4  47        MOV     B,A
        XOR     L               ;  4  AD        XRA     L
        AND     F0H             ;  7  E6 F0     ANI     F0H
        LD      L,A             ;  4  6F        MOV     L,A
        XOR     C               ;  4  A9        XRA     C
        ADD     HL,HL           ; 11  29        DAD     H
        XOR     H               ;  4  AC        XRA     H
        LD      H,A             ;  4  67        MOV     H,A
        LD      A,L             ;  4  7D        MOV     A,L
        XOR     B               ;  4  A8        XRA     B
        LD      L,A             ;  4  6F        MOV     L,A
        RET                     ; 10  C9        RET

        ; 115 T-states, 25 bytes

; ==========================================================================
; CRC16
; --------------------------------------------------------------------------
; Calculates the CRC16 for a block of data.
; --------------------------------------------------------------------------
; Inputs:     DE: Pointer to address to calculate the CRC16 for.
;             BC: Number of bytes to calculate the CRC16 for.
; Outputs:    HL: CRC16 calculated for the data block.
; Destroyed:  AF, BC, DE.
; ==========================================================================
CRC16:
	ld hl,0
-:	ld a,(de)
	push bc
	call crc16_xmodem_f
	pop bc
	inc de
	dec bc
	ld a,b
	or c
	jr nz,-
	ret

; ==========================================================================
; Catalogue
; --------------------------------------------------------------------------
; Shows a list of files on the tape.
; --------------------------------------------------------------------------
; Outputs:    F: NZ if there was a protocol/receive error.
;                C is set if the transfer was cancelled.
; Destroyed:  AF, BC, DE, HL.
; Interrupts: Disabled.
; ==========================================================================
Catalogue:
	ld de,512
	call Host.GetSafeScratchMemoryDE
	jr nc,+
	xor a
	dec a
	ret

+:	; We'll reserve the low 128 bytes for scrolling operations.
	ld hl,128
	add hl,de
	ex de,hl
	ld hl,0
	ld bc,384
	; Fall-through to GetFile.

; ==========================================================================
; GetFile
; --------------------------------------------------------------------------
; Gets a file from the tape.
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

	ld (TempSize),hl
	ld (TempCapacity),bc
	ld (TempPtr),de
	
	ld a,l
	or h
	jr z,GetFile.ValidFilename
	
	ld a,(hl)
	or a
	jr z,GetFile.ValidFilename
	cp '\r'
	jr z,GetFile.ValidFilename
	
	call ValidateFilename
	jp nz,Host.BadString
	

GetFile.ValidFilename:
	
	push ix

	call MotorOn
	
	; We always want to start loading from block 0.
	ld hl,0
	ld (LoadBlockNumber),hl
	
	; Pretend we're loading so that the below routine to forces a change of state.
	ld a,(Host.Flags)
	set Host.Loading,a
	ld (Host.Flags),a
	
GetFile.SetSearching:

	; Set state to "searching"
	ld a,(Host.Flags)
	bit Host.Loading,a
	jr z,GetFile.AlreadySearching
	
	res Host.Loading,a
	ld (Host.Flags),a

	; Set last block name and number = 0
	ld hl,0
	ld (LastBlockNumber),hl
	ld (LastBlockName),hl
	
	; Set all the last block flags.
	xor a
	cpl
	ld (LastBlockFlag),a
	
	; Is the filename NULL?
	ld hl,(TempSize)
	ld a,h
	or l
	jr z,+
	
	; If not, print "Searching".
	ld hl,Searching
	.bcall "VDU.PutStringWithNewLines"
+:

GetFile.AlreadySearching:
GetFile.AwaitNextBlock:

	; Check the amount of free space.
	ld hl,(TempCapacity)
	ld de,384
	or a
	sbc hl,de
	jr nc,GetFile.CheckEscape
	
	call MotorOff
	pop ix
	xor a
	dec a
	scf
	ret

GetFile.CheckEscape:

	; Check Escape
	.bcall "VDU.BeginBlinkingCursor"

GetFile.CheckEscapeLoop:
	ei
	halt
	pop ix
	call KeyboardBuffer.GetDeviceKey
	call Host.CheckEscape
	push ix
	.bcall "VDU.DrawBlinkingCursor"
	
	ld b,10
-:	push bc
	call GetBit
	pop bc
	jr z,GetFile.CheckEscapeLoop  ; No bit.
	jr nc,GetFile.CheckEscapeLoop ; Zero bit, not carrier.
	djnz -
	
	; If we get this far, we just received a long string of "1" bits in a row.
	; It's the carrier!
	.bcall "VDU.EndBlinkingCursor"
	
	; Set up DE->data, HL->header
	ld de,(TempPtr)
	ld h,d
	ld l,e
	inc h
	
	; Assume there are no errors.
	xor a
	ld (LoadBlockError),a
	
	; Read the block.
	call GetBlock
	jr z,GetFile.AwaitNextBlock
	
	; Check the header CRC16.
	ld de,(TempPtr)
	inc d
	scf
	dec hl
	sbc hl,de
	ld c,l
	ld b,h
	call CRC16
	
	ld d,(ix+Header.CRC+0) ; \ Intentionally byte-swapped.
	ld e,(ix+Header.CRC+1) ; /
	or a
	sbc hl,de
	jr z,+
	
	ld a,217
	ld (LoadBlockError),a
	ld hl,HeaderError
	ld (LoadBlockReport),hl	
+:

	; Check the data CRC16.
	ld c,(ix+Header.DataBlockLength+0)
	ld b,(ix+Header.DataBlockLength+1)
	ld a,b
	or c
	jr z,GetFile.NoDataCRC
	
	ld de,(TempPtr)
	call CRC16
	
	ld d,(ix+Header.DataCRC+0) ; \ Intentionally byte-swapped.
	ld e,(ix+Header.DataCRC+1) ; / 
	or a
	sbc hl,de
	jr z,+
	
	ld a,216
	ld (LoadBlockError),a
	ld hl,DataError
	ld (LoadBlockReport),hl
	
+:


GetFile.NoDataCRC:

	; Are we loading a file?
	
	; We can't load if there's an error.
	ld a,(Options)
	and %00001100
	jr z,+
	ld a,(LoadBlockError)
	or a
	jr nz,GetFile.NotLoading
+:
	
	ld hl,(TempSize)
	ld a,h
	or l
	jr z,GetFile.NeverLoading
	
	; Is the filename empty?
	ld a,(hl)
	or a
	jr z,GetFile.EmptyFilename
	cp '\r'
	jr nz,GetFile.NonEmptyFilename

GetFile.EmptyFilename:
	
	; Is this the first block of the file? If so, pretend that's what we're trying to load.
	ld a,(ix+Header.BlockNumber+0)
	or (ix+Header.BlockNumber+1)
	jr nz,GetFile.NonEmptyFilename
	
	ld hl,(TempPtr)
	inc h
	ld de,LoadBlockName
	ld (TempSize),de
	push de
	ld bc,11
	ldir
	pop hl

GetFile.NonEmptyFilename:

	; HL -> filename to load.
	ld a,(Host.Flags)
	bit Host.Loading,a
	jr nz,GetFile.AlreadyLoading
	
	; We're not loading. Should we load?
	ld de,(TempPtr)
	inc d
	
	call CompareFilename
	jr nz,GetFile.NotLoading

	ld hl,(LoadBlockNumber)
	ld e,(ix+Header.BlockNumber+0)
	ld d,(ix+Header.BlockNumber+1)
	or a
	sbc hl,de
	jr nz,GetFile.NotLoading
	
	; We are now loading!
	ld a,(Host.Flags)
	set Host.Loading,a
	ld (Host.Flags),a
	
	ld hl,Loading
	.bcall "VDU.PutStringWithNewLines"

GetFile.NotLoading:
GetFile.AlreadyLoading:
GetFile.NeverLoading:
	
	; Has the block name changed since last time?
	ld de,LastBlockName
	ld hl,(TempPtr)
	inc h
	
	call CompareFilename
	jr z,GetFile.SameBlockFilename
	
GetFile.NewBlockFilename:

	; The block name has changed, so move to the next line.
	push af
	.bcall "VDU.CursorDown"
	pop af

GetFile.SameBlockFilename:
	
	; Print header information.
	push af
	.bcall "VDU.HomeLeft"
	ld hl,(TempPtr)
	inc h
	call PrintBlockDetails
	pop af
	
	; Check if the block name and number matched our expectations.
	jr nz,GetFile.BlockNameOrNumberChanged
	
	ld hl,(LastBlockNumber)
	inc hl
	ld e,(ix+Header.BlockNumber+0)
	ld d,(ix+Header.BlockNumber+1)
	or a
	sbc hl,de
	jr z,GetFile.BlockNameAndNumberAsExpected

GetFile.BlockNameOrNumberChanged:

	; Either the block name OR the block number are a surprise to us now.
	
	; If we just got the first block (and were expecting that), then a mismatched filename is to be expected.
	ld a,(LastBlockFlag)
	bit 7,a
	jr z,GetFile.TriggerBlockError
	inc a
	jr z,GetFile.BlockNameAndNumberAsExpected
	
	ld hl,(LastBlockNumber)
	ld a,h
	or l
	or (ix+Header.BlockNumber+0)
	or (ix+Header.BlockNumber+1)
	jr z,GetFile.BlockNameAndNumberAsExpected
	
	; Block error.
GetFile.TriggerBlockError:
	ld a,218
	ld (LoadBlockError),a
	ld hl,BlockError
	ld (LoadBlockReport),hl

GetFile.BlockNameAndNumberAsExpected:
	
	ld a,(LoadBlockError)
	or a
	jr nz,GetFile.SkipBlockNameNumberDoubleCheck
	
	; If we're loading, then double check the load block name/number.
	ld a,(Host.Flags)
	bit Host.Loading,a
	jr z,GetFile.GotValidBlockButNotLoading
	
	; Double check the load block name.
	ld de,(TempPtr)
	inc d
	ld hl,(TempSize)
	call CompareFilename
	jr nz,GetFile.TriggerBlockError
	
	; Double check the load block number.
	ld de,(LoadBlockNumber)
	ld l,(ix+Header.BlockNumber+0)
	ld h,(ix+Header.BlockNumber+1)
	or a
	sbc hl,de
	jr nz,GetFile.TriggerBlockError
	
GetFile.GotValidBlockButNotLoading:
GetFile.SkipBlockNameNumberDoubleCheck:
	
	; If there's an error, handle it.
	ld hl,(LoadBlockReport)
	ld a,(LoadBlockError)
	or a
	jr z,GetFile.NoLoadingError
	
	; Display information about the error.
	ld a,(Options)
	and %00001100
	jr z,GetFile.IgnoreError
	
	cp %00001000
	jr nz,GetFile.IgnoreError
	
	; Trigger an error.
	pop ix
	inc hl
	push hl
	ld a,(LoadBlockError)
	jp Basic.BBCBASIC_EXTERR
	

GetFile.IgnoreError:
	.bcall "VDU.PutStringWithNewLines"
	call GetFile.RememberLastBlockNameAndNumber
	
	; Ignore the error if appropriate.
	ld a,(Options)
	and %00001100
	jr z,GetFile.NoLoadingError
	
	; Prompt to fix the error then start searching again.
	ld a,(Host.Flags)
	bit Host.Loading,a
	jp z,GetFile.SetSearching
	ld hl,RewindTape
	.bcall "VDU.PutStringWithNewLines"
	jp GetFile.SetSearching

GetFile.NoLoadingError:

	; Have we reached the end of the file yet?
	bit 7,(ix+Header.BlockFlag)
	jr z,GetFile.NotEndOfFile

GetFile.EndOfFile:
	; If we've reached the end of a file, we won't know anything about the next block.
	ld hl,0
	ld (LastBlockNumber),hl
	ld (LastBlockName),hl
	
	ld a,(ix+Header.BlockFlag)
	ld (LastBlockFlag),a
	
	; Are we loading?
	ld a,(Host.Flags)
	bit Host.Loading,a
	jp z,GetFile.AwaitNextBlock
	
	; We've loaded the whole file successfully!
	ld a,'\r'
	.bcall "VDU.PutChar"
	
	call MotorOff
	
	pop ix
	xor a
	ret

GetFile.NotEndOfFile:	
	
	call GetFile.RememberLastBlockNameAndNumber
	
	; If we're loading, then increment the load block number and target pointer.
	ld a,(Host.Flags)
	bit Host.Loading,a
	jp z,GetFile.AwaitNextBlock
	
	; Increment the load block number.
	ld de,(LoadBlockNumber)
	inc de
	ld (LoadBlockNumber),de
	
	; Increase the target pointer by the block size.
	ld hl,(TempPtr)
	ld e,(ix+Header.DataBlockLength+0)
	ld d,(ix+Header.DataBlockLength+1)
	add hl,de
	ld (TempPtr),hl
	
	; Decrease the amount of free space by the block size.
	ld hl,(TempCapacity)
	or a
	sbc hl,de
	ld (TempCapacity),hl
	
	jp GetFile.AwaitNextBlock


GetFile.RememberLastBlockNameAndNumber:
	; Copy the last block name and number.
	ld hl,(TempPtr)
	inc h
	ld de,LastBlockName
	ld bc,11
	ldir
	
	ld l,(ix+Header.BlockNumber+0)
	ld h,(ix+Header.BlockNumber+1)
	ld (LastBlockNumber),hl
	
	ld a,(ix+Header.BlockFlag)
	ld (LastBlockFlag),a
	ret

Searching:
.db "Searching\r",0
Loading:
.db "\rLoading\r",0

DataError:
.db "\rData?\r",0
HeaderError:
.db "\rHeader?\r",0
BlockError:
.db "\rBlock?\r",0
RewindTape:
.db "Rewind tape\r\r",0

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
	push bc
	
	; Check that the name is not empty.
	ld a,(hl)
	call NormaliseFilenameCharacter
	or a
	jr z,ValidateFilename.Invalid
	
	; Maximum length + terminator.
	ld b,11
-:	ld a,(hl)
	inc hl
	call NormaliseFilenameCharacter
	or a
	jr z,ValidateFilename.Valid
	jp m,ValidateFilename.Invalid
	cp 33
	jr c,ValidateFilename.Invalid
	djnz -
	
ValidateFilename.Invalid:
	xor a
	dec a
	pop bc
	ret
	
ValidateFilename.Valid:
	xor a
	pop bc
	ret

; ==========================================================================
; CompareFilename
; --------------------------------------------------------------------------
; Compares two filenames for equality.
; --------------------------------------------------------------------------
; Inputs:     HL: One filename to compare.
;             DE: The filename to compare it to.
; Outputs:    F: Z set if the file names are equal.
; Destroyed:  AF, HL, DE.
; ==========================================================================
CompareFilename:
	push bc
	ld b,11
-:	ld a,(hl)
	call NormaliseFilenameCharacter
	ld c,a
	ld a,(de)
	call NormaliseFilenameCharacter
	cp c
	jr nz,+
	or a
	jr z,+
	inc hl
	inc de
	djnz -
+:	pop bc
	ret

; ==========================================================================
; NormaliseFilenameCharacter
; --------------------------------------------------------------------------
; Converts lowercase characters to uppercase and CR to NUL to assist in 
; filename comparisons.
; --------------------------------------------------------------------------
; Inputs:     A: Character to normalise.
; Destroyed:  F.
; ==========================================================================
NormaliseFilenameCharacter:
	or a
	ret z
	cp '\r'
	jr nz,+
	xor a
	ret
+:	cp 'a'
	ret c
	cp 'z'*1+1
	ret nc
	and ~('a'-'A')
	ret

InvertOutput:
	ld a,(IOControl)
	xor 1 << OutputBit
	ld (IOControl),a
	out ($3F),a
	ret

MaintainOutput:
	ld a,(IOControl)
	xor 0
	ld (IOControl),a
	out ($3F),a
	ret

; ==========================================================================
; WriteBit
; --------------------------------------------------------------------------
; Writes a single bit to the tape.
; --------------------------------------------------------------------------
; Inputs:     F: C is the bit (set or reset for 1 or 0) to send.
; Destroyed:  AF.
; ==========================================================================
WriteBit:
	jp c,WriteBit1

WriteBit0: ; 1x 1200Hz cycle.
	call InvertOutput
	call BitDelayFull
	call MaintainOutput
	call BitDelayFull
	call InvertOutput
	call BitDelayFull
	call MaintainOutput
	call BitDelayShort
	ret

WriteBit1: ; 2x 2400Hz cycles.
	call InvertOutput
	call BitDelayFull
	call InvertOutput
	call BitDelayFull
	call InvertOutput
	call BitDelayFull
	call InvertOutput
	call BitDelayShort
	ret

BitDelayFull:
	push bc
	ld b,47
-:	djnz -
	dec bc
	pop bc
	nop
	ret

BitDelayShort:
	push bc
	ld b,20
-:	djnz -
	inc bc
	pop bc
	scf ; Stop bit!
	ret

; ==========================================================================
; WriteByte
; --------------------------------------------------------------------------
; Writes a byte to the tape with the bits evenly spaced.
; --------------------------------------------------------------------------
; Inputs:     A: The byte to write.
; Destroyed:  AF.
; ==========================================================================
WriteByte:
	push bc

.if 0
	push hl
	push de
	.bcall "VDU.PutHexByte"
	ld a,' '
	.bcall "VDU.PutChar"
	ld a,' '
	.bcall "VDU.PutChar"
	pop de
	pop hl
.endif

	ld c,a
	ld b,9 ; start bit, 8 data bits

	or a
	
	; Start/data bit + long delay
-:	call WriteBit
	rr c
	call ByteDelayFull
	djnz -
	
	; Stop bit + short delay
	nop
	call WriteBit
	call ByteDelayShort
	
	pop bc
	ret

ByteDelayFull:
	push bc
	ld b,18
-:	djnz -
	ld a,r
	pop bc
	ret

ByteDelayShort:
	push bc
	nop
	inc bc
	nop
	pop bc
	ret

; ==========================================================================
; WriteBytes
; --------------------------------------------------------------------------
; Writes a single block of bytes to the tape with the bytes evenly spaced.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to the block of bytes to write.
;             BC: The number of bytes to write (1..65536).
; Destroyed:  AF, BC, HL.
; ==========================================================================
WriteBytes:
-:	ld a,(hl)
	call WriteByte
	inc hl
	dec bc
	ld a,b
	or c
	jr z,+
	call BytesDelayFull
	jr -
+:	push bc
	pop bc
	nop
	nop
	ret

BytesDelayFull:
	push bc
	ld b,6
-:	djnz -
	or 0
	nop
	nop
	pop bc
	ret

; ==========================================================================
; WriteBytesList
; --------------------------------------------------------------------------
; Writes multiple blocks of bytes to the tape with the bytes evenly spaced.
; The list of blocks must take the form of a 16-bit pointer to the data
; followed by the 16-bit number of bytes in the block.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to the list of blocks of bytes to write.
;             B: The number of entries in the list.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
WriteBytesList:
-:	push bc
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ex de,hl
	call WriteBytes
	ex de,hl
	pop bc
	djnz -
	ret

; ==========================================================================
; WriteBytesListWithCarrier
; --------------------------------------------------------------------------
; Writes multiple blocks of bytes to the tape with the bytes evenly spaced.
; The list of blocks must take the form of a 16-bit pointer to the data
; followed by the 16-bit number of bytes in the block.
; A carrier sequence is inserted at the start and end of the data written to
; tape.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to the list of blocks of bytes to write.
;             B: The number of entries in the list.
;             D: Length of the carrier at the start of the data list (1/50s)
;             E: Length of the carrier at the end of the data list (1/50s)
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
WriteBytesListWithCarrier:
	push de
	
	; Write the lead-in carrier.
--:	ld c,24
-:	call WriteBit1
	dec c
	jr z,+
	call CarrierDelayFull
	jr -
+:	dec d
	jr z,+
	call CarrierDelayShort
	jr --
+:	
	
	; Delay to pad between lead-in carrier and data block.
	push bc
	ld b,8
-:	djnz -
	nop
	pop bc

	; Write the data.
	call WriteBytesList
	
	; Delay to pad between data block and lead-out carrier.
	push bc
	ld b,7
-:	djnz -
	rr b
	pop bc
	
	; Write the lead-out carrier.
	pop de
--:	ld c,24
-:	call WriteBit1
	dec c
	jr z,+
	call CarrierDelayFull
	jr -
+:	dec e
	jr z,+
	call CarrierDelayShort
	jr --
+:	
	ret

CarrierDelayFull:
	push bc
	ld b,19
-:	djnz -
	nop
	pop bc
	ret

CarrierDelayShort:
	push bc
	ld b,17
-:	djnz -
	or 0
	pop bc
	ret

; ==========================================================================
; WriteFile
; --------------------------------------------------------------------------
; Writes a file to the tape.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to the file name NUL/CR terminated.
;             DE: Pointer to RAM to get the file data from.
;             BC: Size of the file to write.
; Destroyed:  AF, BC, DE, HL.
; Interrupts: Enabled.
; ==========================================================================
WriteFile:
	
	; Validate the filename.
	push hl
	call ValidateFilename
	pop hl
	jp nz,Host.BadString

	; We'll need to remember the file location and size.
	ld (TempPtr),de
	ld (TempSize),bc
	
	; Get a free block of memory to store the header/data/CRC in.
	ld de,64
	call Host.GetSafeScratchMemoryDE
	ret c
	
	; Display the "RECORD then RETURN" prompt.
	push hl
	ld hl,RecordThenReturn
	.bcall "VDU.PutString"
-:	call Host.CheckEscape
	call Host.OSRDCH
	cp '\r'
	jr nz,-
	.bcall "VDU.Console.NewLine"
	pop hl
	
	; Remember the old value of FREE.
	ld bc,(Basic.BBCBASIC_FREE)
	ld (TempCapacity),bc
	
	; Move the FREE pointer in case future operations also need to make use it.
	; (e.g. scrolling text).
	push hl
	ld hl,64
	add hl,de
	ld (Basic.BBCBASIC_FREE),hl
	pop hl
	
	; Back up IX and IY as we'll be modifying them later.
	push ix
	push iy
	
	; Each bytes list will require:
	; Sync byte + Header + Header CRC
	; Data
	; Data CRC
	
	ex de,hl
	
	; Remember the pointer to the header.
	push hl
	
	; Synchronisation byte.
	ld (hl),$2A 
	inc hl
	
	; File name.
-:	ld a,(de)
	or a
	jr z,+
	cp '\r'
	jr z,+
	
	ld (hl),a
	inc hl
	inc de
	jr -
+:	
	; File name terminator.
	ld (hl),0
	inc hl
	
	; We'll need to access the fields below when writing each block,
	; so remember the address in IX.
	push hl
	pop ix
	
	; Load address and execution address.
	ld b,2
	
-:	ld a,(TempPtr+0)
	ld (hl),a
	inc hl
	
	ld a,(TempPtr+1)
	ld (hl),a
	inc hl
	
	xor a
	ld (hl),a
	inc hl
	
	xor a
	ld (hl),a
	inc hl
	
	djnz -
	
	; Write placeholder data for:
	; Block number (2 bytes)
	; Data block length (2 bytes).
	; Block flag (1 byte).
	; Address of next file (4 bytes).
	; Header CRC (2 bytes, byte swapped).
	ld b,2+2+1+4+2
-:	ld (hl),a
	inc hl
	djnz -
	
	; Restore where the end of the header is (DE).
	ld d,h
	ld e,l
	
	; How long is the header block?
	pop bc
	or a
	sbc hl,bc
	ex de,hl
	
	; HL = End of header data.
	; BC = Start of header data.
	; DE = Size of header data.
	
	; We'll be writing our list of byte sequences here,
	; so remember where that is.
	push hl
	pop iy
	
	ld (hl),c
	inc hl
	ld (hl),b
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	
	; We'll need to write the actual data to the tape after the header.
	ld a,(TempPtr+0)
	ld (hl),a
	inc hl
	ld a,(TempPtr+1)
	ld (hl),a
	inc hl
	
	ld a,(TempSize+0)
	ld (hl),a
	inc hl
	ld a,(TempSize+1)
	ld (hl),a
	inc hl
	
	; We'll need to store the data CRC somewhere too!
	ld d,h
	ld e,l
	ld bc,4
	add hl,bc
	ex de,hl
	
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ld (hl),2
	inc hl
	ld (hl),0
	inc hl
	
	; At this point, we've loaded our dummy block into memory and set up our bytes lists.
	
	; IX: Points to block header.
	; IY: Points to bytes list table.
	
	call MotorOn
	di
	
	; Make the data output pin an output and hold it high by default.
	ld a,(IOControl)
	and ~(1 << (OutputBit - 4))
	or 1 << OutputBit
	ld (IOControl),a
	out ($3F),a

WriteBlock:

	; What should the size of the block be?
	ld hl,(TempSize)
	ld a,h
	or l
	jr z,WriteEmptyBlock
	
	; If the block size > 256 bytes, only write 256.
	dec hl
	ld a,h
	or a
	jr z,+
	ld hl,255
+:	inc hl

WriteEmptyBlock:
	
	; Store the data block size in the header area.
	ld (ix+Header.DataBlockLength+0),l
	ld (ix+Header.DataBlockLength+1),h
	
	; Store the data block size in the bytes list table.
	ld (iy+6),l
	ld (iy+7),h
	

	; Update the block flag.
	ld (ix+Header.BlockFlag),0
	
	ld a,h
	or l
	jr nz,+
	set 6,(ix+Header.BlockFlag) ; bit 6 = 0 bytes in data block.
+:	
	
	; Is the amount of data we're writing equal to the size of the file remaining?
	ld de,(TempSize)
	or a
	sbc hl,de
	jr nz,+
	set 7,(ix+Header.BlockFlag) ; bit 7 = end of file.
+:
	
	; We'll need to compute the header's CRC.
	ld e,(iy+0)
	ld d,(iy+1)
	ld c,(iy+2)
	ld b,(iy+3)
	
	; Skip the sync byte.
	inc de
	dec bc
	
	; We don't want to include the CRC bytes in the CRC calculation, do we?
	dec bc
	dec bc
	
	call CRC16
	ld (ix+Header.CRC+0),h ; \ Intentionally byteswapped.
	ld (ix+Header.CRC+1),l ; /
	
	; Assume we have no data, and are only writing the block header.
	ld b,1
	
	; We'll need to compute the data CRC (if there is any!)
	bit 6,(ix+Header.BlockFlag)
	jr nz,+
	
	ld e,(iy+4)
	ld d,(iy+5)
	ld c,(iy+6)
	ld b,(iy+7)
	
	call CRC16
	
	ld (iy+12),h ; \ Intentionally byteswapped.
	ld (iy+13),l ; /
	
	; We have data, so will need to write the block header, data, and CRC.
	ld b,3
+:

	; How long should the carrier lead-in be?
	ld d,23 ; 0.46s.
	ld a,(ix+Header.BlockNumber+0)
	or (ix+Header.BlockNumber+1)
	jr nz,+
	ld d,255 ; 5.1s
+:

	; How long should the carrier lead-out be?
	ld e,23 ; 0.46s
	bit 7,(ix+Header.BlockFlag)
	jr z,+
	ld e,255 ; 5.1s
+:

	; Print the name of the block.
	.bcall "VDU.HomeLeft"
	ld l,(iy+0)
	ld h,(iy+1)
	inc hl
	call PrintBlockDetails
	di

	; Commit to tape.
	push iy
	pop hl
	call WriteBytesListWithCarrier
	
	; Have we finished?
	bit 7,(ix+Header.BlockFlag)
	jr nz,FinishedWriteFile
	
	; Advance to the next block number.
	ld l,(ix+Header.BlockNumber+0)
	ld h,(ix+Header.BlockNumber+1)
	inc hl
	ld (ix+Header.BlockNumber+0),l
	ld (ix+Header.BlockNumber+1),h
	
	; Advance the data pointer by the amount of data written.
	ld e,(ix+Header.DataBlockLength+0)
	ld d,(ix+Header.DataBlockLength+1)
	
	ld l,(iy+4)
	ld h,(iy+5)
	add hl,de
	ld (iy+4),l
	ld (iy+5),h
	
	; Decrement the amount of data remaining by the amount of data written.
	ld hl,(TempSize)
	or a
	sbc hl,de
	ld (TempSize),hl
	
	; Write the next block!
	jp WriteBlock

FinishedWriteFile:

	; Restore the BASIC FREE pointer.
	ld bc,(TempCapacity)
	ld (Basic.BBCBASIC_FREE),bc

	; Done!
	pop iy
	pop ix
	
	; Release the data output pin.
	ld a,(IOControl)
	or (1 << OutputBit) | (1 << (OutputBit - 4)) 
	ld (IOControl),a
	out ($3F),a
	
	.bcall "VDU.Console.NewLine"
	
	call MotorOff
	ret

RecordThenReturn:
.db "RECORD then RETURN", 0

; ==========================================================================
; PrintBlockDetails
; --------------------------------------------------------------------------
; Prints details about the current block to the screen.
; --------------------------------------------------------------------------
; Inputs:     HL: Pointer to the block header.
; Destroyed:  AF, HL.
; ==========================================================================
PrintBlockDetails:
	push bc
	push de
	
	ld b,11 ; Pad the file name to 11 characters.
	
-:	ld a,(hl)
	inc hl
	or a
	jr z,PrintedEndOfFilename
	
	; Only show printable characters here.
	jp p,+
	ld a,'?'
+:	cp 32
	jr nc,+
	ld a,'?'
+: 	.bcall "VDU.PutChar"
	dec b
	jr nz,-
	
	; If we get here, the filename is longer than 11 characters.
	; Try to find the end of the filename anyway...
-:	ld a,(hl)
	inc hl
	or a
	jr nz,-
	
PrintedEndOfFilename:
	
	; Pad the filename with spaces.
	ld a,b
	or a
	jr z,+

-:	ld a,' '
	.bcall "VDU.PutChar"
	djnz -
+:
	
	; Skip the load address, execution address.
	ld bc,8
	add hl,bc
	
	; Block number (LSB only).
	ld a,(hl)
	ld b,a
	inc hl
	inc hl
	.bcall "VDU.PutHexByte"
	
	; Block size.
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	
	; Block flag: is it EOF?
	bit 7,(hl)
	
	jr z,PrintBlockDetailsNameNumberOnly
	
	; Show the file size.
	ld a,' '
	.bcall "VDU.PutChar"
	
	; Add the block number * 256 to the current block size to get the final size.
	push hl
	ld h,b
	ld l,0
	add hl,de
	
	.bcall "VDU.PutHexWord"
	pop hl
	
	; Are we showing the start/execution addresses?
	ld a,(Options)
	and %00000011
	cp 2
	jr nz,PrintBlockDetailsNameNumberOnly
	
	; Three spaces.
	ld b,3
-:	ld a,' '
	.bcall "VDU.PutChar"
	djnz -
	
	; At this point, HL->block flag
	ld de,-2-2-4-4 ; block length, block number, execution address, load address
	add hl,de
	
	ld b,2
	
-:	ld a,' '
	.bcall "VDU.PutChar"
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	
	push de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	
	ex de,hl
	.bcall "VDU.PutHexWord"
	pop hl
	.bcall "VDU.PutHexWord"
	ex de,hl
	
	djnz -

PrintBlockDetailsNameNumberOnly:	
	pop de
	pop bc
	ret

.endmodule