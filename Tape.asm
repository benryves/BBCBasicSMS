.module Tape

; Basic format:
; - 0 bit = one cycle of 1200Hz.
; - 1 bit = two cycles of 2400Hz.

; Data byte:
; - 0 dl....dh 1

; Pins:
; MIC-> = 7 Port B TH ($3F.7)
; <-EAR = 2 Port B Down ($DC.7)
; MOTOR = 9 Port B TR ($3F.6)

InputPort = $DC
InputBit  = 7

HalfWaveLengthThreshold = 18
FullWaveLengthThreshold = 45

.define GetFileName          Basic.BBCBASIC_BUFFER+207 ; Pointer to the filename to retrieve.
.define GetFileAddress       Basic.BBCBASIC_BUFFER+209 ; Pointer to the start address to store the actual file.
.define GetFileExpectedBlock Basic.BBCBASIC_BUFFER+211 ; 2 byte block number we SHOULD get.
.define GetFileExpectedName  Basic.BBCBASIC_BUFFER+213 ; 10 character+NUL filename we SHOULD get.
.define GetFileHeader        Basic.BBCBASIC_BUFFER+224 ; Last-received file header.

Header.LoadAddress      = 0
Header.ExecutionAddress = 4
Header.BlockNumber      = 8
Header.DataBlockLength  = 10
Header.BlockFlag        = 12
Header.NextFileAddress  = 13
Header.CRC              = 17
Header.DataCRC          = 19

; ---------------------------------------------------------
; GetHalfWaveLength -> Gets the length of half of a wave.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  b = length of the half wave.
;           z and c set if the bit timed out. 
; Destroys: af, bc.
; ---------------------------------------------------------
GetHalfWaveLength:
	di
	
	; Count the length of the half-wave in B.
	ld b,0
	
	; We need to see where the wave changes, so remember the initial state.
	in a,(InputPort)
	ld c,a
	
-:	in a,(InputPort)   ; 11
	xor c              ; 4
	and 1 << InputBit  ; 7
	ret nz             ; 11/5
	inc b              ; 4
	jr nz,-            ; 12/7
	scf
	ret


; ---------------------------------------------------------
; GetFullWaveLength -> Gets the length of a wave cycle.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  b = length of the half wave.
;           z set if the bit timed out. 
;           c set if it was a short wave, cleared if it was
;           long.
; Destroys: af, bc.
; ---------------------------------------------------------
GetFullWaveLength:
	di
	ld b,0
	
	; Remember the initial state of the input port.
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

; ---------------------------------------------------------
; GetBit -> Gets a bit from the tape.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  c = the bit value.
;           z set if the bit timed out. 
; Destroys: af, bc.
; ---------------------------------------------------------
GetBit:

	call GetFullWaveLength
	ret z
	ret nc ; If it's a 0 bit, it's 1x 1200Hz.
	call GetFullWaveLength
	ret

; ---------------------------------------------------------
; GetByte -> Gets a byte from the tape.
; ---------------------------------------------------------
; Outputs:  a = the byte value.
;           z set if the byte timed out. 
; Destroys: af, bc.
; ---------------------------------------------------------
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

; ---------------------------------------------------------
; GetBlock -> Gets a complete block from the tape.
; ---------------------------------------------------------
; Inputs:   hl = pointer to storage for the block header.
;           de = pointer to storage for the block data.
; Outputs:  ix = address of block data structure.
;           z set if there was a problem receiving.
; Destroys: af, bc, de, hl.
; ---------------------------------------------------------
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

+:	cp $2A
	jr z,+
	
	; Not the synchronisation byte.
	xor a
	ret
	
+:
	
	; Now read the header!

	; Variable length filename, NUL terminated.
-:	call GetByte
	ret z
	ld (hl),a
	inc hl
	or a
	jr nz,-
	
	push hl
	
	; 19 bytes of further data.
	ld b,19
	
-:	push bc
	call GetByte
	pop bc
	ret z
	ld (hl),a
	inc hl
	djnz -
	
	pop ix

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
	
	inc a ; Set NZ
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

; ---------------------------------------------------------
; Catalogue -> Shows a list of files.
; ---------------------------------------------------------
; Outputs:  nz if there was a protocol/receive error.
;           if no error, c set if transfer was cancelled.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------
Catalogue:
	ld hl,0
	ld de,256
	call Host.GetSafeScratchMemoryDE
	ret c
	ld bc,512
	; Fall-through.

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
	push ix

	ld (GetFileName),hl
	ld (GetFileAddress),de
	ld (TempCapacity),bc
	ld bc,0
	ld (TempSize),bc
	
	ld a,h
	or l
	jr z,TapeSearchFileLoop
	
	ld hl,Searching
	.bcall "VDU.PutStringWithNewLines"

TapeSearchFileLoop:
	
	; We always want to start from block 0 and a zero-length file.
	ld hl,0
	ld (GetFileExpectedBlock),hl
	
	ld bc,0
	ld (TempSize),bc
	
	; ...and always start from the beginning of the available buffer.
	ld de,(GetFileAddress)
	ld (TempPtr),de

TapeBlockLoop:

	; Wait for carrier tone.
	.bcall "VDU.BeginBlinkingCursor"

TapeBlockAwaitCarrier:
	ei
	halt
	call KeyboardBuffer.GetDeviceKey
	call Host.CheckEscape
	.bcall "VDU.DrawBlinkingCursor"
	
	ld b,30
-:	push bc
	call GetBit
	pop bc
	jr z,TapeBlockAwaitCarrier  ; No bit.
	jr nc,TapeBlockAwaitCarrier ; Zero bit, not carrier.
	djnz -
	
	; If we get this far, we just received a long string of "1" bits in a row.
	; It's the carrier!
	.bcall "VDU.EndBlinkingCursor"
	
	ld de,(TempPtr)
	ld hl,GetFileHeader
	
	call Tape.GetBlock
	jr nz,TapeGotBlock
	jr TapeBlockLoop

CRCError:

	ld hl,DataError
	.bcall "VDU.PutStringWithNewLines"
	jr TapeBlockLoop
	
TapeGotBlock:
	
	; Are we loading block 0?
	ld a,(ix+Tape.Header.BlockNumber+0)
	or (ix+Tape.Header.BlockNumber+1)
	jr nz,TapeNotLoading
	
	; Is it the one we need to load?
	ld bc,(GetFileName)
	ld a,b
	or c
	jr z,TapeNotLoading
	
	; If it's the empty string, always load that.
	ld a,(bc)
	cp '\r'
	jr z,TapeStartLoading
	
	ld hl,GetFileHeader
-:	ld a,(bc)
	cp '\r'
	jr nz,+
	xor a
+:	cp (hl)
	jr nz,TapeNotLoading
	or a
	jr z,TapeStartLoading
	inc hl
	inc bc
	jr -
	
TapeStartLoading:

	ld hl,Loading
	.bcall "VDU.PutStringWithNewLines"
	
	; Blank out the filename to search for to indicate we've found it.
	ld hl,-1
	ld (GetFileName),hl

TapeNotLoading:
	; Print the name of the received block.
	.bcall "VDU.Console.HomeLeft"
	ld hl,GetFileHeader
	.bcall "VDU.PutString"
	
	; Pad to 11 characters.
	ld a,GetFileHeader+12
	sub l
	
	ld b,a
-:	ld a,' '
	.bcall "VDU.PutChar"
	djnz -
	
	; Display the block number in hex.
	ld a,(ix+Tape.Header.BlockNumber+0)
	.bcall "VDU.PutHexByte"
	
	; If we're not loading a program, we can ignore the block number check.
	ld hl,(GetFileName)
	inc hl
	ld a,h
	or l
	jr nz,GotValidBlockFileName
	
	; Is this the right block?
	ld hl,(GetFileExpectedBlock)
	ld c,(ix+Tape.Header.BlockNumber+0)
	ld b,(ix+Tape.Header.BlockNumber+1)
	or a
	sbc hl,bc
	jr z,CorrectBlockNumber

GotWrongBlock:
	; Zut, the wrong block!
	ld hl,BlockError
	.bcall "VDU.PutStringWithNewLines"
	jp TapeBlockLoop

CorrectBlockNumber:
	; bc = old block number, so get ready for the next block number.
	inc bc
	ld (GetFileExpectedBlock),bc
	
	dec bc
	ld a,b
	or c
	jr nz,CheckBlockFilename
	
	; This is our first time around, so remember the block name for next time.
	push de
	ld hl,GetFileHeader
	ld de,GetFileExpectedName
	ld bc,11
	ldir
	pop de
	jr GotValidBlockFilename

CheckBlockFilename:
	
	ld hl,GetFileHeader
	ld bc,GetFileExpectedName
-:	ld a,(bc)
	cp (hl)
	jr nz,GotWrongBlock
	or a
	jr z,GotValidBlockFilename
	inc hl
	inc bc
	jr -

GotValidBlockFilename:

	; Now we can advance the pointer if the block had valid data.
	
	; Was there any data in the block?
	ld c,(ix+Tape.Header.DataBlockLength+0)
	ld b,(ix+Tape.Header.DataBlockLength+1)
	ld a,b
	or c
	jr z,EmptyBlock
	
	; Increment the received size.
	ld hl,(TempSize)
	add hl,bc
	ld (TempSize),hl
	
	; Compute the CRC16.
	push de
	ld de,(TempPtr)
	call Tape.CRC16
	pop de
	
	ld a,(ix+Tape.Header.DataCRC+1)
	cp l
	jp nz,CRCError
	ld a,(ix+Tape.Header.DataCRC+0)
	cp h
	jp nz,CRCError
	
.if 0
	ld a,' '
	.bcall "VDU.PutChar"
	.bcall "VDU.PutHexWord"
	ld a,'='
	.bcall "VDU.PutChar"
	
	ld l,(ix+Tape.Header.DataCRC+1)
	ld h,(ix+Tape.Header.DataCRC+0)
	.bcall "VDU.PutHexWord"
	
	ld a,'@'
	.bcall "VDU.PutChar"
	push de
	pop hl
	.bcall "VDU.PutHexWord"
.endif
	
	
EmptyBlock:	
	; If we're not actively loading a file (GetFileName=-1), reset DE.
	ld bc,(GetFileName)
	inc bc
	ld a,b
	or c
	jr z,+
	ld de,(GetFileAddress)
+:	ld (TempPtr),de
	
	; Is this end of the file?
	ld a,(ix+Tape.Header.BlockFlag)
	add a,a
	jp nc,TapeBlockLoop
	
	; Show the file size (in hex).
	ld a,' '
	.bcall "VDU.PutChar"
	ld hl,(TempSize)
	.bcall "VDU.PutHexWord"
	
	; Always move down at the end of the file.
	.bcall "VDU.Console.NewLine"
	
	; Is it the actual file we want?
	ld hl,(GetFileName) ; If filename to get = 0, never return.
	ld a,h
	or a
	jp z,TapeSearchFileLoop
	
	inc hl  ; If filename to get = -1, we have our file!
	ld a,h
	or a
	jp nz,TapeSearchFileLoop
	
	
	pop ix
	xor a
	ret

GetFileError:
	pop ix
	xor a
	inc a
	ret

Searching:
.db "Searching\r\r",0
Loading:
.db "Loading\r\r",0

DataError:
.db "\rData?\r",0
HeaderError:
.db "\rHeader?\r",0
BlockError:
.db "\rBlock?\r",0
	

.endmodule