; ==========================================================================
; File
; --------------------------------------------------------------------------
; Provides a library of routines for handling different file systems.
; ==========================================================================
.module File

FileSystem = allocVar(1)
Options    = allocVar(1)
MaxHandles = 27
HandleDataSize = 256 + 128

.module FileSystems
	None = 0
	Tape1200 = 1
	Tape300 = 2
	PCLink2 = 88
.endmodule

; ==========================================================================
; Reset
; --------------------------------------------------------------------------
; Resets the file system handler to its defaults.
; --------------------------------------------------------------------------
; Destroyed:  Everything.
; ==========================================================================
Reset:
	xor a
	ld (FileSystem),a
	ret

; ==========================================================================
; SetFileSystem
; --------------------------------------------------------------------------
; Switches to the selected file system.
; --------------------------------------------------------------------------
; Inputs:     A: File system number.
; Destroyed:  Everything.
; ==========================================================================
SetFileSystem:
	ld (FileSystem),a
	ret

; ==========================================================================
; GetHandleVariable
; --------------------------------------------------------------------------
; Finds the variable used to represent information about a file handle.
; --------------------------------------------------------------------------
; Inputs:     A = File handle number.
; Outputs:    F = Z if the file handle was found, NZ if not.
;             IX = pointer to the file handle variable if found.
; Destroyed:  AF, BC, DE, HL, IX, IY.
; ==========================================================================
GetHandleVariable:
	ld hl,HandleVariableName
	ld de,VDU.TempTile
	
	push af
	
-:	ld a,(hl)
	or a
	jr z,+
	ldi
	jr -
+:	
	pop af
	
	add a,'@'-1 ; Handles start at 1
	ld (de),a
	inc de
	ld a,'%'
	ld (de),a
	inc de
	ld a,'='
	ld (de),a
	
	ld iy,VDU.TempTile
	ld a,(iy)
	call Basic.BBCBASIC_GETVAR
	ret

HandleVariableName:
.db "__fh_",0

; ==========================================================================
; GetHandle
; --------------------------------------------------------------------------
; Gets an existing handle for file.
; --------------------------------------------------------------------------
; Inputs:     A = the file handle.
; Outputs:    F = Z if the file handle was found, NZ if not.
;             IX = pointer to the file handle variable if it was found.
; Destroyed:  AF, IX.
; ==========================================================================
GetHandle:
	push iy
	push hl
	push de
	push bc
	
	call GetHandleVariable
	jr nz,+
	call IsHandleVariableOpen
+:	
	pop bc
	pop de
	pop hl
	pop iy
	ret

; ==========================================================================
; IsHandleVariableOpen
; --------------------------------------------------------------------------
; Checks if an existing handle variable is open.
; --------------------------------------------------------------------------
; Inputs:     IX = Pointer to handle variable.
; Outputs:    F = Z if the file is open, NZ if the file is closed.
; Destroyed:  AF.
; ==========================================================================
IsHandleVariableOpen:
	ld a,(ix+3)
	srl a
	or (ix+3)
	and 1
	xor 1
	ret

; ==========================================================================
; CreateHandle
; --------------------------------------------------------------------------
; Creates a new handle for file operations.
; --------------------------------------------------------------------------
; Outputs:    F = Z if the file handle was created, NZ if not.
;                 C if there is not enough space.
;             IX = pointer to the file handle variable if created.
; Destroyed:  AF, IX.
; ==========================================================================
CreateHandle:
	push iy
	push hl
	push de
	push bc
	
	; First up, check that there's enough room.
	ld hl,(Basic.BBCBASIC_FREE)
	ld de,HandleDataSize
	add hl,de
	sbc hl,sp
	jr c,CreateHandleEnoughRoom

	; No room.
	xor a
	ld b,a
	inc b
	scf
	
	pop bc
	pop de
	pop hl
	pop iy
	ret

CreateHandleEnoughRoom:

	; Try to find a free handle.
	ld bc,MaxHandles*256+1
	
-:	push bc
	ld a,c
	call GetHandleVariable
	pop bc
	jr nz,FoundFreeHandle
	call IsHandleVariableOpen
	jr nz,ReuseClosedHandle
	inc c
	djnz -
	
	pop bc
	pop de
	pop hl
	pop iy
	
	; No free handles!
	xor a  ; clear carry
	inc b ; set non-zero
	ret

FoundFreeHandle:

	; At this point we should be able to create a new variable.
	push bc
	call Basic.BBCBASIC_PUTVAR
	pop bc
	ld a,0
	jr nz,CouldNotCreateHandle

ReuseClosedHandle:

	; We have now created the variable.
	; Store a pointer to its data.
	ld hl,(Basic.BBCBASIC_FREE)
	ld (ix+0),l
	ld (ix+1),h
	ld (ix+2),0
	ld (ix+3),0
	
	ld de,HandleDataSize
	add hl,de
	ld (Basic.BBCBASIC_FREE),hl
	
	ld a,c
	cp a ; Z, NC.
	
CouldNotCreateHandle:

	pop bc
	pop de
	pop hl
	pop iy
	ret
	
; ==========================================================================
; Open
; --------------------------------------------------------------------------
; Open a file for reading or writing.
; --------------------------------------------------------------------------
; Inputs:     HL: addresses a file descriptor (filename) terminated by CR.
;             AF: -1, NZ, NC = OPENOUT
;                  0,  Z,  C = OPENIN
;                  1, NZ,  C = OPENUP
; Outputs:    A: the file "handle" (channel number) allocated. This may be 
;                any value from 1 to 255. If a value 0 is returned, the file
;                could not be opened. Normal practice is to return with A=0
;                if the file  specified in OPENIN or OPENUP was not found
;                but to abort with an appropriate message (e.g.
;                "Directory full") in the event of other errors.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
Open:
	add a,2 ; 1 = OPENOUT, 2 = OPENIN, 3 = OPENUP
	push ix
	push af
	call CreateHandle
	pop bc
	ld (ix+3),b
	ld b,a
	ld a,(FileSystem)
	ld (ix+2),a
	ld a,b
	push af
	
	ld bc,DoneOpen
	push bc
	
	ld a,(FileSystem)
	cp FileSystems.Tape1200
	jp z,Tape.FileOpen
	cp FileSystems.Tape300
	jp z,Tape.FileOpen
	
	pop bc
	ld (ix+3),0 ; Close
	jp Host.DeviceFault

DoneOpen:
	
	; Is the file still open?
	ld a,(ix+3)
	or a
	jr nz,+
	pop af
	xor a
	push af
+:	
	pop af
	pop ix
	ret

; ==========================================================================
; Close
; --------------------------------------------------------------------------
; Close a file previously opened with OSOPEN.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number) to close.
;                If E is 0 then all open files (if any) are closed.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
Close:
	ld a,e
	or a
	jr z,CloseAll
	
	push ix
	call GetHandle
	jp nz,Channel
	ld (ix+3),0
	pop ix
	
	ret

CloseAll:
	push ix
	ld bc,MaxHandles*256+1
-:	push bc
	ld a,c
	call GetHandle
	jr nz,+
	ld e,c
	call Close
+:	pop bc
	inc c
	djnz -
	pop ix
	ret

; ==========================================================================
; GetByte
; --------------------------------------------------------------------------
; Read a single byte from an open file.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number).
; Outputs:    A: The byte read from the file.
; Destroyed:  AF, BC.
; ==========================================================================
GetByte:
	push ix
	
	; Get the handle.
	ld a,e
	call GetHandle
	
	; Could we retrieve it?
	jp nz,Channel
	
	; Is it open for reading?
	bit 1,(ix+3)
	jp z,Channel
	
	push hl
	push de
	push bc
	
	ld bc,DoneGetByte
	push bc
	
	ld a,(ix+2)
	
	cp FileSystems.Tape1200
	jp z,Tape.FileGetByte
	
	cp FileSystems.Tape300
	jp z,Tape.FileGetByte
	
	; Unsupported device.
	pop bc
	pop de
	pop hl
	pop ix
	jp Host.DeviceFault
	
DoneGetByte:

	pop bc
	pop de
	pop hl
	pop ix
	ret

; ==========================================================================
; IsEOF
; --------------------------------------------------------------------------
; Determines whether an open file pointer is at the end-of-file.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number).
; Outputs:    F: If at the end-of-file, return Z.
;                If not at end-of file, return NZ.
; Destroyed:  AF, DE, HL.
; ==========================================================================
IsEOF:
	push ix
	
	; Get the handle.
	ld a,e
	call GetHandle
	
	; Could we retrieve it?
	jp nz,Channel
	
	; Is it open for reading?
	bit 1,(ix+3)
	jp z,Channel
	
	push bc
	
	ld bc,DoneIsEOF
	push bc
	
	ld a,(ix+2)
	
	cp FileSystems.Tape1200
	jp z,Tape.FileIsEOF
	
	cp FileSystems.Tape300
	jp z,Tape.FileIsEOF
	
	; Unsupported device.
	pop bc
	pop ix
	jp Host.DeviceFault
	
DoneIsEOF:

	pop bc
	pop ix
	ret

; ==========================================================================
; GetPointer
; --------------------------------------------------------------------------
; Read the sequential pointer of an open file.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number).
; Outputs:    DEHL: the 32-bit pointer.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
GetPointer:
	push ix
	
	; Get the handle.
	ld a,e
	call GetHandle
	
	; Could we retrieve it?
	jp nz,Channel
	
	; Is it open for reading or writing?
	ld a,(ix+3)
	and 3
	jp z,Channel
	
	push bc
	
	ld bc,DoneGetPointer
	push bc
	
	ld a,(ix+2)
	
	cp FileSystems.Tape1200
	jp z,Tape.FileGetPointer
	
	cp FileSystems.Tape300
	jp z,Tape.FileGetPointer
	
	; Unsupported device.
	pop bc
	pop ix
	jp Host.DeviceFault
	
DoneGetPointer:
	
	pop bc
	pop ix
	ret

; ==========================================================================
; SetPointer
; --------------------------------------------------------------------------
; Update the sequential pointer of an open file.
; --------------------------------------------------------------------------
; Inputs:     A: The file handle (channel number).
;             DEHL: The new file pointer.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
SetPointer:
	push ix
	
	; Get the handle.
	call GetHandle
	
	; Could we retrieve it?
	jp nz,Channel
	
	; Is it open for reading or writing?
	ld a,(ix+3)
	and 3
	jp z,Channel
	
	push bc
	
	ld bc,DoneSetPointer
	push bc
	
	ld a,(ix+2)
	
	cp FileSystems.Tape1200
	jp z,Tape.FileSetPointer
	
	cp FileSystems.Tape300
	jp z,Tape.FileSetPointer
	
	; Unsupported device.
	pop bc
	pop ix
	jp Host.DeviceFault
	
DoneSetPointer:
	
	pop bc
	pop ix
	ret
	
; ==========================================================================
; Channel
; --------------------------------------------------------------------------
; Triggers the "Channel" error.
; ==========================================================================
Channel:
	ld a,222
	call Basic.BBCBASIC_EXTERR
	.db "Channel",0

; ==========================================================================
; EOF
; --------------------------------------------------------------------------
; Triggers the "EOF" error.
; ==========================================================================
EOF:
	ld a,223
	call BASIC.BBCBASIC_EXTERR
	.db "EOF",0

.endmodule