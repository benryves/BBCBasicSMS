; ==========================================================================
; File
; --------------------------------------------------------------------------
; Provides a library of routines for handling different file systems.
; ==========================================================================
.module File

FileSystem        = allocVar(1)
Options           = allocVar(1)
PersistentHandles = allocVar(1)

MaxHandles = 27
HandleDataSize = 256 + 64

.module FileSystems
	None = 0
	Tape1200 = 1
	Tape300 = 2
	PCLink2 = 88
	VDrive = 89
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
	ld (PersistentHandles),a
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
	; Quick check for invalid handle A=0.
	or a
	jr nz,+
	dec a
	ret
+:	
	
	; Is it a persistent handle?
	ld b,a
	ld a,(PersistentHandles)
	cp b
	jr c,GetHandleVariable.NotPersistent

	; It's a persistent handle!
	ld hl,HIMEM
	ld de,-(HandleDataSize+4)
-:	add hl,de
	djnz -
	
	push hl
	pop ix
	
	xor a
	ret

GetHandleVariable.NotPersistent:
	
	ld hl,HandleVariableName
	ld de,VDU.TempTile
	
	push bc ; B = handle
	
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
	
	; If this is a persistent variable, then we've already
	; allocated memory for it so can skip the next part.
	ld a,(PersistentHandles)
	cp c
	jr nc,FinishedAllocatingHandle
	
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
	
FinishedAllocatingHandle:

	ld a,c
	cp a ; Z, NC.
	
CouldNotCreateHandle:

	pop bc
	pop de
	pop hl
	pop iy
	ret

; ==========================================================================
; SetPersistentHandleLimit
; --------------------------------------------------------------------------
; Sets the number of persistent handles.
; --------------------------------------------------------------------------
; Inputs:     A: Number of persistent handles to permit.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
SetPersistentHandleLimit:
	; Start by closing all existing handles.
	push af
	
	ld e,0
	call Close
	
	; Do we already have any persistent handles?
	ld a,(PersistentHandles)
	or a
	jr z,NoPersistentHandlesToFree
	
	; We will need to move data from SP..(HIMEM)-1 back up to end at HIMEM-1
	ld hl,(Basic.BBCBASIC_HIMEM)
	or a
	sbc hl,sp
	ld c,l
	ld b,h
	
	; BC = amount of data on stack.
	
	ld hl,(Basic.BBCBASIC_HIMEM)
	dec hl
	ld de,HIMEM-1
	
	lddr
	
	; Now move SP to where it should be.
	ld hl,HIMEM
	ld de,(Basic.BBCBASIC_HIMEM)
	or a
	sbc hl,de
	add hl,sp
	ld sp,hl

NoPersistentHandlesToFree:
	
	; Set BASIC HIMEM to match actual HIMEM.
	ld hl,HIMEM
	ld (Basic.BBCBASIC_HIMEM),hl
	
	pop af
	
	; If the number of persistent handles = 0, we're done.
	or a
	jr nz,+
	ld (PersistentHandles),a
	ret
+:
		
	; How much space will the persistent handles take?
	ld b,a
	ld c,a
	ld hl,0
	ld de,HandleDataSize+4
-:	add hl,de
	jr c,SetPersistentHandlesNoRoom
	djnz -
	ex de,hl
	ld (TempSize),de
	
	; Is there enough room for all those handles?
	ld hl,(Basic.BBCBASIC_FREE)
	add hl,de
	ret c
	sbc hl,sp
	jr c,SetPersistentHandlesEnoughRoom
	; No room.
	
SetPersistentHandlesNoRoom:
	scf
	ret

SetPersistentHandlesEnoughRoom:

	; Will the new HIMEM be below $C100?
	ld hl,HIMEM-$C100
	ld de,(TempSize)
	sbc hl,de
	ret c

	; We have enough room!
	ld a,c
	ld (PersistentHandles),a
	
	; Now move HIMEM down.
	; We'll need to move between SP..HIMEM-1
	
	ld hl,0
	add hl,sp
	; HL = source
	
	ld (TempPtr),hl
	
	ld de,HIMEM
	ex de,hl
	or a
	
	sbc hl,de
	
	ld c,l
	ld b,h
	
	; BC = number of bytes between SP and HIMEM.
	
	ld hl,(TempPtr)  ; SP
	ld de,(TempSize) ; size of persistent file storage
	or a
	sbc hl,de
	ld sp,hl
	ex de,hl
	
	; DE = new SP.
	
	ld hl,(TempPtr)
	ldir
	
	; Finally, update HIMEM variable.
	ld hl,HIMEM
	ld de,(TempSize)
	or a
	sbc hl,de
	ld (Basic.BBCBASIC_HIMEM),hl
	
	; Now the space is allocated, fill it with dummy closed handles.
	ld a,(PersistentHandles)
	ld b,a
	
	ld hl,HIMEM
	ld de,-(HandleDataSize+4)
	
	xor a
	
-:	add hl,de
	push de
	push hl
	
	; We need to set the data area for the handle to be HL+4
	ex de,hl
	ld hl,4
	add hl,de
	ex de,hl
	
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	
	pop hl
	pop de
	djnz -
	
	xor a
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
	cp FileSystems.VDrive
	jp z,VDrive.FileOpen
	
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
; Close a file previously opened with Open.
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
	
	
	ld bc,Closed
	push bc
	
	ld a,(ix+2)
	cp FileSystems.Tape1200
	jp z,Tape.FileClose
	
	cp FileSystems.Tape300
	jp z,Tape.FileClose
	
	cp FileSystems.VDrive
	jp z,VDrive.FileClose

Closed:
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
	
	; Is it open?
	ld a,(ix+3)
	and 3
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
	
	cp FileSystems.VDrive
	jp z,VDrive.FileGetByte
	
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
; WriteByte
; --------------------------------------------------------------------------
; Write a single byte to an open file.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number).
;             A: The value to write to the file.
; Destroyed:  AF, BC.
; ==========================================================================
WriteByte:
	push ix
	
	ld b,a
	
	; Get the handle.
	ld a,e
	call GetHandle
	
	; Could we retrieve it?
	jp nz,Channel
	
	; Is it open for writing?
	bit 0,(ix+3)
	jp z,ReadOnly
	
	push hl
	push de
	
	ld a,b
	
	ld bc,DoneWriteByte
	push bc
	
	ld b,a
	
	ld a,(ix+2)
	cp FileSystems.Tape1200
	ld a,b
	jp z,Tape.FileWriteByte
	
	ld a,(ix+2)
	cp FileSystems.Tape300
	ld a,b
	jp z,Tape.FileWriteByte

	ld a,(ix+2)
	cp FileSystems.VDrive
	ld a,b
	jp z,VDrive.FileWriteByte
	
	; Unsupported device.
	pop de
	pop hl
	pop ix
	jp Host.DeviceFault
	
DoneWriteByte:

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
	
	; Is it open?
	ld a,(ix+3)
	and 3
	jp z,Channel
	
	push bc
	
	ld bc,DoneIsEOF
	push bc
	
	ld a,(ix+2)
	
	cp FileSystems.Tape1200
	jp z,Tape.FileIsEOF
	
	cp FileSystems.Tape300
	jp z,Tape.FileIsEOF
	
	cp FileSystems.VDrive
	jp z,VDrive.FileIsEOF
	
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
	
	cp FileSystems.VDrive
	jp z,VDrive.FileGetPointer
	
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
	
	cp FileSystems.VDrive
	jp z,VDrive.FileSetPointer
	
	; Unsupported device.
	pop bc
	pop ix
	jp Host.DeviceFault
	
DoneSetPointer:
	
	pop bc
	pop ix
	ret

; ==========================================================================
; GetLength
; --------------------------------------------------------------------------
; Return the length of an open file.
; --------------------------------------------------------------------------
; Inputs:     E: The file handle (channel number).
; Outputs:    DEHL: File size (bytes).
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
GetLength:
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
	
	ld hl,DoneGetLength
	push hl
	
	ld a,(ix+2)
	
	cp FileSystems.VDrive
	jp z,VDrive.FileGetLength
	
	; Unsupported device.
	pop hl
	pop ix
	jp Host.DeviceFault
	
DoneGetLength:
	
	pop ix
	ret

; ==========================================================================
; AccessDenied
; --------------------------------------------------------------------------
; Triggers the "Access denied" error.
; ==========================================================================
AccessDenied:
	ld a,189
	call Basic.BBCBASIC_EXTERR
	.db "Access denied",0

; ==========================================================================
; TooManyOpenFiles
; --------------------------------------------------------------------------
; Triggers the "Too many open files" error.
; ==========================================================================
TooManyOpenFiles:
 	ld a,192
	call Basic.BBCBASIC_EXTERR
	.db "Too many open files",0

; ==========================================================================
; ReadOnly
; --------------------------------------------------------------------------
; Triggers the "Read only" error.
; ==========================================================================
ReadOnly:
 	ld a,193
	call Basic.BBCBASIC_EXTERR
	.db "Read only",0

; ==========================================================================
; DiskFull
; --------------------------------------------------------------------------
; Triggers the "Disk full" error.
; ==========================================================================
DiskFull:
	ld a,198
	call Basic.BBCBASIC_EXTERR
	.db "Disk full",0

; ==========================================================================
; DiskFault
; --------------------------------------------------------------------------
; Triggers the "Disk fault" error.
; ==========================================================================
DiskFault:
	ld a,199
	call Basic.BBCBASIC_EXTERR
	.db "Disk fault",0

; ==========================================================================
; BadName
; --------------------------------------------------------------------------
; Triggers the "Bad name" error.
; ==========================================================================
BadName:
 	ld a,204
	call Basic.BBCBASIC_EXTERR
	.db Tokens.Bad, "name",0

; ==========================================================================
; BadDirectory
; --------------------------------------------------------------------------
; Triggers the "Bad directory" error.
; ==========================================================================
BadDirectory:
 	ld a,206
	call Basic.BBCBASIC_EXTERR
	.db Tokens.Bad, "directory",0

; ==========================================================================
; FileNotFound
; --------------------------------------------------------------------------
; Triggers the "File not found" error.
; ==========================================================================
FileNotFound:
 	ld a,214
	call Basic.BBCBASIC_EXTERR
	.db "File not found",0

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

.endmodule