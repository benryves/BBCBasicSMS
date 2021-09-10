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
	ld a,(ix+2)
	srl a
	or (ix+2)
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
	cp 1 ; OPENUP
	jr nz,+
	
	xor a
	ret

+:	add a,2 ; 1 = OPENOUT, 2 = OPENIN, 3 = OPENUP
	push af
	call CreateHandle
	pop bc
	ld (ix+2),b
	push af
	
	pop af
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
	ld (ix+2),0
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
; Channel
; --------------------------------------------------------------------------
; Triggers the "Channel" error.
; ==========================================================================
Channel:
	ld a,222
	call Basic.BBCBASIC_EXTERR
	.db "Channel",0

.endmodule