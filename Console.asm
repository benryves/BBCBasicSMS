; ==========================================================================
; Console
; --------------------------------------------------------------------------
; Handles outputting text to the screen.
; ==========================================================================
.module Console

CurRow = allocVar(1)
CurCol = allocVar(1)

MinRow = allocVar(1)
MaxRow = allocVar(1)
MinCol = allocVar(1)
MaxCol = allocVar(1)

OriginX = allocVar(1)
OriginY = allocVar(1)
MaxWidth = allocVar(1)
MaxHeight = allocVar(1)

Colour = allocVar(1)

ConsoleFlags = allocVar(1)
PageMode = 1
CursorHidden = 3
CursorBlinking = 4
CursorBlinkOn = 5
Overwrite = 6
CursorEditingDisabled = 7

CursorBlinkTimer = allocVar(1)

AreaUnderCursor = allocVar(16)

; ==========================================================================
; Reset
; --------------------------------------------------------------------------
; Resets all console settings to their intial values for the current mode.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; ==========================================================================
Reset:
	
	xor a
	ld (ConsoleFlags),a
	ld a,$0F
	ld (Colour),a
	; Fall-through to ResetViewport

; ==========================================================================
; ResetViewport
; --------------------------------------------------------------------------
; Resets the viewport to the appropriate settings for the current mode.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; ==========================================================================
ResetViewport:
	
	; Set to sensible defaults.
	xor a
	ld (MinRow),a
	ld (OriginY),a
	inc a
	ld (MinCol),a
	ld (OriginX),a
	ld a,23
	ld (MaxRow),a
	ld a,30
	ld (MaxCol),a
	
	ld a,30
	ld (MaxWidth),a
	ld a,24
	ld (MaxHeight),a
	
	; Now invoke the driver-specific version.
	ld a,Driver.Execute.ResetConsoleViewport
	call Driver.Execute
	; Fall-through to HomeUp

; ==========================================================================
; HomeUp
; --------------------------------------------------------------------------
; Moves the cursor to the top left corner of the viewport.
; --------------------------------------------------------------------------
; Destroyed:  A.
; ==========================================================================
HomeUp:
	ld a,(MinRow)
	ld (CurRow),a
	; Fall-through to HomeLeft

; ==========================================================================
; HomeLeft
; --------------------------------------------------------------------------
; Moves the cursor to the left edge of the viewport.
; --------------------------------------------------------------------------
; Destroyed:  A.
; ==========================================================================
HomeLeft:
	ld a,(MinCol)
	ld (CurCol),a
	ret

; ==========================================================================
; CursorRight
; --------------------------------------------------------------------------
; Moves the cursor right one character.
; --------------------------------------------------------------------------
; Outputs:    F: Carry set if this caused the screen to scroll.
; Destroyed:  AF.
; ==========================================================================
CursorRight:
	ld a,(CurCol)
	inc a
	push bc
	ld bc,(MaxCol)
	cp c
	pop bc
	jr z,+
	jr c,+
	ld a,(MinCol)
	ld (CurCol),a
	jr NewLine
	
+:	ld (CurCol),a
	or a
	ret
	

; ==========================================================================
; NewLine
; --------------------------------------------------------------------------
; Moves the cursor down one row and into the leftmost column.
; --------------------------------------------------------------------------
; Outputs:    F: Carry set if this caused the screen to scroll.
; Destroyed:  AF.
; ==========================================================================
NewLine:
	ld a,(MinCol)
	ld (CurCol),a
	; Fall-through to CursorDown

; ==========================================================================
; CursorDown
; --------------------------------------------------------------------------
; Moves the cursor down one row.
; --------------------------------------------------------------------------
; Outputs:    F: Carry set if this caused the screen to scroll.
; Destroyed:  AF.
; ==========================================================================
CursorDown:

	ld a,(CurRow)
	inc a
	push bc
	ld bc,(MaxRow)
	cp c
	pop bc
	jr z,+
	jr c,+
	
	ld a,(MaxRow)
	ld (CurRow),a
	call ScrollUp
	scf
	ret
	
+:	ld (CurRow),a
	or a
	ret

; ==========================================================================
; CursorLeft
; --------------------------------------------------------------------------
; Moves the cursor left one column.
; --------------------------------------------------------------------------
; Outputs:    F: Carry set if this caused the screen to scroll.
; Destroyed:  AF.
; ==========================================================================
CursorLeft:
	ld a,(CurCol)
	or a
	jr z,CursorLeftWrapped
	dec a
	push bc
	ld bc,(MinCol)
	cp c
	pop bc
	jr nc,+
CursorLeftWrapped:
	scf
	ld a,(MaxCol)
+:	push af
	ld (CurCol),a
	pop af
	ret nc
+:	; Fall-through to CursorUp.

; ==========================================================================
; CursorUp
; --------------------------------------------------------------------------
; Moves the cursor up one row.
; --------------------------------------------------------------------------
; Outputs:    F: Carry set if this caused the screen to scroll.
; Destroyed:  AF.
; ==========================================================================
CursorUp:
	ld a,(CurRow)
	or a
	jr z,CursorUpWrapped
	dec a
	push bc
	ld bc,(MinRow)
	cp c
	pop bc
	jr nc,+

CursorUpWrapped:
	ld a,(MinRow)
	ld (CurRow),a
	call ScrollDown
	scf
	ret
	
+:	ld (CurRow),a
	or a
	ret

; ==========================================================================
; Clear
; --------------------------------------------------------------------------
; Clears the console viewport.
; --------------------------------------------------------------------------
; Destroyed:  AF, BC.
; ==========================================================================
Clear:
	ld a,(Console.MinRow)
	ld c,a
	ld a,(Console.MaxRow)
	sub c
	inc a
	ld c,a

	ld a,(Console.MinRow)
	ld (Console.CurRow),a

--:	ld a,(Console.MinCol)
	ld b,a
	ld a,(Console.MaxCol)
	sub b
	inc a
	ld b,a
	
	ld a,(Console.MinCol)
	ld (Console.CurCol),a
	
-:	push bc
	ld a,' '
	call Driver.PutMap
	ld a,(Console.CurCol)
	inc a
	ld (Console.CurCol),a
	pop bc
	
	djnz -
	
	ld a,(Console.CurRow)
	inc a
	ld (Console.CurRow),a
	
	dec c
	jr nz,--
	
	call Console.HomeUp
	ret

DriverExecute:
	push bc
	push de
	push hl
	call Driver.Execute
	pop hl
	pop de
	pop bc
	ret

ScrollUp:
	ld a,(MaxCol)
	call CheckScrollInhibit
	ld a,Driver.Execute.ScrollUp
	jr DriverExecute
	
ScrollDown:
	ld a,(MinCol)
	call CheckScrollInhibit
	ld a,Driver.Execute.ScrollDown
	jr DriverExecute

CheckScrollInhibit:
	push bc
	ld b,a
	ld a,(Keyboard.Status)
	and %101
	cp %101
	ld a,b
	pop bc
	ret nz
	
	push bc
	ld bc,(CurRow)
	push bc
	
	ld (CurCol),a
	
-:	call VDU.DrawBlinkingCursor
	call KeyboardBuffer.GetDeviceKeyImmediate
	
	ld a,(Keyboard.Status)
	and %101
	cp %101
	jr z,-
	
	call VDU.EndBlinkingCursor
	
	pop bc
	ld (CurRow),bc
	pop bc
	ret

; ---------------------------------------------------------
; RotateBlock -> Rotates a block of characters.
; ---------------------------------------------------------
; Inputs:   hl = offset in VRAM to top left corner.
;           de = stride of VRAM in bytes.
;           b = number of rows to rotate.
;           c = number of columns to rotate.
; Outputs:  None.
; Destroys: af, hl, bc.
; ---------------------------------------------------------
RotateBlock:
	
	; Back up the top row of the nametable.
	push hl
	push bc
	call Video.SetReadAddress
	ld hl,128
	call Host.GetSafeScratchMemoryHL
	jr c,+
	ld a,c
	ld c,64
	add hl,bc
	ld (TempPtr),hl
	ld b,c
-:	in a,(Video.Data)
	ld (hl),a
	inc hl
	djnz -
	or a
+:	pop bc
	pop hl
	
	jr c,ScrollBlock
	
	push bc
	call ScrollBlockNoClear
	pop bc
	
	; Move the top row to the bottom row.
	ld hl,(TempPtr)
	
	ld b,c
-:	ld a,(hl)
	out (Video.Data),a
	inc hl
	djnz -
	ret

; ---------------------------------------------------------
; ScrollBlockNoClear -> Scrolls a block of characters but
;                       doesn't clear the bottom row.
; ---------------------------------------------------------
; Inputs:   hl = offset in VRAM to top left corner.
;           de = stride of VRAM in bytes.
;           b = number of rows to scroll.
;           c = number of columns to scroll.
; Outputs:  None.
; Destroys: af, hl, bc.
; ---------------------------------------------------------
ScrollBlockNoClear:
	
	xor a
	dec a
	jr ScrollBlockSetNZ

; ---------------------------------------------------------
; ScrollBlock -> Scrolls a block of characters and clears
;                the bottom row.
; ---------------------------------------------------------
; Inputs:   hl = offset in VRAM to top left corner.
;           de = stride of VRAM in bytes.
;           b = number of rows to scroll.
;           c = number of columns to scroll.
; Outputs:  None.
; Destroys: af, hl, bc.
; ---------------------------------------------------------
ScrollBlock:
		
	xor a

ScrollBlockSetNZ:
	push af
	
	dec b
	jr z,ClearBottomRow

ScrollBlockRow:
	add hl,de
	
	push hl
	push bc
	
	; Copy one row.
	call Video.SetReadAddress
	
	ld hl,64
	call Host.GetSafeScratchMemoryHL
	jr c,+
	
	ld b,c
-:	in a,(Video.Data) ; 11
	ld (hl),a         ; 7
	inc hl            ; 6
	djnz -            ; 12 <- 36

+:
	
	pop bc
	pop hl
	
	; Write back to the row above.
	push hl
	push bc
	
	or a
	sbc hl,de
	
	call Video.SetWriteAddress
	
	ld hl,64
	call Host.GetSafeScratchMemoryHL
	jr c,+
	
	ld b,c
-:	ld a,(hl)          ; 7
	out (Video.Data),a ; 11
	inc hl             ; 6
	djnz -             ; 12 <- 36

+:

	pop bc
	pop hl
	
	djnz ScrollBlockRow

ClearBottomRow:
	
	; Now that we've scrolled, clear that bottom row.
	call Video.SetWriteAddress
	
	pop af
	jr nz,+
	
	ld b,c
-:	ld a,0              ; 7
	out (Video.Data),a  ; 11
	nop                 ; 4
	djnz -              ; 12 <- 34

+:	ret

; ---------------------------------------------------------
; BeginBlinkingCursor -> Prepare to draw blinking cursor.
; ---------------------------------------------------------
; Destroys: af.
; ---------------------------------------------------------
BeginBlinkingCursor:
	; Is the cursor already blinking?
	ld a,(ConsoleFlags)
	bit CursorBlinking,a
	ret nz
	set CursorBlinking,a
	res CursorBlinkOn,a
	ld (ConsoleFlags),a
	
	; We haven't already started.
	push hl
	push de
	push bc
	ld hl,AreaUnderCursor
	ld a,Driver.Execute.GetCursorArea
	call Driver.Execute
	pop bc
	pop de
	pop hl
	
	ld a,(Host.TIME)
	ld (CursorBlinkTimer),a
	ret

; ---------------------------------------------------------
; EndBlinkingCursor -> Ends drawing the blinking cursor.
; ---------------------------------------------------------
; Destroys: af.
; ---------------------------------------------------------
EndBlinkingCursor:
	; Is the cursor currently blinking?
	ld a,(ConsoleFlags)
	bit CursorBlinking,a
	ret z
	res CursorBlinking,a
	ld (ConsoleFlags),a

	; We had been drawing a blinking cursor.
	push hl
	push de
	push bc
	ld hl,AreaUnderCursor
	ld a,Driver.Execute.SetCursorArea
	call Driver.Execute
	pop bc
	pop de
	pop hl
	ret

; ---------------------------------------------------------
; DrawBlinkingCursor -> Draws the blinking cursor.
; ---------------------------------------------------------
; Destroys: af.
; ---------------------------------------------------------
DrawBlinkingCursor:
	; Is the cursor hidden?
	ld a,(ConsoleFlags)
	bit CursorHidden,a
	ret nz

	; Is the cursor set up to blink?
	bit CursorBlinking,a
	call z,BeginBlinkingCursor
	
	; Are we showing the cursor or the character underneath?
	push bc
	ld a,(CursorBlinkTimer)
	ld b,a
	ld a,(Host.TIME)
	sub b
	and %00100000
	pop bc
	
	jr z,DrawBlinkingCursorOn

DrawBlinkingCursorOff:
	; Is the cursor already blinking off?
	ld a,(ConsoleFlags)
	bit CursorBlinkOn,a
	ret z
	res CursorBlinkOn,a
	ld (ConsoleFlags),a
	push hl
	push de
	push bc
	ld hl,AreaUnderCursor
	ld a,Driver.Execute.SetCursorArea
	call Driver.Execute
	pop bc
	pop de
	pop hl
	ret

DrawBlinkingCursorOn:
	; Is the cursor already blinking on?
	ld a,(ConsoleFlags)
	bit CursorBlinkOn,a
	ret nz
	set CursorBlinkOn,a
	ld (ConsoleFlags),a
	; What is the cursor?
	bit Overwrite,a
	ld a,'_' ; Insert mode cursor
	jr z,+
	ld a,127 ; Overwrite mode cursor 
+:	jp Driver.PutMap

.endmodule