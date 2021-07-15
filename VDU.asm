.module VDU

; Dependencies
.include "Console.asm"
.include "Graphics.asm"

; Mode files
.module Modes

	.include "Text.asm"
	.include "GraphicsII.asm"
	.include "Mode4.asm"

	Count = 3

.endmodule

; Font data
.module Fonts

	Font8x8:
	.incbin "Font8x8.bin"

	Font6x8:
	.incbin "Font6x8.bin"

.endmodule

; Mode-specific vectors.
PutMap = allocVar(3)
Scroll = allocVar(3)

SetForegroundPixel = allocVar(3)
SetBackgroundPixel = allocVar(3)
InvertPixel = allocVar(3)

CommandQueue.Capacity = 10
CommandQueue = allocVar(CommandQueue.Capacity)
CommandQueue.Waiting = allocVar(1)

Reset:
	xor a
SetMode:
	di
	push af
	
	ld a,$C3 ; JP
	ld (PutMap),a
	ld (Scroll),a
	ld (SetForegroundPixel),a
	ld (SetBackgroundPixel),a
	ld (InvertPixel),a
	
	ld hl,Stub
	ld (PutMap+1),hl
	ld (Scroll+1),hl
	ld (SetForegroundPixel+1),hl
	ld (SetBackgroundPixel+1),hl
	ld (InvertPixel+1),hl

	
	call Console.Reset
	call Graphics.Reset
	
	
	; Reset all video settings to their defaults.
	call Video.Reset
	
	pop af
	
	; Mode-specific initialisation.
	call SetModeInitialize
	
	; Move to the top-left of the screen.
	call Console.HomeUp
	
	; Clear the point queue
	call Graphics.Reset
	
	; Clear the command queue.
	xor a
	ld (CommandQueue),a
	ld (CommandQueue.Waiting),a
	
	
	; Screen on, enable frame interrupts.
	call Video.DisplayOn
	call Video.EnableFrameInterrupt
	ei
	
Stub:
	ret

SetModeInitialize:
	or a  \ jp z,Modes.Text.Initialise
	dec a \ jp z,Modes.GraphicsII.Initialise
	dec a \ jp z,Modes.Mode4.Initialise
	ret

FontTileIndex = 0
FontCharOffset = FontTileIndex-' '

; ---------------------------------------------------------
; WriteWord -> Writes a word to the VDU.
; ---------------------------------------------------------
; Inputs:   hl = word to write, writing L before H.
; Outputs:  None.
; Destroys: None.
; ---------------------------------------------------------
WriteWord:
	push hl
	push de
	push bc
	push af
	
	push hl
	
	ld a,l
	call WriteByteDestructive
	
	pop hl
	
	ld a,h
	call WriteByteDestructive
	
	pop af
	pop bc
	pop de
	pop hl
	ret

; ---------------------------------------------------------
; WriteByte -> Writes a byte to the VDU.
; ---------------------------------------------------------
; Inputs:   a = value to output.
; Outputs:  None.
; Destroys: None.
; ---------------------------------------------------------
PutChar:
WriteByte:
	push hl
	push de
	push bc
	push af
	call WriteByteDestructive
	pop af
	pop bc
	pop de
	pop hl
	ret

; ---------------------------------------------------------
; WriteByteDestructive -> Writes a byte to the VDU.
; ---------------------------------------------------------
; Inputs:   a = value to output.
; Outputs:  None.
; Destroys: af, bc, de, hl.
; ---------------------------------------------------------
WriteByteDestructive:
	; Check to see if we're waiting for anything in the command queue.
	ld c,a
	ld a,(CommandQueue.Waiting)
	or a
	jr z,VDU.WriteByteNotWaiting
	
	; Calculate the offset to the command queue.
	ld hl,CommandQueue + CommandQueue.Capacity
	ld e,a
	ld d,-1
	add hl,de
	
	; Store the received value in the command queue.
	ld (hl),c
	inc a
	ld (CommandQueue.Waiting),a
	
	ret nz ; Still waiting for more data...
	
HandleCommand:
	
	; At this point we have a multi-byte VDU command stored in the queue.
	ld a,(CommandQueue)
	
	; Quick sanity check... VDU commands must be below 32.
	cp 32
	ret nc
	
	; Find the address of the command handler in the jump table.
	
	call GetCommandJumpTable
	jp (hl)

WriteByteNotWaiting:
	
	; We're not waiting to fill up the command queue before doing something exciting.
	ld a,c
	ld (CommandQueue),a
	
	; Is it a special command?
	cp 32
	jr nc,NotCommand
	
	; It is!
	
	; Find the address of the command handler in the jump table.
	call GetCommandJumpTable
	
	; How long are we waintg?
	ld (CommandQueue.Waiting),a
	ret nz
	
	; It's a zero-byte command, so execute immediately.
	jp (hl)

NotCommand:

	cp 127 ; DELETE
	jp z,Delete

PutLiteralChar:
	call PutMap
	jp Console.CursorRight

GetCommandJumpTable:
	ld l,a
	add a,a
	add a,l
	ld l,a
	ld h,0
	ld de,CommandJumpTable
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld a,(hl)
	ex de,hl
	or a
	ret
	
CommandJumpTable:
	.dw Stub                  \ .db  0 ;  0 NUL
	.dw Stub                  \ .db  0 ;  1 Data -> Printer
	.dw Stub                  \ .db  0 ;  2 Enable printer.
	.dw Stub                  \ .db  0 ;  3 Disable printer.
	.dw Stub                  \ .db  0 ;  4 Write text at text cursor position.
	.dw Stub                  \ .db  0 ;  5 Write text at graphics cursor position.
	.dw Stub                  \ .db  0 ;  6 Enable output to the screen.
	.dw Stub                  \ .db  0 ;  7 BEL
	.dw Console.CursorLeft    \ .db  0 ;  8 Move text cursor backwards one character.
	.dw Console.CursorRight   \ .db  0 ;  9 Move text cursor forwards one character.
	.dw Console.CursorDown    \ .db  0 ; 10 Move text cursor down a line.
	.dw Console.CursorUp      \ .db  0 ; 11 Move text cursor up a line.
	.dw Console.Clear         \ .db  0 ; 12 Clear the text area (CLS).
	.dw Console.HomeLeft      \ .db  0 ; 13 Move text cursor to start of current line.
	.dw Stub                  \ .db  0 ; 14 Enable the auto-paging mode.
	.dw Stub                  \ .db  0 ; 15 Disable the auto-paging mode.
	.dw Stub                  \ .db  0 ; 16 Clear the graphics area (CLG).
	.dw TextColourCommand     \ .db -1 ; 17 Define a text colour (COLOUR).
	.dw GraphicsColourCommand \ .db -2 ; 18 Define a graphics colour (CGOL).
	.dw Stub                  \ .db -5 ; 19 Select a colour palette.
	.dw ResetColourCommand    \ .db  0 ; 20 Restore default logical colours.
	.dw Stub                  \ .db  0 ; 21 Disable output to the screen.
	.dw ModeCommand           \ .db -1 ; 22 Set the screen mode (MODE).
	.dw Stub                  \ .db -9 ; 23 User-defined characters and screen modes.
	.dw Stub                  \ .db -8 ; 24 Define a graphics viewport.
	.dw PlotCommand           \ .db -5 ; 25 PLOT
	.dw Stub                  \ .db  0 ; 26 Restore default viewports.
	.dw PutLiteralChar        \ .db -1 ; 27 Send the next character to the screen.
	.dw Stub                  \ .db -4 ; 28 Define a text viewport.
	.dw SetOriginCommand      \ .db -4 ; 29 Set the graphics origin.
	.dw Console.HomeUp        \ .db  0 ; 30 Home the cursor to the top-left of the screen.
	.dw TabCommand            \ .db -2 ; 31 Move the text cursor (TAB(x,y)).

.function VDUQ(offset, commandLength)
	VDUQ = VDU.CommandQueue + offset + VDU.CommandQueue.Capacity - commandLength
.endfunction

; ========================================================================================
; VDU 4                                                           DRAW TEXT AT TEXT CURSOR
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 5                                                       DRAW TEXT AT GRAPHICS CURSOR
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 13                                                                   CARRIAGE RETURN
; ========================================================================================

; ========================================================================================
; VDU 14                                                           ENABLE AUTO-PAGING MODE
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 15                                                          DISABLE AUTO-PAGING MODE
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 16                                                           CLEAR GRAPHICS VIEWPORT
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 17,<colour>                                                          SET TEXT COLOUR
; ========================================================================================
TextColourCommand:
	ld a,(VDUQ(0, 1))
	jp Console.SetColour
	
; ========================================================================================
; VDU 18,<mode>,<colour>                                               SET GRAPHICS COLOUR
; ========================================================================================
GraphicsColourCommand:
	ld a,(VDUQ(1, 2))
	jp Graphics.SetColour

; ========================================================================================
; VDU 20                                                                     RESET COLOURS
; ========================================================================================
ResetColourCommand:
	call Console.ResetColour
	jp Graphics.ResetColour

; ========================================================================================
; VDU 22                                                                          SET MODE
; ========================================================================================
ModeCommand:
	ld a,(VDUQ(0, 1))
	cp Modes.Count
	ret nc
	jp SetMode

; ========================================================================================
; VDU 23,<command>,<data0>,...,<data7>                                    USER DEFINITIONS
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 24,<left>;<top>;<right>;<bottom>;                              SET GRAPHICS VIEWPORT
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 25,<command>,<x>;<y>;                                                           PLOT
; ========================================================================================
PlotCommand:
	ld a,(VDUQ(0, 5))
	ld (Graphics.PlotShape),a
	
	ld de,0
	ld b,d \ ld c,e
	bit 2,a
	jr nz,NotPlotBy
	
	; Is it a "PLOT BY"	command?
	; If so, coordinates are relative to the previous one, so adjust.
	
	ld hl,(Graphics.VisitedPoint0X)
	ld de,(Graphics.OriginX)
	or a
	sbc hl,de
	ld d,h
	ld e,l
	
	ld hl,(Graphics.VisitedPoint0Y)
	ld bc,(Graphics.OriginY)
	or a
	sbc hl,bc
	ld b,h
	ld c,l
	
NotPlotBy:
	
	ld hl,(VDUQ(3,5))
	add hl,bc
	push hl
	ld hl,(VDUQ(1,5))
	add hl,de
	pop de
	
	call Graphics.VisitPoint
	call Graphics.Plot
	
	ret

; ========================================================================================
; VDU 26                                                                   RESET VIEWPORTS
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 27,<char>                                                     PRINT ESCAPE CHARACTER
; ========================================================================================
DirectOut
	ld a,(VDUQ(0,1))
	jp PutLiteralChar

; ========================================================================================
; VDU 28,<left>,<top>,<right>,<bottom>                                   SET TEXT VIEWPORT
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 29,<x>;<y>;                                                      SET GRAPHICS ORIGIN
; ========================================================================================
SetOriginCommand:
	ld hl,VDUQ(0,4)
	ld de,Graphics.OriginX
	ld bc,4
	ldir
	ret

; ========================================================================================
; VDU 30                                                           MOVE CURSOR TO TOP-LEFT
; ========================================================================================

; ========================================================================================
; VDU 31,<X>,<Y>                                                               MOVE CURSOR
; ========================================================================================
TabCommand:
	call Console.FlushPendingScroll

	ld de,(VDUQ(0,2))
	ld hl,(VDUQ(1,2))

	ld a,(Console.MinCol)
	ld d,a
	ld a,(Console.MaxCol)
	sub d
	cp e
	ret z
	ret c
	
	ld a,(Console.MinRow)
	ld h,a
	ld a,(Console.MaxRow)
	sub h
	cp l
	ret z
	ret c
	
	ld a,(Console.MinCol)
	add a,e
	ld (Console.CurCol),a
	
	ld a,(Console.MinRow)
	add a,l
	ld (Console.CurRow),a
	
	ret

; ========================================================================================
; VDU 127                                                                           DELETE
; ========================================================================================
Delete:
	call Console.FlushPendingScroll
	call Console.CursorLeft
	ld a,' '
	jp PutMap
	

PutString:
	ld a,(hl)
	inc hl
	or a
	ret z
	push hl
	call PutChar
	pop hl
	jr PutString


.endmodule