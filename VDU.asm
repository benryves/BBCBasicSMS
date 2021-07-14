.module VDU

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
SetTextColour = allocVar(3)
SetGraphicsColour = allocVar(3)
SetPixel = allocVar(3)

GraphicsCursorQueueLength = 2
GraphicsCursorQueue = allocVar(GraphicsCursorQueueLength * 4)

GraphicsBounds.MinX = allocVar(1)
GraphicsBounds.MaxX = allocVar(1)
GraphicsBounds.MinY = allocVar(1)
GraphicsBounds.MaxY = allocVar(1)

g_wndXMin = GraphicsBounds.MinX ; The g_wnd* variables must appear
g_wndXMax = GraphicsBounds.MaxX ; in this order.
g_wndYMin = GraphicsBounds.MinY
g_wndYMax = GraphicsBounds.MaxY

CommandQueue.Capacity = 10
CommandQueue = allocVar(CommandQueue.Capacity)
CommandQueue.Waiting = allocVar(1)

TextColour = allocVar(1)
GraphicsColour = allocVar(1)

.include "Console.asm"
.include "Clip.asm"

Reset:
	xor a
SetMode:
	di
	push af
	
	ld a,$C3 ; JP
	ld (PutMap),a
	ld (Scroll),a
	ld (SetTextColour),a
	ld (SetGraphicsColour),a
	ld (SetPixel),a
	
	ld hl,Stub
	ld (PutMap+1),hl
	ld (Scroll+1),hl
	ld (SetPixel+1),hl
	
	ld hl,SetTextColourDefault
	ld (SetTextColour+1),hl
	
	ld hl,SetGraphicsColourDefault
	ld (SetGraphicsColour+1),hl
	
	; Sensible text colour
	ld a,%11110100
	ld (TextColour),a
	
	; Sensible graphics colour
	ld a,%11110000
	ld (GraphicsColour),a
	
	; Reset all video settings to their defaults.
	call Video.Reset
	
	pop af
	
	; Mode-specific initialisation.
	call SetModeInitialize
	
	; Move to the top-left of the screen.
	call Console.HomeUp
	
	; Clear the point queue
	xor a
	ld (GraphicsCursorQueue),a
	ld hl,GraphicsCursorQueue
	ld de,GraphicsCursorQueue+1
	ld bc,GraphicsCursorQueueLength*4-1
	ldir
	
	; Clear the command queue.
	ld (CommandQueue),a
	ld (CommandQueue.Waiting),a
	
	; Set default graphics bounds
	xor a
	ld (GraphicsBounds.MinX),a
	ld (GraphicsBounds.MinY),a
	dec a
	ld (GraphicsBounds.MaxX),a
	ld a,191
	ld (GraphicsBounds.MaxY),a
	
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


WriteWord:
	push hl
	ld a,l
	call WriteByte
	pop hl
	ld a,h

PutChar:
WriteByte:

	; Check to see if we're waiting for anything in the command queue.
	ld c,a
	ld a,(CommandQueue.Waiting)
	or a
	jr z,VDU.WriteByteNotWaiting
	
	
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
	.dw Stub                \ .db  0 ;  0 NUL
	.dw Stub                \ .db  0 ;  1 Data -> Printer
	.dw Stub                \ .db  0 ;  2 Enable printer.
	.dw Stub                \ .db  0 ;  3 Disable printer.
	.dw Stub                \ .db  0 ;  4 Write text at text cursor position.
	.dw Stub                \ .db  0 ;  5 Write text at graphics cursor position.
	.dw Stub                \ .db  0 ;  6 Enable output to the screen.
	.dw Stub                \ .db  0 ;  7 BEL
	.dw Console.CursorLeft  \ .db  0 ;  8 Move text cursor backwards one character.
	.dw Console.CursorRight \ .db  0 ;  9 Move text cursor forwards one character.
	.dw Console.CursorDown  \ .db  0 ; 10 Move text cursor down a line.
	.dw Console.CursorUp    \ .db  0 ; 11 Move text cursor up a line.
	.dw Console.Clear       \ .db  0 ; 12 Clear the text area (CLS).
	.dw Console.HomeLeft    \ .db  0 ; 13 Move text cursor to start of current line.
	.dw Stub                \ .db  0 ; 14 Enable the auto-paging mode.
	.dw Stub                \ .db  0 ; 15 Disable the auto-paging mode.
	.dw Stub                \ .db  0 ; 16 Clear the graphics area (CLG).
	.dw Stub                \ .db -1 ; 17 Define a text colour (COLOUR).
	.dw Stub                \ .db -2 ; 18 Define a graphics colour (CGOL).
	.dw Stub                \ .db -5 ; 19 Select a colour palette.
	.dw Stub                \ .db  0 ; 20 Restore default logical colours.
	.dw Stub                \ .db  0 ; 21 Disable output to the screen.
	.dw ModeCommand         \ .db -1 ; 22 Set the screen mode (MODE).
	.dw Stub                \ .db -9 ; 23 User-defined characters and screen modes.
	.dw Stub                \ .db -8 ; 24 Define a graphics viewport.
	.dw Stub                \ .db -5 ; 25 PLOT
	.dw Stub                \ .db  0 ; 26 Restore default viewports.
	.dw PutLiteralChar      \ .db -1 ; 27 Send the next character to the screen.
	.dw Stub                \ .db -4 ; 28 Define a text viewport.
	.dw Stub                \ .db -4 ; 29 Set the graphics origin.
	.dw Console.HomeUp      \ .db  0 ; 30 Home the cursor to the top-left of the screen.
	.dw TabCommand          \ .db -2 ; 31 Move the text cursor (TAB(x,y)).

.function VDUQ(offset, commandLength)
	VDUQ = CommandQueue + offset + CommandQueue.Capacity - commandLength
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
;;; TODO
	
; ========================================================================================
; VDU 18,<mode>,<colour>                                               SET GRAPHICS COLOUR
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 20                                                                     RESET COLOURS
; ========================================================================================
;;; TODO

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
;;; TODO

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
;;; TODO

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

SetTextColourDefault:
	push bc
	push af
	pop af
	bit 7,a
	jr nz,SetTextBackgroundColour
SetTextForegroundColour:
	ld b,4
-:	add a,a
	djnz -
	ld b,a
	ld a,(VDU.TextColour)
	and $0F
	or b
	ld (VDU.TextColour),a
	jr DoneSetTextColour
SetTextBackgroundColour:
	and $0F
	ld b,a
	ld a,(VDU.TextColour)
	and $F0
	or b
	ld (VDU.TextColour),a
DoneSetTextColour:
	pop bc
	ret
	
SetGraphicsColourDefault:
	push bc
	push af
	pop af
	bit 7,a
	jr nz,SetGraphicsBackgroundColour
SetGraphicsForegroundColour:
	ld b,4
-:	add a,a
	djnz -
	ld b,a
	ld a,(VDU.GraphicsColour)
	and $0F
	or b
	ld (VDU.GraphicsColour),a
	jr DoneSetGraphicsColour
SetGraphicsBackgroundColour:
	and $0F
	ld b,a
	ld a,(VDU.GraphicsColour)
	and $F0
	or b
	ld (VDU.GraphicsColour),a
DoneSetGraphicsColour:
	pop bc
	ret

EnqueueGraphicsCursor:
	push hl
	push de
	push bc
	
	ld hl,GraphicsCursorQueue + (GraphicsCursorQueueLength - 1) * 4 - 1
	ld de,GraphicsCursorQueue + (GraphicsCursorQueueLength - 0) * 4 - 1
	ld bc,(GraphicsCursorQueueLength - 1) * 4
	lddr
	
	pop bc
	pop de
	pop hl
	
	ld (GraphicsCursorQueue+0),hl
	ld (GraphicsCursorQueue+2),de
	ret


PlotLine:
	ld hl,GraphicsCursorQueue + 0
	ld de,GraphicsCursorQueue + 4
	call Clip.Clip2DLine16
	ret c
	
	ld h,b
	ld l,c

; Draw a line from (D,E) to (H,L)
DrawLine:

	; Is the line steep (|dy|>|dx|) or shallow (|dx|>|dy|)?
	ld a,d
	sub h
	jr nc,+
	neg
+:	ld b,a
	
	ld a,e
	sub l
	jr nc,+
	neg
+:	ld c,a

	cp b
	jr c,DrawLine.Shallow

DrawLine.Steep:
	; Line is steep
	
	; Ensure that we draw it from top to bottom, so swap (D,E) and (H,L) if necessary.
	ld a,e
	cp l
	jr c,+
	ex de,hl
+:

	ld l,c

	; Ensure that when we adjust the X coordinate, we move in the correct direction
	ld a,d
	cp h
	ld c,+1
	jr c,+
	ld c,-1
+:

	ld h,b
	
	; H = |dx|, L = |dy|

	ld b,l
	inc b
	ld a,l ; Initial error
	srl a
	neg
	
-:	push hl
	push de
	push bc
	push af
	call SetPixel
	pop af
	pop bc
	pop de
	pop hl
	
	inc e ; Always moving down
	
	add a,h
	jr nc,+
	sub l
	
	push af
	ld a,d
	add a,c
	ld d,a
	pop af
	
+:	
	
	djnz -
	
	ret


DrawLine.Shallow:
	; Line is shallow

	; Ensure that we draw it from left to right, so swap (D,E) and (H,L) if necessary.
	ld a,d
	cp h
	jr c,+
	ex de,hl
+:

	; Ensure that when we adjust the Y coordinate, we move in the correct direction
	ld a,e
	cp l
	ld l,c
	ld c,+1
	jr c,+
	ld c,-1
+:

	ld h,b
	
	; H = |dx|, L = |dy|

	ld b,h
	inc b
	ld a,h ; Initial error
	srl a
	neg
	
-:	push hl
	push de
	push bc
	push af
	call SetPixel
	pop af
	pop bc
	pop de
	pop hl
	
	inc d ; Always moving right
	
	add a,l
	jr nc,+
	sub h
	
	push af
	ld a,e
	add a,c
	ld e,a
	pop af
	
+:	
	djnz -
	
	ret

.endmodule