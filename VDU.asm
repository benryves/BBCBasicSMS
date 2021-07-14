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
.include "bbc"

Font6x8:
.incbin "Font6x8.bin"

.endmodule

PutMap = allocVar(3)
Scroll = allocVar(3)
SetTextColour = allocVar(3)
SetGraphicsColour = allocVar(3)
SetPixel = allocVar(3)

GraphicsCursorQueueLength = 2
GraphicsCursorQueue = allocVar(GraphicsCursorQueueLength * 4)

.struct BoundingBox
	ubyte MinX,
	ubyte MaxX,
	ubyte MinY,
	ubyte MaxY

GraphicsBounds.MinX = allocVar(1)
GraphicsBounds.MaxX = allocVar(1)
GraphicsBounds.MinY = allocVar(1)
GraphicsBounds.MaxY = allocVar(1)

g_wndXMin = GraphicsBounds.MinX ; The g_wnd* variables must appear
g_wndXMax = GraphicsBounds.MaxX ; in this order.
g_wndYMin = GraphicsBounds.MinY
g_wndYMax = GraphicsBounds.MaxY

TextColour = allocVar(1)
GraphicsColour = allocVar(1)

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
	call HomeUp
	
	; Clear the point queue
	xor a
	ld (GraphicsCursorQueue),a
	ld hl,GraphicsCursorQueue
	ld de,GraphicsCursorQueue+1
	ld bc,GraphicsCursorQueueLength*4-1
	ldir
	
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

CurRow = allocVar(1)
CurCol = allocVar(1)

MinRow = allocVar(1)
MaxRow = allocVar(1)
MinCol = allocVar(1)
MaxCol = allocVar(1)

HomeUp:
	ld a,(MinCol)
	ld (CurCol),a
	ld a,(MinRow)
	ld (CurRow),a
	ret
	
PutChar:
	cp '\r'
	jr nz,+
	ld a,(MinCol)
	ld (CurCol),a
	ret

+:	cp '\n'
	jr z,NewLine

+:	call PutMap
	; Fall-through to CursorRight

CursorRight:
	ld a,(CurCol)
	inc a
	push bc
	ld bc,(MaxCol)
	cp c
	pop bc
	jr nz,+
	ld a,(MinCol)
+:	ld (CurCol),a
	ret nz
	; Fall-through to NewLine

NewLine:
	ld a,(MinCol)
	ld (CurCol),a

CursorDown:
	ld a,(CurRow)
	inc a
	push bc
	ld bc,(MaxRow)
	cp c
	pop bc
	jr nz,+
	call Scroll
	ld a,(MaxRow)
	dec a
+:	ld (CurRow),a
	ret

CursorLeft:
	ld a,(CurCol)
	push bc
	ld bc,(MinCol)
	cp c
	pop bc
	jr nz,+
	ld a,(MaxCol)
+:	push af
	dec a
	ld (CurCol),a
	pop af
	ret nz
+:	; Move up a row
	ld a,(CurRow)
	push bc
	ld bc,(MinRow)
	cp c
	pop bc
	jr nz,+
	ld a,(MaxRow)
+:	push af
	dec a
	ld (CurRow),a
	pop af
	ret

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

SerialIdleAnimationLast = allocVar(1)

StartSerialIdleAnimation:
	push af
	push bc
	push de
	push hl
	ld a,(Host.TIME+0)
	
	srl a
	srl a
	srl a
	srl a
	and %11
	ld (SerialIdleAnimationLast),a
	
	ld hl,SerialIdleAnimation
	call Serial.SetIdleCallback
	ld a,127
	call PutMap
	pop hl
	pop de
	pop bc
	pop af
	ret

StopSerialIdleAnimation:
	push af
	push bc
	push de
	push hl
	call Serial.ClearIdleCallback
	ld a,' '
	call PutMap
	pop hl
	pop de
	pop bc
	pop af
	ret
	

SerialIdleAnimation:
	in a,(Video.Control)
	bit 7,a
	ret z
	
	call FrameInterrupt
	
	ld a,(Host.TIME+0)
	srl a
	srl a
	srl a
	srl a
	and %11
	ld e,a
	ld a,(SerialIdleAnimationLast)
	cp e
	ret z
	
	ld a,e
	ld (SerialIdleAnimationLast),a
	
	ld d,0
	ld hl,IdleAnimationFrames
	add hl,de
	ld a,(hl)
	call VDU.PutMap
	
	ret

IdleAnimationFrames:
.db "|"
.db "/"
.db "-"
.db "\\"

.endmodule