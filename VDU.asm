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

.var ubyte[3] PutMap
.var ubyte[3] Scroll
.var ubyte[3] SetTextColour
.var ubyte[3] SetGraphicsColour
.var ubyte[3] SetPixel

GraphicsCursorQueueLength = 2
.var uword[GraphicsCursorQueueLength * 2] GraphicsCursorQueue

.struct BoundingBox
	ubyte MinX,
	ubyte MaxX,
	ubyte MinY,
	ubyte MaxY

.var BoundingBox GraphicsBounds

.define g_wndXMin GraphicsBounds.MinX ; The g_wnd* variables must appear
.define g_wndXMax GraphicsBounds.MaxX ; in this order.
.define g_wndYMin GraphicsBounds.MinY
.define g_wndYMax GraphicsBounds.MaxY

.var ubyte TextColour
.var ubyte GraphicsColour

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

.var ubyte CurRow, CurCol
.var ubyte MinRow, MaxRow, MinCol, MaxCol

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
	jr nz,+
	
	ld a,(MinCol)
	ld (CurCol),a
	jr NewLine

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

.endmodule