; =========================================================
; Module: Graphics
; =========================================================
; Handles drawing graphics to the screen.
; =========================================================
.module Graphics

.include "Clip.asm"

OriginX = allocVar(2)
OriginY = allocVar(2)

MinX = allocVar(1)
MaxX = allocVar(1)
MinY = allocVar(1)
MaxY = allocVar(1)

g_wndXMin = MinX ; The g_wnd* variables must appear
g_wndXMax = MaxX ; in this order.
g_wndYMin = MinY
g_wndYMax = MaxY

VisitedPoints         = allocVar(0) ; Stores recently visited points.
VisitedPoint0X       = allocVar(2)
VisitedPoint0Y       = allocVar(2)
VisitedPoint1X       = allocVar(2)
VisitedPoint1Y       = allocVar(2)
VisitedPoint2X       = allocVar(2)
VisitedPoint2Y       = allocVar(2)
VisitedPoints.Size    = Variables - Graphics.VisitedPoints

PlotShape = allocVar(1)

Colour = allocVar(1)

Reset:

	; Clear the plot shape and visited points.
	xor a
	ld (PlotShape),a
	ld (VisitedPoints),a
	ld hl,VisitedPoints
	ld de,VisitedPoints+1
	ld bc,VisitedPoints.Size-1
	ldir	

	; Set default graphics bounds
	xor a
	ld (MinX),a
	ld (MinY),a
	dec a
	ld (MaxX),a
	ld a,191
	ld (MaxY),a
	
	ret

VisitPoint:
	; Shift the visited point buffer.
	push hl
	push de
	push bc
	
	ld hl,VisitedPoints + VisitedPoints.Size - 4 - 1
	ld de,VisitedPoints + VisitedPoints.Size - 1
	ld bc,VisitedPoints.Size - 4
	lddr
	
	pop bc
	pop de
	pop hl
	
	; Store the new point at the top of the buffer.
	ld (VisitedPoint0Y),de
	
	push hl
	
	ld de,(OriginX)
	add hl,de
	ld (VisitedPoint0X),hl
	
	push de
	
	ld hl,(VisitedPoint0Y)
	ld de,(OriginY)
	add hl,de
	ld (VisitedPoint0Y),hl
	
	pop de
	
	pop hl
	ret

Plot:
	
	ld a,(PlotShape)
	ld c,a
	and %11
	ret z ; Invisible!
	
	; 0..63 = lines
	ld a,c
	cp 64
	jp c,PlotLine
	ret
	

PlotLine:
	ld hl,VisitedPoint0X
	ld de,VisitedPoint1X
	call Clip.Clip2DLine16
	ret c
	
	ld h,b
	ld l,c

	; Draw a line from (D,E) to (H,L)

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
	jr c,PlotLine.Shallow

PlotLine.Steep:
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
	call PlotPixel
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


PlotLine.Shallow:
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
	call PlotPixel
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
	

PlotPixel:
	ld a,(PlotShape)
	and %11
	ret z ; No pixel
	dec a
	jp z,SetForegroundPixel
	dec a
	jp z,InvertPixel
	jp SetBackgroundPixel

.endmodule