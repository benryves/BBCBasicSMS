; =========================================================
; Module: Graphics
; =========================================================
; Handles drawing graphics to the screen.
; =========================================================
.module Graphics

MinX = allocVar(1)
MaxX = allocVar(1)
MinY = allocVar(1)
MaxY = allocVar(1)

g_wndXMin = MinX ; The g_wnd* variables must appear
g_wndXMax = MaxX ; in this order.
g_wndYMin = MinY
g_wndYMax = MaxY

OriginX              = allocVar(2) ; Stores the graphics origin.
OriginY              = allocVar(2)
VisitedPoints        = allocVar(0) ; Stores recently visited points.
VisitedPoint0        = allocVar(0)
VisitedPoint0X       = allocVar(2)
VisitedPoint0Y       = allocVar(2)
VisitedPoint1        = allocVar(0)
VisitedPoint1X       = allocVar(2)
VisitedPoint1Y       = allocVar(2)
VisitedPoint2        = allocVar(0)
VisitedPoint2X       = allocVar(2)
VisitedPoint2Y       = allocVar(2)
VisitedPoints.Size   = Variables - Graphics.VisitedPoints

TransformedPoints    = allocVar(0) ; Stores transformed points, ready for plotting to the screen.
TransformedPoint0    = allocVar(0)
TransformedPoint0X   = allocVar(2)
TransformedPoint0Y   = allocVar(2)
TransformedPoint1    = allocVar(0)
TransformedPoint1X   = allocVar(2)
TransformedPoint1Y   = allocVar(2)
TransformedPoint2    = allocVar(0)
TransformedPoint2X   = allocVar(2)
TransformedPoint2Y   = allocVar(2)

PlotShape  = allocVar(1)
PlotMode   = allocVar(1)
PlotColour = allocVar(1)

ColourMode = allocVar(1) ; GCOL <m>, ....
Colour     = allocVar(1) ; GCOL ..., <c>

.include "Clip.asm"
.include "Ellipse.asm"
.include "Triangle.asm"

Reset:

	; Clear the plot shape and visited points.
	xor a
	ld (PlotShape),a
	ld (PlotMode),a
	ld (ColourMode),a
	ld (VisitedPoints),a
	ld hl,OriginX
	ld (hl),a
	ld de,OriginX+1
	ld bc,VisitedPoints.Size+4-1 ; Extra 4 bytes for the graphics origin.
	ldir
	
	; Reset the colours.
	ld a,$0F
	ld (Colour),a
	ld (PlotColour),a
	; Fall-through to reset viewport.
	
ResetViewport:
	; Set default graphics bounds
	xor a
	ld (MinX),a
	ld (MinY),a
	dec a
	ld (MaxX),a
	ld a,191
	ld (MaxY),a
	
	ret

; ---------------------------------------------------------
; VisitPoint -> Visits a point for plotting.
; ---------------------------------------------------------
; Inputs:   hl = X coordinate to visit.
;           de = Y coordinate to visit. 
; Outputs:  None
; Destroys: f.
; ---------------------------------------------------------
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

; ---------------------------------------------------------
; DivideBy5 -> Divides HL by 5.
; ---------------------------------------------------------
; Inputs:   hl = value to divide by 5.
; Outputs:  hl is divided by 5.
; Destroys: f.
; ---------------------------------------------------------
DivideBy5:
	bit 7,h
	push af
	jr z,+
		ld a,h \ cpl \ ld h,a
		ld a,l \ cpl \ ld l,a
		inc hl
	+
	xor a
	.rept 16
		add	hl,hl
		rla
		cp 5
		jr c,$+5
		sub	5
		inc	l
	.loop
	pop af
	ret z
	push af
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
	pop af
	ret

; ---------------------------------------------------------
; MultiplyBy5 -> Multiplies HL by 5.
; ---------------------------------------------------------
; Inputs:   hl = value to multiply by 5.
; Outputs:  hl is multiplied by 5.
; Destroys: f.
; ---------------------------------------------------------
MultiplyBy5:
	push de
	ld d,h
	ld e,l
	add hl,hl
	add hl,hl
	add hl,de
	pop de
	ret

; ---------------------------------------------------------
; SortDEHL -> Sort DE <= HL.
; ---------------------------------------------------------
; Inputs:   de, hl: the two values to sort.
; Outputs:  de <= hl.
; Destroys: None.
; ---------------------------------------------------------
SortDEHL:
	or a
	sbc hl,de
	add hl,de
	ret z
	jp m,+
	
	ret po
	ex de,hl
	ret
	
+:	ret pe
	ex de,hl
	ret

; ---------------------------------------------------------
; SignedCPHLBC -> Compare signed HL and BC.
; ---------------------------------------------------------
; Inputs:   hl, bc: the two values to compare.
; Outputs:  z = signed equality.
;           c = set if HL<BC.
; Destroys: f.
; ---------------------------------------------------------
SignedCPHLBC:
	or a
	sbc hl,bc
	add hl,bc
	jr SignedCPHLDE.F

; ---------------------------------------------------------
; SignedCPHLDE -> Compare signed HL and DE.
; ---------------------------------------------------------
; Inputs:   hl, de: the two values to compare.
; Outputs:  z = signed equality.
;           c = set if HL<DE.
; Destroys: f.
; ---------------------------------------------------------
SignedCPHLDE:
	or a
	sbc hl,de
	add hl,de

SignedCPHLDE.F:
	jr z,SignedCPHLDE.Z
	jp m,SignedCPHLDE.M

SignedCPHLDE.P:
	scf
	ret pe
	ccf
	ret

SignedCPHLDE.M:
	scf
	ret po
	ccf
	ret

SignedCPHLDE.Z: ; If =Z, ensure =NC too.
	ret nc
	ccf
	ret

; ---------------------------------------------------------
; TransformPoints -> Converts visited point coordinates to
;                    physical screen coordinates.
; ---------------------------------------------------------
; Inputs:   VisitedPoints
;           b = number of points to transform.
; Outputs:  TransformedPoints
; Destroys: hl, de, bc
; ---------------------------------------------------------
TransformPoints:
	ld hl,(VisitedPoint0X)
	ld de,(VisitedPoint0Y)
	call TransformPoint
	ld (TransformedPoint0X),hl
	ld (TransformedPoint0Y),de
	dec b
	ret z
	ld hl,(VisitedPoint1X)
	ld de,(VisitedPoint1Y)
	call TransformPoint
	ld (TransformedPoint1X),hl
	ld (TransformedPoint1Y),de
	dec b
	ret z
	ld hl,(VisitedPoint2X)
	ld de,(VisitedPoint2Y)
	call TransformPoint
	ld (TransformedPoint2X),hl
	ld (TransformedPoint2Y),de
	ret

; ---------------------------------------------------------
; TransformPoint -> Converts a visited point coordinate to
;                   the physical screen coordinate.
; ---------------------------------------------------------
; Inputs:   (hl, de) = logical coordinates
; Outputs:  (hl, de) = physical screen coordinates
; Destroys: hl, de, f
; ---------------------------------------------------------
TransformPoint:
	push bc
	ex de,hl
	call DivideBy5
	ld c,l
	ld b,h
	ld hl,191
	or a
	sbc hl,bc
	ex de,hl
	call DivideBy5
	pop bc
	ret

; ---------------------------------------------------------
; Plot -> Plots a shape on the screen.
; ---------------------------------------------------------
; Inputs:   PlotShape = shape to plot.
;           VisitedPoints = points to plot.
; Destroys: Everything.
; ---------------------------------------------------------
Plot:
	
	; Assume that the plot mode = the colour mode.
	ld a,(ColourMode)
	ld (PlotMode),a
	
	; Plot mode 5 is a no-op.
	cp 5
	ret z
	
	; What's the plot shape?
	ld a,(PlotShape)
	and %11
	ret z ; Invisible!
	
	dec a
	jr nz,Plot.NotForegroundColour

Plot.ForegroundColour: ; PLOT %..01
	ld a,(Colour)
	ld (PlotColour),a
	jr Plot.SelectedColour

Plot.NotForegroundColour:
	dec a
	jr nz,Plot.BackgroundColour

Plot.Inverted:  ; PLOT %..10
	ld a,4
	ld (PlotMode),a
	ld a,$FF
	jr Plot.SelectedColour

Plot.BackgroundColour:  ; PLOT %..11
	ld a,(Colour)
	rrca
	rrca
	rrca
	rrca
	ld (PlotColour),a
	
Plot.SelectedColour:

	; Ensure the graphics mode driver is set up to plot.
	ld a,(PlotMode)
	and 7
	call BeginPlot

	; Check if the plot shape is out of bounds.
	; Note that we must do this down here, not earlier, so the Plot handler
	; can be used to initialse colours and plotting modes for VDU 5.
	ld a,(PlotShape)
	cp 208
	ret nc
	
	srl a
	srl a
	and %11111110
	ld c,a
	ld b,0
	ld hl,PlotCommands
	add hl,bc
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	jp (hl)

; Plot commands:
PlotCommands:
.dw PlotLine          ;     0..7: Regular lines.
.dw PlotLinePlusPixel ;    8..15: Lines but last point is omitted.
.dw PlotLine          ;   16..23: Dotted lines.
.dw PlotLinePlusPixel ;   24..31: Dotted lines (cont).
.dw PlotLine          ;   32..39: Dashed lines.
.dw PlotLinePlusPixel ;   40..47: Dashed lines (cont).
.dw PlotLine          ;   48..55: Broken lines.
.dw PlotLinePlusPixel ;   56..63: Broken lines (cont).
.dw PlotPixel         ;   64..71: Single point.
.dw Stub              ;   72..79: Horizontal line fill to non-background.
.dw PlotTriangle      ;   80..87: Triangle fill.
.dw Stub              ;   88..95: Horizontal line fill to background right.
.dw PlotRectangle     ;  96..103: Rectangle fill.
.dw Stub              ; 104..111: Horizontal line fill to foreground.
.dw Stub              ; 112..119: Horizontal line fill to foreground.
.dw Stub              ; 120..127: Horizontal line fill to non-foreground right.
.dw Stub              ; 128..135: Flood fill to non-background.
.dw Stub              ; 136..143: Flood fill to foreground.
.dw PlotDrawCircle    ; 144..151: Circle outline.
.dw PlotFillCircle    ; 152..159: Circle fill.
.dw Stub              ; 160..167: Draw circular arc.
.dw Stub              ; 168..175: Draw solid segment.
.dw Stub              ; 176..183: Draw solid sector.
.dw SpriteTest        ; 184..191: Block transfer operations.
.dw PlotDrawEllipse   ; 192..199: Ellipse outline.
.dw PlotFillEllipse   ; 200..207: Ellipse fill.

; ---------------------------------------------------------
; Plot -> Plots a line.
; ---------------------------------------------------------
; Inputs:   PlotShape = line type to plot.
;           VisitedPoints = points to plot.
; Destroys: Everything.
; ---------------------------------------------------------
PlotLine:

	ld b,2
	call TransformPoints

	ld hl,TransformedPoint0
	ld de,TransformedPoint1
	call Clip.Clip2DLine16
	ret c
	
	ld h,b
	ld l,c
	
SetLine:
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

; ---------------------------------------------------------
; PlotLinePlusPixel -> Plots a line, then draws the end
;                      pixel again.
; ---------------------------------------------------------
; This is useful for inverse lines, to avoid the connection
; between line segments from inverting themselves.
; ---------------------------------------------------------
; Inputs:   PlotShape = pixel type to plot.
;           VisitedPoint0 = End coordinate.
;           VisitedPoint1 = Start coordinate.
; Destroys: Everything.
; ---------------------------------------------------------
PlotLinePlusPixel:
	call PlotLine
	; Fall-through to PlotPixel

; ---------------------------------------------------------
; PlotPixel -> Plots a single pixel.
; ---------------------------------------------------------
; Inputs:   PlotShape = pixel type to plot.
;           VisitedPoint0 = Point coordinates.
; Destroys: Everything.
; ---------------------------------------------------------
PlotPixel:
	ld b,1
	call TransformPoints

	; Ensure that both points are in the range 0..255 at the very least (makes checking later easier).
	ld a,(TransformedPoint0X+1)
	or a
	ret nz
	ld a,(TransformedPoint0Y+1)
	or a
	ret nz
	
	; Now check that the point is within our graphics viewport.
	ld bc,(MinX) ; c = min, b = max
	
	ld a,(TransformedPoint0X+0)
	cp c
	ret c
	cp b
	jr z,+
	ret nc
+:	ld d,a
	
	ld bc,(MinY) ; c = min, b = max
	
	ld a,(TransformedPoint0Y+0)
	cp c
	ret c
	cp b
	jr z,+
	ret nc
+:	ld e,a

	jp SetPixel


; ---------------------------------------------------------
; PlotTransformedHorizontalSpan -> Plots a horizontal span
; ---------------------------------------------------------
; Inputs:   PlotShape = pixel type to plot.
;           (d,e) leftmost pixel to plot.
;           h = X coordinate of rightmost pixel to plot.
; Destroys: Everything.
; ---------------------------------------------------------
PlotTransformedHorizontalSpan:
	ld a,h
	sub d
	jr nc,+
	neg
	ld d,h
+:	inc a
	ld l,a
	
	; At this point, x = (d <= h); y = e; l = number of pixels.
	
	ld a,(SetAlignedHorizontalLineSegment)
	cp $C9
	jr z,NoSetAlignedHorizontalLineSegment
	
	push de
	push hl
	
	; Are d and h in the same 8-byte aligned pixel?
	srl d
	srl d
	srl d
	
	srl h
	srl h
	srl h
	
	ld a,h
	sub d
	jr z,SingleAlignedSegment
	
	; We have at least a start cap and an end cap.
	; There may also be solid columns in the middle to fill.
	
	pop hl
	pop de
	
	push af
	push de
	push hl
	
	; Left side mask.
	ld c,%11111111
	ld a,d
	and %00000111
	jr z,+
	ld b,a
-:	srl c
	djnz -
+:
	
	ld a,c
	cpl
	ld h,c
	ld l,a
	
	call SetAlignedHorizontalLineSegment
	ei
	
	pop hl
	pop de
	pop af
	
	dec a
	jr z,NoFullMiddleSegments
	
	push hl
	push de
	ld b,a
	
-:	ld a,8
	add a,d
	ld d,a
	push de
	push bc
	ld hl,$FF00
	call SetAlignedHorizontalLineSegment
	ei
	pop bc
	pop de
	djnz -
	
	pop de
	pop hl

NoFullMiddleSegments:
	
	push de
	push hl
	
	; Right side mask.
	
	ld d,h
	ld c,%10000000
	ld a,d
	and %00000111
	jr z,+
	ld b,a
-:	sra c
	djnz -
+:
	
	ld a,c
	cpl
	ld h,c
	ld l,a
	
	call SetAlignedHorizontalLineSegment
	ei
	pop hl
	pop de
	ret


SingleAlignedSegment:
	pop hl
	pop de

NoSetAlignedHorizontalLineSegment:

	ld b,l
	
-:	push de
	push bc
	call SetPixel
	pop bc
	pop de
	inc d
	djnz -
	ei
	ret
	

SpriteTest:

	ld a,'A'
	call PutChar
	ret

	ld b,1
	call TransformPoints
	
	push ix
	
	ld ix,VDU.Fonts.Font8x8	 + ('A'*1+FontCharOffset)*8
	
	ld a,(TransformedPoint0X)
	ld d,a
	ld a,(TransformedPoint0Y)
	ld e,a
	
	call PlotTransformedSprite
	
	pop ix
	ret

; ---------------------------------------------------------
; PutChar -> Draws a character at the graphics cursor and
; advances right.
; ---------------------------------------------------------
PutChar:
	call PutMap
	jr CursorRight

; ---------------------------------------------------------
; PutMap -> Draws a character at the graphics cursor.
; ---------------------------------------------------------
PutMap:
	push ix
	push hl
	push de
	push bc	
	push af
	push af
	
	cp 127
	jr z,PutMap.Delete

	ld a,1
	ld (PlotShape),a

	ld a,(Colour)
	ld (PlotColour),a
	
	ld a,(ColourMode)
	and 7
	jr PutMap.BeginPlot

PutMap.Delete:

	ld a,3
	ld (PlotShape),a

	ld a,(Colour)
	rrca
	rrca
	rrca
	rrca
	ld (PlotColour),a
	xor a

PutMap.BeginPlot:
	ld (PlotMode),a
	call BeginPlot
	
	ld b,1
	call TransformPoints
	
	pop af
	add a,FontCharOffset
	
	ld l,a
	ld h,0
	
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,VDU.Fonts.Font8x8
	add hl,de
	
	push hl
	pop ix
	
	ld a,(TransformedPoint0X)
	ld d,a
	ld a,(TransformedPoint0Y)
	ld e,a
	
	call PlotTransformedSprite
	
	pop af
	pop bc
	pop de
	pop hl
	pop ix
	ret

; ---------------------------------------------------------
; CursorRight -> Move the cursor right one character.
; ---------------------------------------------------------
CursorRight:
	push hl
	push de
	push bc
	
	ld hl,(VisitedPoint0X)
	ld bc,8*5
	add hl,bc
	
	push hl
	call DivideBy5
	ld bc,(MaxX)
	ld b,0
	call SignedCPHLBC
	pop hl
	
	jr c,CursorRightNoWrap
	
	; Cursor home.
	ld hl,(MinX)
	ld h,0
	call MultiplyBy5
	jr CursorDown.FromCursorRight
	
CursorRightNoWrap:

	ld de,(VisitedPoint0Y)
	call VisitPoint
	
	pop bc
	pop de
	pop hl
	ret

; ---------------------------------------------------------
; CursorDown -> Move the cursor down one character.
; ---------------------------------------------------------
CursorDown:
	push hl
	push de
	push bc
	
	ld hl,(VisitedPoint0X)

CursorDown.FromCursorRight:

	ex de,hl
	
	ld hl,(VisitedPoint0Y)
	ld bc,-8*5
	add hl,bc
	
	push hl
	
	call DivideBy5
	
	ld a,(MaxY)
	neg
	add a,192
	ld c,a
	ld b,0
	call SignedCPHLBC
	pop hl
	
	jr nc,CursorDownNoWrap

	ld a,(MinY)
	neg
	add a,192
	ld l,a
	ld h,0
	call MultiplyBy5
	dec hl

CursorDownNoWrap:
	
	ex de,hl
	call VisitPoint
	
	pop bc
	pop de
	pop hl
	ret

; ---------------------------------------------------------
; CursorLeft -> Move the cursor left one character.
; ---------------------------------------------------------
CursorLeft:
	push hl
	push de
	push bc
	
	ld hl,(VisitedPoint0X)
	ld bc,-8*5
	add hl,bc
	
	push hl
	call DivideBy5
	ld bc,(MinX)
	ld b,0
	call SignedCPHLBC
	pop hl
	
	jr nc,CursorLeftNoWrap
	
	; Cursor to right edge.
	ld hl,(MaxX)
	ld h,0
	ld bc,-8
	add hl,bc
	call MultiplyBy5
	jr CursorUp.FromCursorLeft
	
CursorLeftNoWrap:

	ld de,(VisitedPoint0Y)
	call VisitPoint
	
	pop bc
	pop de
	pop hl
	ret
	ret

; ---------------------------------------------------------
; CursorUp -> Move the cursor up one character.
; ---------------------------------------------------------
CursorUp:
	push hl
	push de
	push bc
	
	ld hl,(VisitedPoint0X)

CursorUp.FromCursorLeft:

	ex de,hl
	
	ld hl,(VisitedPoint0Y)
	ld bc,8*5
	add hl,bc
	
	push hl
	
	call DivideBy5
	
	ld a,(MinY)
	neg
	add a,191
	ld c,a
	ld b,0
	call SignedCPHLBC
	pop hl
	
	jr c,CursorDownNoWrap

	ld a,(MinY)
	neg
	add a,192
	ld l,a
	ld h,0
	call MultiplyBy5
	dec hl

CursorDownNoWrap:
	
	ex de,hl
	call VisitPoint
	
	pop bc
	pop de
	pop hl
	ret
	
; ---------------------------------------------------------
; HomeLeft -> Move the cursor home to the left edge.
; ---------------------------------------------------------
HomeLeft:
	push hl
	push de
	ld hl,(MinX)
	ld h,0
	call MultiplyBy5
	ld de,(VisitedPoint0Y)
	call VisitPoint
	pop de
	pop hl
	ret

; ---------------------------------------------------------
; PlotTransformedSprite -> Plots a sprite.
; ---------------------------------------------------------
; Inputs:   PlotShape = pixel type to plot.
;           (d,e) top left corner.
;           ix = pointer to sprite data.
; Destroys: Everything.
; ---------------------------------------------------------
PlotTransformedSprite:

	ld b,8

PlotTransformedSprite.Loop:
	
	push bc
	
	ld h,(ix)
	ld a,d
	and 7
	jr z,+
	
	ld b,a
-:	srl h
	djnz -
+:	ld a,h
	cpl
	ld l,a
	
	push de
	call SetAlignedHorizontalLineSegment
	pop de
	
	ld a,d
	and 7
	jr z,PlotTransformedSprite.Aligned
	
	ld h,(ix)
	
	ld l,a
	ld a,8
	sub l
	
	ld b,a
-:	sla h
	djnz -
	
	ld a,h
	cpl
	ld l,a
	
	push de
	ld a,d
	add a,8
	ld d,a
	call SetAlignedHorizontalLineSegment
	pop de

PlotTransformedSprite.Aligned:
	
	pop bc
	inc e
	inc ix
	djnz PlotTransformedSprite.Loop
	
	ret	
	

; ---------------------------------------------------------
; Clear -> Clears the viewport.
; ---------------------------------------------------------
; Inputs:   PlotShape = rectangle type to plot.
;           VisitedPoint0 = One corner.
;           VisitedPoint1 = The other corner.
; Destroys: Everything.
; ---------------------------------------------------------
Clear:
	

	
	ld hl,(MinX)
	ld h,0
	ld (TransformedPoint0X),hl
	
	ld hl,(MinY)
	ld h,0
	ld (TransformedPoint0Y),hl
	
	ld hl,(MaxX)
	ld h,0
	ld (TransformedPoint1X),hl
	
	ld hl,(MaxY)
	ld h,0
	ld (TransformedPoint1Y),hl
	
	; Set the shape to plot to be a rectangle.
	ld a,103
	ld (Graphics.PlotShape),a	
	
	; Set the plot colour to the background colour.
	ld a,(Colour)
	rrca
	rrca
	rrca
	rrca
	ld (PlotColour),a
	
	; Load the plot mode.
	ld a,(ColourMode)
	ld (PlotMode),a
	and 7
	call BeginPlot
	
	jr PlotTransformedRectangle

; ---------------------------------------------------------
; PlotRectangle -> Fills a rectangle.
; ---------------------------------------------------------
; Inputs:   PlotShape = rectangle type to plot.
;           VisitedPoint0 = One corner.
;           VisitedPoint1 = The other corner.
; Destroys: Everything.
; ---------------------------------------------------------
PlotRectangle:
	; <corner>, <corner>
	ld b,2
	call TransformPoints

PlotTransformedRectangle:

	; Sort X so left <= right
	ld hl,(TransformedPoint0X)
	ld de,(TransformedPoint1X)
	call SortDEHL
	
	; Is left edge > right viewport bound?
	bit 7,d
	jr nz,+ ; If DE<0, we can't be past the right edge.
	ld a,d
	or a
	ret nz ; If D is non-zero, DE>=256 and must be off-screen.
	ld a,(MaxX)
	cp e
	ret c
+:	; Our left edge <= the right viewport edge.
	; Clamp to the left viewport edge.
	bit 7,d
	jr z,+ ; 0<=DE<=255
	ld e,0
+:	ld a,(MinX)
	cp e
	jr c,+
	ld e,a
+:

	; Is right edge < left viewport bound?
	bit 7,h
	ret nz ; If HL<0, we must be off-screen.
	ld a,h
	or a
	jr nz,+ ; If H is non-zero, HL>=256 and can't be to the left of the left viewport edge.
	ld a,(MinX)
	cp l
	jr z,+
	ret nc
+:	; Our right edge >= the left viewport edge.
	; Clamp to the right viewport edge.
	ld a,h
	or a
	jr z,+ ; 0<=HL<=255
	ld l,255
+:	ld a,(MaxX)
	cp l
	jr nc,+
	ld l,a
+:

	ld (TransformedPoint0X),de
	ld (TransformedPoint1X),hl
	
	; Sort Y so top <= bottom
	ld hl,(TransformedPoint0Y)
	ld de,(TransformedPoint1Y)
	call SortDEHL
	
	; Is top edge > bottom viewport bound?
	bit 7,d
	jr nz,+ ; If DE<0, we can't be past the bottom edge.
	ld a,d
	or a
	ret nz ; If D is non-zero, DE>=256 and must be off-screen.
	ld a,(MaxY)
	cp e
	ret c
+:	; Our top edge <= the bottom viewport edge.
	; Clamp to the top viewport edge.
	bit 7,d
	jr z,+ ; 0<=DE<=255
	ld e,0
+:	ld a,(MinY)
	cp e
	jr c,+
	ld e,a
+:

	; Is bottom edge < top viewport bound?
	bit 7,h
	ret nz ; If HL<0, we must be off-screen.
	ld a,h
	or a
	jr nz,+ ; If H is non-zero, HL>=256 and can't be to the top of the top viewport edge.
	ld a,(MinY)
	cp l
	jr z,+
	ret nc
+:	; Our bottom edge >= the top viewport edge.
	; Clamp to the bottom viewport edge.
	ld a,h
	or a
	jr z,+ ; 0<=HL<=255
	ld l,255
+:	ld a,(MaxY)
	cp l
	jr nc,+
	ld l,a
+:
	
	ld (TransformedPoint0Y),de
	ld (TransformedPoint1Y),hl
	
	; Draw the rectangle.
	
	ld a,(TransformedPoint0X)
	ld d,a
	ld a,(TransformedPoint0Y)
	ld e,a
	
	ld a,(TransformedPoint1X)
	ld h,a
	ld a,(TransformedPoint1Y)
	sub e
	inc a
	ld l,a
	
-:	push de
	push hl
	call PlotTransformedHorizontalSpan
	pop hl
	pop de
	inc e
	dec l
	jr nz,-
	
	ret

; ---------------------------------------------------------
; PlotDrawCircle -> Draws a circle outline.
; ---------------------------------------------------------
; Inputs:   PlotShape = circle type to plot.
;           VisitedPoint0 = Point on circumference.
;           VisitedPoint1 = Center.
; Destroys: Everything.
; ---------------------------------------------------------
PlotDrawCircle:
	push iy
	ld iy,PlotShape
	res fFillEllipse,(iy+ellipseFlags)
	jr PlotCircle

; ---------------------------------------------------------
; PlotFillCircle -> Fills a circle.
; ---------------------------------------------------------
; Inputs:   PlotShape = circle type to plot.
;           VisitedPoint0 = Point on circumference.
;           VisitedPoint1 = Center.
; Destroys: Everything.
; ---------------------------------------------------------
PlotFillCircle:
	push iy
	ld iy,PlotShape
	set fFillEllipse,(iy+ellipseFlags)

PlotCircle:
	; <center>, <radius>
	ld b,2
	call TransformPoints
	
	; dx = ?
	ld hl,(TransformedPoint0X)
	ld de,(TransformedPoint1X)
	or a
	sbc hl,de
	ld a,h
	or l
	jr z,CircleRadiusZeroWidth
	ld c,l
	ld b,h
	
	; dy = ?
	ld hl,(TransformedPoint0Y)
	ld de,(TransformedPoint1Y)
	or a
	sbc hl,de
	ld a,h
	or l
	jr z,CircleRadiusZeroHeight
	
	
	; If we get this far, the radius is defined by a point on the radius that is
	; not on the same axis as the centre.
	push hl
	
	call Maths.AbsBC
	call SquareBC
	
	pop bc
	push hl
	push de
	
	call Maths.AbsBC
	call SquareBC
	
	ex de,hl
	
	pop bc
	add hl,bc
	jr nc,+
	inc de
+:	ex de,hl
	pop bc
	add hl,bc
	
	; HL:DE = (dx*dx)+(dy*dy)

	call Maths.Sqrt32 ; DE = SQR(HL:DE)
	
	jr CircleFoundRadius

CircleRadiusZeroWidth:
	
	; Get dy
	ld hl,(TransformedPoint0Y)
	ld de,(TransformedPoint1Y)
	or a
	sbc hl,de
	jr CalculateRadiusZeroAxis

CircleRadiusZeroHeight:
	
	; We still have dx in bc
	ld l,c
	ld h,b
	
CalculateRadiusZeroAxis:
	
	bit 7,h
	jr z,+
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
+:	ex de,hl
	jr CircleFoundRadius

CircleFoundRadius:

	; Copy the centre of the ellipse into the right field.
	ld hl,(TransformedPoint1X)
	ld (g_ellipseCX),hl
	ld hl,(TransformedPoint1Y)
	ld (g_ellipseCY),hl

	; Store radius.
	ld (g_ellipseRX),de
	ld (g_ellipseRY),de

	call DrawEllipse
	
	pop iy
	ret

; ---------------------------------------------------------
; PlotDrawEllipse -> Draws an ellipse outline.
; ---------------------------------------------------------
; Inputs:   PlotShape = ellipse type to plot.
;           VisitedPoint0 = Point on h tangent (v radius)
;           VisitedPoint1 = Point on v tangent (h radius)
;           VisitedPoint2 = Center.
; Destroys: Everything.
; ---------------------------------------------------------
PlotDrawEllipse:
	push iy
	ld iy,PlotShape
	res fFillEllipse,(iy+ellipseFlags)
	jr PlotEllipse
	
; ---------------------------------------------------------
; PlotFillEllipse -> Fills an ellipse.
; ---------------------------------------------------------
; Inputs:   PlotShape = ellipse type to plot.
;           VisitedPoint0 = Point on h tangent (v radius)
;           VisitedPoint1 = Point on v tangent (h radius)
;           VisitedPoint2 = Center.
; Destroys: Everything.
; ---------------------------------------------------------
PlotFillEllipse:
	push iy
	ld iy,PlotShape
	set fFillEllipse,(iy+ellipseFlags)

PlotEllipse:
	; <center>, <horizontal radius>, <vertical radius>
	ld b,3
	call TransformPoints
	
	ld de,(TransformedPoint2X)
	ld bc,(TransformedPoint2Y)
	
	; Calculate the horizontal radius.
	
	ld hl,(TransformedPoint1X)
	or a
	sbc hl,de
	call Maths.AbsHL
	
	ld (g_ellipseRX),hl
	
	; Calculate the vertical radius.
	
	ld hl,(TransformedPoint0Y)
	or a
	sbc hl,bc
	call Maths.AbsHL
	
	ld (g_ellipseRY),hl
	
	; Copy the centre coordinates to the right memory location.
	ld (g_ellipseCX),de
	ld (g_ellipseCY),bc
	
	call DrawEllipse
	
	pop iy
	ret

; ---------------------------------------------------------
; PlotTriangle -> Fills a triangle.
; ---------------------------------------------------------
; Inputs:   PlotShape = triangle type to plot.
;           VisitedPoint0 = First coordinate
;           VisitedPoint1 = Second coordinate.
;           VisitedPoint2 = Third coordinate.
; Destroys: Everything.
; ---------------------------------------------------------
PlotTriangle:
	ld b,3
	call TransformPoints
	jp Triangle.Fill

; ---------------------------------------------------------
; ClampPhysicalHLX -> Clamps transformed HL X value to
;                     physical screen coordinates.
; ---------------------------------------------------------
; Inputs:   hl = X value to clamp.
; Outputs:  l = clamped value 0..255.
; Destroys: af.
; ---------------------------------------------------------
ClampTransformedHLX:
	bit 7,h
	jr z,+
	ld l,0
	ret
+:	ld a,h
	or a
	ret z
	ld l,255
	ret

; ---------------------------------------------------------
; ClampPhysicalDEY -> Clamps transformed DE Y value to
;                     physical screen coordinates.
; ---------------------------------------------------------
; Inputs:   de = Y value to clamp.
; Outputs:  e = clamped value 0..191.
; Destroys: af.
; ---------------------------------------------------------
ClampTransformedDEY:
	bit 7,d
	jr z,+
	ld e,0
	ret
+:	ld a,d
	or a
	jr z,+
	ld e,191
	ret
+:	ld a,e
	cp 192
	ret c
	ld e,191
	ret
	
.endmodule