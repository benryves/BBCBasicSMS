; ==========================================================================
; Graphics
; --------------------------------------------------------------------------
; Handles drawing graphics to the screen.
; ==========================================================================
.module Graphics

MinX = allocVar(1)
MaxX = allocVar(1)
MinY = allocVar(1)
MaxY = allocVar(1)

OffsetX = allocVar(1)
OffsetY = allocVar(1)
MaxWidth = allocVar(1)
MaxHeight = allocVar(1)

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

PlotMode   = allocVar(1) ; \ Must appear in this order!
PlotColour = allocVar(1) ; /

ForegroundMode   = allocVar(1) ; GCOL <m>, .... \
ForegroundColour = allocVar(1) ; GCOL ..., <c>   \ Must appear in this order!
BackgroundMode   = allocVar(1) ; GCOL <m>, ....  /
BackgroundColour = allocVar(1) ; GCOL ..., <c>  /

DivideCoordinate   = allocVar(3)
MultiplyCoordinate = allocVar(3)

.include "Clip.asm"
.include "Ellipse.asm"
.include "Triangle.asm"

; ==========================================================================
; Reset
; --------------------------------------------------------------------------
; Resets the graphics module.
; --------------------------------------------------------------------------
; Destroyed: AF, HL, DE, BC.
; ==========================================================================
Reset:

	; Clear the plot shape and visited points.
	xor a
	ld (PlotShape),a
	ld (PlotMode),a
	ld (ForegroundMode),a
	ld (BackgroundMode),a
	ld (BackgroundColour),a
	ld (VisitedPoints),a
	ld hl,OriginX
	ld (hl),a
	ld de,OriginX+1
	ld bc,VisitedPoints.Size+4-1 ; Extra 4 bytes for the graphics origin.
	ldir
	
	; Reset the colours.
	ld a,15
	ld (ForegroundColour),a
	ld (PlotColour),a
	; Fall-through to reset viewport.

; ==========================================================================
; ResetViewport
; --------------------------------------------------------------------------
; Resets the graphics viewport.
; --------------------------------------------------------------------------
; Destroyed: AF, HL, DE, BC.
; ==========================================================================
ResetViewport:
	; Set default graphics bounds
	xor a
	ld (OffsetY),a
	ld (MinY),a
	ld a,8
	ld (OffsetX),a
	ld (MinX),a
	ld a,255-8
	ld (MaxX),a
	ld a,191
	ld (MaxY),a
	
	; Now invoke the driver-specific version.
	ld a,Driver.Execute.ResetGraphicsViewport
	call Driver.Execute
	
	; Calculate the maximum bounds from the maximum coordinate - offset.
	ld a,(OffsetX)
	ld b,a
	ld a,(MaxX)
	sub b
	ld (MaxWidth),a
	
	ld a,(OffsetY)
	ld b,a
	ld a,(MaxY)
	sub b
	ld (MaxHeight),a
	ret
	
; ==========================================================================
; ResetOrigin
; --------------------------------------------------------------------------
; Resets the graphics origin.
; --------------------------------------------------------------------------
; Destroyed: HL.
; ==========================================================================
ResetOrigin:
	ld hl,0
	ld (OriginX),hl
	ld (OriginY),hl
	ret

; ==========================================================================
; VisitPointAbsolute
; --------------------------------------------------------------------------
; Visits an absolute point for plotting.
; --------------------------------------------------------------------------
; Inputs:    HL: absolute X coordinate to visit.
;            DE: absolute Y coordinate to visit.
; Outputs:   VisitedPoint0
; Destroyed: F.
; ==========================================================================
VisitPointAbsolute:
	call ShiftPointBuffer
	ld (VisitedPoint0X),hl
	ld (VisitedPoint0Y),de
	ret

; ==========================================================================
; ReturnCursorToOrigin
; --------------------------------------------------------------------------
; Returns the graphics cursor to the origin.
; --------------------------------------------------------------------------
; Outputs:   VisitedPoint0
; Destroyed: HL, DE, F.
; ==========================================================================
ReturnCursorToOrigin:
	ld hl,0
	ld de,0
	; Fall-through to VisitPoint

; ==========================================================================
; VisitPoint
; --------------------------------------------------------------------------
; Visits a point for plotting.
; --------------------------------------------------------------------------
; Inputs:    HL: X coordinate to visit.
;            DE: Y coordinate to visit.
; Outputs:   VisitedPoint0
; Destroyed: F.
; ==========================================================================
VisitPoint:
	
	call ShiftPointBuffer
	
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

; ==========================================================================
; ShiftPointBuffer
; --------------------------------------------------------------------------
; Shifts the point buffer before adding a new one.
; --------------------------------------------------------------------------
; Inputs:    VisitedPoints
; Outputs:   VisitedPoints
; Destroyed: F.
; ==========================================================================
ShiftPointBuffer:
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
	ret

; ==========================================================================
; DivideBy5
; --------------------------------------------------------------------------
; Divides HL by five.
; --------------------------------------------------------------------------
; Inputs:    HL: Value to divide by five.
; Outputs:   HL: Value divided by five.
; Destroyed: F.
; ==========================================================================
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

; ==========================================================================
; MultiplyBy5
; --------------------------------------------------------------------------
; Multiplies HL by five.
; --------------------------------------------------------------------------
; Inputs:    HL: Value to multiply by five.
; Outputs:   HL: Value multiplied by five.
; Destroyed: F.
; ==========================================================================
MultiplyBy5:
	push de
	ld d,h
	ld e,l
	add hl,hl
	add hl,hl
	add hl,de
	pop de
	ret

; ==========================================================================
; DivideBy5T
; --------------------------------------------------------------------------
; Divides HL by five and a third.
; --------------------------------------------------------------------------
; This actually multiplies by three then divides by sixteen, which is a much
; easier calculation.
; --------------------------------------------------------------------------
; Inputs:    HL: Value to divide by five and a third.
; Outputs:   HL: Value divided by five and a third.
; Destroyed: None.
; ==========================================================================
DivideBy5T:
	push af
	push de
	
	ld d,h
	ld e,l
	
	add hl,hl
	sbc a,a
	add hl,de
	adc a,0
	
	sra h \ rr l
	sra h \ rr l
	sra h \ rr l
	sra h \ rr l
	
	pop de
	pop af
	ret

; ==========================================================================
; MultiplyBy5T
; --------------------------------------------------------------------------
; Multiplies HL by five and a third.
; --------------------------------------------------------------------------
; Inputs:    HL: Value to multiply by five and a third.
; Outputs:   HL: Value multiplied by five and a third.
; Destroyed: None.
; ==========================================================================
MultiplyBy5T:
	push af
	push de
	
	bit 7,h
	jr z,MultiplyBy5TP
	
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
	
	call MultiplyBy5T
	
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
	
	pop de
	pop af
	ret

MultiplyBy5TP:

	; Store the accumlator in EHL
	xor a
	ld e,a
	
	; Multiply by 16
	add hl,hl
	sbc a,a
	add hl,hl
	adc a,e
	add hl,hl
	adc a,e
	add hl,hl
	adc a,e
	ld e,a
	
	; Divide by 3
	xor	a
	ld d,3
	
	.rept 24
	
	add	hl,hl
	rl e
	rla
	jr c,$+5
	cp d
	jr c,$+4

	sub	d
	inc	l

	.loop
	
	or a
	jr z,+
	inc hl
+:
	
	pop de
	pop af
	ret

; ==========================================================================
; DivideBy8
; --------------------------------------------------------------------------
; Divides HL by eight.
; --------------------------------------------------------------------------
; Inputs:    HL: Value to divide by eight.
; Outputs:   HL: Value divided by eight.
; Destroyed: F.
; ==========================================================================
DivideBy8:
	.rept 3
	sra h \ rr l
	.loop
	ret

; ==========================================================================
; MultiplyBy8
; --------------------------------------------------------------------------
; Multiplies HL by eight.
; --------------------------------------------------------------------------
; Inputs:    HL: Value to multiply by eight.
; Outputs:   HL: Value multiplied by eight.
; Destroyed: F.
; ==========================================================================
MultiplyBy8:
	.rept 3
	add hl,hl
	.loop
	ret

; ==========================================================================
; SortDEHL
; --------------------------------------------------------------------------
; Sort DE and HL so that DE<=HL.
; --------------------------------------------------------------------------
; Inputs:     HL: One value to sort.
;             DE: The other value to sort.
; Outputs:    DE <= HL.
; Destroyed:  F.
; ==========================================================================
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

; ==========================================================================
; SignedCPHLBC
; --------------------------------------------------------------------------
; Compare signed HL and BC.
; --------------------------------------------------------------------------
; Inputs:     HL: One value to compare.
;             BC: The other value to compare.
; Outputs:    F: Z if HL=BC, C if HL<BC.
; Destroyed:  F.
; ==========================================================================
SignedCPHLBC:
	or a
	sbc hl,bc
	add hl,bc
	jr SignedCPHLDE.F

; ==========================================================================
; SignedCPHLDE
; --------------------------------------------------------------------------
; Compare signed HL and DE.
; --------------------------------------------------------------------------
; Inputs:     HL: One value to compare.
;             DE: The other value to compare.
; Outputs:    F: Z if HL=DE, C if HL<DE.
; Destroyed:  F.
; ==========================================================================
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

; ==========================================================================
; TransformPoints
; --------------------------------------------------------------------------
; Converts visited point coordinates to physical screen coordinates.
; --------------------------------------------------------------------------
; Inputs:     VisitedPoints
;             B: Number of points to transform.
; Outputs:    TransformedPoints
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
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

; ==========================================================================
; TransformPointAroundOrigin
; --------------------------------------------------------------------------
; Converts logical coordinates relative to the origin to the physical
; screen coordinates.
; --------------------------------------------------------------------------
; Inputs:     HL: Logical X coordinate relative to the origin.
;             DE: Logical Y coordinate relative to the origin.
; Outputs:    HL: Transformed physical X coordinate.
;             DE: Transformed physical Y coordinate.
; Destroyed:  HL, DE, F.
; ==========================================================================
TransformPointAroundOrigin:
	push bc
	ld bc,(OriginX)
	add hl,bc
	ex de,hl
	ld bc,(OriginY)
	add hl,bc
	ex de,hl
	pop bc
	; Fall-through to TransformPoint

; ==========================================================================
; TransformPoint
; --------------------------------------------------------------------------
; Converts logical "visited" coordinates to the physical screen coordinates.
; --------------------------------------------------------------------------
; Inputs:     HL: Logical visited X coordinate.
;             DE: Logical visited Y coordinate.
; Outputs:    HL: Transformed physical X coordinate.
;             DE: Transformed physical Y coordinate.
; Destroyed:  HL, DE, F.
; ==========================================================================
TransformPoint:
	push bc
	ex de,hl
	call DivideCoordinate
	ld c,l
	ld b,h
	ld hl,191
	or a
	sbc hl,bc
	ld bc,(OffsetY)
	ld b,0
	or a
	sbc hl,bc
	ex de,hl
	call DivideCoordinate
	ld bc,(OffsetX)
	ld b,0
	add hl,bc
	pop bc
	ret

; ==========================================================================
; Plot
; --------------------------------------------------------------------------
; Plots a shape on the screen.
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Shape to plot.
;             VisitedPoints: Cooridnates of the points to plot.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
Plot:
	
	; Default assumption is that we're using the foreground.
	ld hl,(ForegroundMode)
	ld (PlotMode),hl
	
	; Plot mode 5 is a no-op.
	ld a,l
	cp 5
	ret z
	
	; What's the plot shape?
	ld a,(PlotShape)
	and %11
	ret z ; Invisible!
	
	dec a
	jr z,Plot.SelectedColour ; PLOT %..01

Plot.NotForegroundColour:
	dec a
	jr nz,Plot.BackgroundColour

Plot.Inverted:  ; PLOT %..10
	ld hl,$FF04
	ld (PlotMode),hl
	jr Plot.SelectedColour

Plot.BackgroundColour:  ; PLOT %..11
	ld hl,(BackgroundMode)
	ld (PlotMode),hl
	
Plot.SelectedColour:

	; Ensure the graphics mode driver is set up to plot.
	ld a,l
	call Driver.BeginPlot

	; Check if the plot shape is out of bounds.
	; Note that we must do this down here, not earlier, so the Plot handler
	; can be used to initialse colours and plotting modes for VDU 5
	; by forcing an out-of-bounds plot shape.
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
.dw PlotParallelogram ; 112..119: Parallelogram plot and fill.
.dw Stub              ; 120..127: Horizontal line fill to non-foreground right.
.dw Stub              ; 128..135: Flood fill to non-background.
.dw Stub              ; 136..143: Flood fill to foreground.
.dw PlotDrawCircle    ; 144..151: Circle outline.
.dw PlotFillCircle    ; 152..159: Circle fill.
.dw PlotDrawArc       ; 160..167: Draw circular arc.
.dw PlotFillSegment   ; 168..175: Draw solid segment.
.dw PlotFillSector    ; 176..183: Draw solid sector.
.dw Stub              ; 184..191: Block transfer operations.
.dw PlotDrawEllipse   ; 192..199: Ellipse outline.
.dw PlotFillEllipse   ; 200..207: Ellipse fill.

; ==========================================================================
; PlotLine
; --------------------------------------------------------------------------
; Plots a line.
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Line type to plot.
;             VisitedPoint0: End coordinate.
;             VisitedPoint1: Start coordinate.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
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

	; Ensure that we draw it from top to bottom, so swap (D,E) and (H,L) if necessary.
	ld a,e
	cp l
	jr c,+
	ex de,hl
+:

	; Are we going from left to right, or right to left?
	ld l,c
	ld a,d
	cp h
	ld h,b
	jr c,PlotLine.Shallow.LeftToRight

PlotLine.Shallow.RightToLeft:
	
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
	
	dec d ; Always moving left
	
	add a,l
	jr nc,+
	sub h
	
	inc e ; Always moving down
	
+:	
	djnz -
	
	ret
	
PlotLine.Shallow.LeftToRight:

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
	
	inc e ; Always moving down
	
+:	
	djnz -

	ret

; ==========================================================================
; PlotLinePlusPixel
; --------------------------------------------------------------------------
; Plots a line, then draws the end pixel again.
; --------------------------------------------------------------------------
; This is useful for inverse lines, to avoid the connection between line
; segments from inverting themselves.
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Line type to plot.
;             VisitedPoint0: End coordinate.
;             VisitedPoint1: Start coordinate.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
PlotLinePlusPixel:
	call PlotLine
	; Fall-through to PlotPixel

; ==========================================================================
; PlotPixel
; --------------------------------------------------------------------------
; Plots a single pixel according to the current plot mode.
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Pixel type to plot.
;             VisitedPoint0: Coordinats of the pixel to plot.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
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

	; Fall-through to SetPixel:
	
; ==========================================================================
; SetPixel
; --------------------------------------------------------------------------
; Sets a pixel according to the current plot mode.
; --------------------------------------------------------------------------
; Inputs:     D: X coordinate.
;             E: Y coordinate.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
SetPixel:
	; Generate the masking values.
	ld a,d
	and %00000111
	ld h,%10000000
	jr z,+
	ld b,a
-:	srl h
	djnz -
+:	ld a,h
	cpl
	ld l,a

	jp Driver.SetAlignedHorizontalLineSegment

; ==========================================================================
; PlotTransformedHorizontalSpan
; --------------------------------------------------------------------------
; Plots a horizontal span
; --------------------------------------------------------------------------
; Inputs:     PlotShape: The pixel type to plot.
;             D: X coordinate of the leftmost pixel.
;             E: Y coordinate of the span.
;             H: X coordinate of the rightmost pixel.
; Destroyed:  AF, BC, DE, HL.
; ==========================================================================
PlotTransformedHorizontalSpan:
	ld a,h
	sub d
	jr nc,+
	neg
	ld d,h
+:	inc a
	ld l,a
	
	; At this point, x = (d <= h); y = e; l = number of pixels.
	
	ld a,(Driver.SetAlignedHorizontalLineSegment)
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
	
	call Driver.SetAlignedHorizontalLineSegment
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
	call Driver.SetAlignedHorizontalLineSegment
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
	
	call Driver.SetAlignedHorizontalLineSegment
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

; ==========================================================================
; PutMap
; --------------------------------------------------------------------------
; Draws a character at the graphics cursor.
; --------------------------------------------------------------------------
; Inputs:     A: The character to draw.
; Destroyed:  AF.
; ==========================================================================
PutMap:
	push ix
	push hl
	push de
	push bc
	
	cp 127
	jr z,PutMap.Delete
	
	push af
	push af
	
	ld a,1
	ld (PlotShape),a
	
	ld hl,(ForegroundMode)
	ld (PlotMode),hl
	jr PutMap.BeginPlot

PutMap.Delete:

	push af
	push af

	ld a,3
	ld (PlotShape),a

	ld hl,(BackgroundMode)
	ld (PlotMode),hl

PutMap.BeginPlot:
	ld a,l
	call Driver.BeginPlot
	
	ld b,1
	call TransformPoints
	
	pop af
	
	call VDU.GetCharacterData
	
	push hl
	pop ix
	
	; Is the sprite completely outside the viewport?
	
	; X axis check.
	ld hl,(MaxX)
	ld h,0
	ld de,(TransformedPoint0X)
	call SignedCPHLDE
	jp c,PutMap.ReturnNoPrint
	
	ld hl,8
	add hl,de
	ld de,(MinX)
	ld d,0
	call SignedCPHLDE
	jp c,PutMap.ReturnNoPrint
	
	; Y axis check.
	ld hl,(MaxY)
	ld h,0
	ld de,(TransformedPoint0Y)
	call SignedCPHLDE
	jp c,PutMap.ReturnNoPrint
	
	ld hl,8
	add hl,de
	ld de,(MinY)
	ld d,0
	call SignedCPHLDE
	jp c,PutMap.ReturnNoPrint
	
	
	; We'll need to build a mask value.
	ld c,%11111111
	
	; Do we need to mask off the left or right edges where they run into the viewport?
	
	ld hl,(TransformedPoint0X)
	ld de,(MinX)
	ld d,0
	call SignedCPHLDE
	jr nc,PutMap.NoClipLeft
	
	; We need to clip the LEFT edge.
	ld a,e
	sub l
	ld b,a
-:	srl c
	djnz -
	
PutMap.NoClipLeft:

	ld de,7
	add hl,de
	ld de,(MaxX)
	ld d,0
	call SignedCPHLDE
	jr c,PutMap.NoClipRight
	jr z,PutMap.NoClipRight
	
	; We need to clip the RIGHT edge.	
	ld a,l
	inc a
	sub e
	ld b,a
	xor a
	cpl
-:	add a,a
	djnz -
	and c
	ld c,a

PutMap.NoClipRight:

	; To clip the sprite vertically, we'll change its height.
	ld b,8

	; Do we need to mask off the top or bottom edges where they run into the viewport?

	ld hl,(TransformedPoint0Y)
	ld de,(MinY)
	ld d,0
	call SignedCPHLDE
	jr nc,PutMap.NoClipTop
	
	; We need to clip the TOP edge.
	ld a,e
	ld (TransformedPoint0Y),a
	push de
	sub l
	pop hl
	
	ld e,a
	ld d,0
	add ix,de

	neg	
	add a,b
	ld b,a

PutMap.NoClipTop:

	ld de,7
	add hl,de
	ld de,(MaxY)
	ld d,0
	call SignedCPHLDE
	jr c,PutMap.NoClipBottom
	
	; We need to clip the BOTTOM edge.	
	ld a,e
	sub l
	add a,b
	ld b,a

PutMap.NoClipBottom:
	
	ld a,b
	or a

	ld a,(TransformedPoint0X)
	ld d,a
	ld a,(TransformedPoint0Y)
	ld e,a
	call nz,PlotTransformedSprite

PutMap.ReturnPrint:
	pop af
	scf
-:	pop bc
	pop de
	pop hl
	pop ix
	ret

PutMap.ReturnNoPrint:
	pop af
	or a
	jr -

GetCursorMinXHL:
	push bc
	ld hl,(MinX)
	ld h,0
	ld bc,(OffsetX)
	ld b,0
	or a
	sbc hl,bc
	pop bc
	ret

GetCursorMaxXHL:
	push bc
	ld hl,(MaxX)
	ld h,0
	inc hl
	ld bc,(OffsetX)
	ld b,0
	or a
	sbc hl,bc
	pop bc
	ret
	
GetCursorMinYHL:
	push bc
	ld hl,(MaxY)
	ld h,0
	call Sub191HL
	ld bc,(OffsetY)
	ld b,0
	scf
	sbc hl,bc
	pop bc
	ret

GetCursorMaxYHL:
	push bc
	ld hl,(MinY)
	ld h,0
	call Sub191HL
	ld bc,(OffsetY)
	ld b,0
	or a
	sbc hl,bc
	pop bc
	ret

Sub191HL:
	push de
	ex de,hl
	ld hl,191
	or a
	sbc hl,de
	pop de
	ret

GetCursorMinXBC:
	push hl
	call GetCursorMinXHL
	ld b,h
	ld c,l
	pop hl
	ret

GetCursorMaxXBC:
	push hl
	call GetCursorMaxXHL
	ld b,h
	ld c,l
	pop hl
	ret
	
GetCursorMinYBC:
	push hl
	call GetCursorMinYHL
	ld b,h
	ld c,l
	pop hl
	ret

GetCursorMaxYBC:
	push hl
	call GetCursorMaxYHL
	ld b,h
	ld c,l
	pop hl
	ret

; ==========================================================================
; PutChar
; --------------------------------------------------------------------------
; Draws a character at the graphics cursor and advances right.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; ==========================================================================
PutChar:
	call PutMap
	ret nc
	; Fall-through to CursorRight.
	
; ==========================================================================
; CursorRight
; --------------------------------------------------------------------------
; Move the cursor right one character.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; ==========================================================================
CursorRight:
	push hl
	push de
	push bc
	
	ld hl,(VisitedPoint0X)
	call DivideCoordinate
	
	ld bc,8
	add hl,bc
	
	call GetCursorMaxXBC
	call SignedCPHLBC
	
	jr c,CursorRightNoWrap
	
	; Cursor home.
	call GetCursorMinXHL
	call MultiplyCoordinate
	jr CursorDown.FromCursorRight
	
CursorRightNoWrap:

	call MultiplyCoordinate

	ld de,(VisitedPoint0Y)
	call VisitPointAbsolute
	
	pop bc
	pop de
	pop hl
	ret

; ==========================================================================
; CursorDown
; --------------------------------------------------------------------------
; Move the cursor down one character.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; ==========================================================================
CursorDown:
	push hl
	push de
	push bc
	
	ld hl,(VisitedPoint0X)

CursorDown.FromCursorRight:

	ex de,hl
	
	ld hl,(VisitedPoint0Y)
	
	call DivideCoordinate
	
	ld bc,-8
	add hl,bc
	
	call GetCursorMinYBC
	call SignedCPHLBC
	
	jr z,+
	jr nc,CursorDownNoWrap
+:
	call GetCursorMaxYHL

CursorDownNoWrap:

	call MultiplyCoordinate
	
	ex de,hl
	
	call VisitPointAbsolute
	
	pop bc
	pop de
	pop hl
	ret

; ==========================================================================
; CursorLeft
; --------------------------------------------------------------------------
; Move the cursor left one character.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; ==========================================================================
CursorLeft:
	push hl
	push de
	push bc
	
	ld hl,(VisitedPoint0X)
	call DivideCoordinate
	ld bc,-8
	add hl,bc
	
	call GetCursorMinXBC
	call SignedCPHLBC
	
	jr nc,CursorLeftNoWrap
	
	; Cursor to right edge.
	call GetCursorMaxXHL
	ld bc,-8
	add hl,bc
	
	call MultiplyCoordinate
	
	jr CursorUp.FromCursorLeft
	
CursorLeftNoWrap:

	call MultiplyCoordinate

	ld de,(VisitedPoint0Y)
	call VisitPointAbsolute
	
	pop bc
	pop de
	pop hl
	ret
	ret

; ==========================================================================
; CursorUp
; --------------------------------------------------------------------------
; Move the cursor up one character.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; ==========================================================================
CursorUp:
	push hl
	push de
	push bc
	
	ld hl,(VisitedPoint0X)

CursorUp.FromCursorLeft:

	ex de,hl
	
	ld hl,(VisitedPoint0Y)
	call DivideCoordinate
	ld bc,8
	add hl,bc
	
	call GetCursorMaxYBC
	call SignedCPHLBC
	
	jr c,CursorUpNoWrap

	; Cursor to bottom edge.
	call GetCursorMinYHL
	ld bc,8
	add hl,bc

CursorUpNoWrap:
	
	call MultiplyCoordinate
	
	ex de,hl
	call VisitPointAbsolute
	
	pop bc
	pop de
	pop hl
	ret
	
; ==========================================================================
; HomeLeft
; --------------------------------------------------------------------------
; Move the cursor home to the left edge.
; --------------------------------------------------------------------------
; Destroyed:  AF.
; ==========================================================================
HomeLeft:
	push hl
	push de
	call GetCursorMinXHL
	call MultiplyCoordinate
	ld de,(VisitedPoint0Y)
	call VisitPointAbsolute
	pop de
	pop hl
	ret

; ==========================================================================
; PlotTransformedSprite
; --------------------------------------------------------------------------
; Draws a sprite.
; --------------------------------------------------------------------------
; Inputs:     PlotShape = pixel type to plot.
;             (D,E): top left corner.
;             B: Height of the sprite.
;             C: Mask value for each sprite row.
;             IX : Pointer to sprite data.
; Destroyed:  AF, HL, DE, BC, IX.
; ==========================================================================
PlotTransformedSprite:

PlotTransformedSprite.Loop:
	
	push bc
	
	ld a,(ix)
	and c
	ld h,a
	
	ld a,d
	and 7
	jr z,+
	
	ld b,a
	ld l,c
-:	srl h
	srl l
	jr z,++
	djnz -
+:	ld a,h
	cpl
	ld l,a
	
	push de
	push bc
	call Driver.SetAlignedHorizontalLineSegment
	pop bc
	pop de
	
++:
	
	ld a,d
	and 7
	jr z,PlotTransformedSprite.Aligned

	ld l,a
	ld a,8
	sub l
	
	ld b,a
	
	ld a,(ix)
	and c
	ld h,a
	
-:	sla h
	sla c
	jr z,++
	djnz -
	
	ld a,h
	cpl
	ld l,a
	
	push de
	push bc
	ld a,d
	add a,8
	ld d,a
	call Driver.SetAlignedHorizontalLineSegment
	pop bc
	pop de

++:

PlotTransformedSprite.Aligned:
	
	pop bc
	inc e
	inc ix
	djnz PlotTransformedSprite.Loop
	
	ret	
	
; ==========================================================================
; Clear
; --------------------------------------------------------------------------
; Clears the viewport.
; --------------------------------------------------------------------------
; Destroyed:  AF, HL, DE, BC.
; ==========================================================================
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
	ld hl,(BackgroundMode)
	ld (PlotMode),hl
	
	; Load the plot mode.
	ld a,l
	call Driver.BeginPlot
	
	jr PlotTransformedRectangle

; ==========================================================================
; PlotRectangle
; --------------------------------------------------------------------------
; Fills a rectangle
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Rectangle type to plot.
;             VisitedPoint0: One corner.
;             VisitedPoint1: The other corner
; Destroyed:  AF, HL, DE, BC.
; ==========================================================================
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

; ==========================================================================
; PlotDrawCircle
; --------------------------------------------------------------------------
; Draws a circle outline.
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Circle type to plot.
;             VisitedPoint0: Point on circumference.
;             VisitedPoint1: Centre.
; Destroyed:  AF, HL, DE, BC.
; ==========================================================================
PlotDrawCircle:
	push iy
	ld iy,PlotShape
	res fFillEllipse,(iy+ellipseFlags)
	jr PlotCircle

; ==========================================================================
; PlotFillCircle
; --------------------------------------------------------------------------
; Fills a circle.
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Circle type to plot.
;             VisitedPoint0: Point on circumference.
;             VisitedPoint1: Centre.
; Destroyed:  AF, HL, DE, BC.
; ==========================================================================
PlotFillCircle:
	push iy
	ld iy,PlotShape
	set fFillEllipse,(iy+ellipseFlags)

PlotCircle:
	res fClipArc,(iy+ellipseFlags)
	; <centre>, <radius>
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

; ==========================================================================
; PlotDrawEllipse
; --------------------------------------------------------------------------
; Draws an ellipse outline.
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Ellipse type to plot.
;             VisitedPoint0: Point on h tangent (v radius)
;             VisitedPoint1: Point on v tangent (h radius)
;             VisitedPoint2: Centre.
; Destroyed:  AF, HL, DE, BC.
; ==========================================================================
PlotDrawEllipse:
	push iy
	ld iy,PlotShape
	res fFillEllipse,(iy+ellipseFlags)
	jr PlotEllipse
	
; ==========================================================================
; PlotFillEllipse
; --------------------------------------------------------------------------
; Fills an ellipse.
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Ellipse type to plot.
;             VisitedPoint0: Point on h tangent (v radius)
;             VisitedPoint1: Point on v tangent (h radius)
;             VisitedPoint2: Centre.
; Destroyed:  AF, HL, DE, BC.
; ==========================================================================
PlotFillEllipse:
	push iy
	ld iy,PlotShape
	set fFillEllipse,(iy+ellipseFlags)

PlotEllipse:
	res fClipArc,(iy+ellipseFlags)
	
	; <centre>, <horizontal radius>, <vertical radius>
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

; ==========================================================================
; InitialiseCircleArc
; --------------------------------------------------------------------------
; Prepares to draw a circle arc, segment or sector.
; --------------------------------------------------------------------------
; Inputs:     VisitedPoint0: End of the arc
;             VisitedPoint1: Start of the arc (providing the radius)
;             VisitedPoint2: Centre
; Outputs:    DE: radius.
;             IY: Set to PlotShape.
; Destroyed:  AF, HL, DE, BC, IY.
; ==========================================================================
InitialiseCircleArcSegmentSectorRadius:

	; We have three points to work with.
	ld b,3
	call TransformPoints
	
	; Store the slope for the line pointing to the end of the arc.
	
	ld hl,(TransformedPoint0X)
	ld de,(TransformedPoint2X)
	or a
	sbc hl,de
	ld (g_ellipseClipBDX),hl
	
	ld hl,(TransformedPoint0Y)
	ld de,(TransformedPoint2Y)
	or a
	sbc hl,de
	ld (g_ellipseClipBDY),hl

GetTransformedCircleStartArcSegmentSectorRadius:

	; Now calculate the radius and slope for the start of the arc.
	
	; dx = ?
	ld hl,(TransformedPoint1X)
	ld de,(TransformedPoint2X)
	or a
	sbc hl,de
	ld (g_ellipseClipADX),hl
	ld a,h
	or l
	jr z,CircleArcRadiusZeroWidth
	ld c,l
	ld b,h
	
	; dy = ?
	ld hl,(TransformedPoint1Y)
	ld de,(TransformedPoint2Y)
	or a
	sbc hl,de
	ld (g_ellipseClipADY),hl
	ld a,h
	or l
	jr z,CircleArcRadiusZeroHeight
	
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
	
	jr CircleArcFoundRadius

CircleArcRadiusZeroWidth:
	
	; Get dy
	ld hl,(TransformedPoint1Y)
	ld de,(TransformedPoint2Y)
	or a
	sbc hl,de
	ld (g_ellipseClipADY),hl
	jr CircleArcCalculateRadiusZeroAxis

CircleArcRadiusZeroHeight:
	
	; We still have dx in bc
	ld l,c
	ld h,b
	
CircleArcCalculateRadiusZeroAxis:
	
	bit 7,h
	jr z,+
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
+:	ex de,hl

CircleArcFoundRadius:
	
	ret
	
; ==========================================================================
; InitialiseCircleArcSector
; --------------------------------------------------------------------------
; Prepares to draw a circle arc or sector.
; --------------------------------------------------------------------------
; Inputs:     VisitedPoint0: End of the arc
;             VisitedPoint1: Start of the arc (providing the radius)
;             VisitedPoint2: Centre
; Outputs:    g_ellipseCX: Centre X coordinate.
;             g_ellipseCY: Centre Y coordinate.
;             g_ellipseRX: Horizontal radius.
;             g_ellipseRY: Horizontal radius.
;             IY: Set to PlotShape.
; Destroyed:  AF, HL, DE, BC, IY.
; ==========================================================================
InitialiseCircleArcSector:
	
	; Set up the plotting flags to clip the ellipse.
	ld iy,PlotShape
	set fClipArc,(iy+ellipseFlags)
	res fClipSegment,(iy+ellipseFlags)
	
	call InitialiseCircleArcSegmentSectorRadius
	
	; Copy the centre of the ellipse into the right field.
	ld hl,(TransformedPoint2X)
	ld (g_ellipseCX),hl
	ld hl,(TransformedPoint2Y)
	ld (g_ellipseCY),hl

	; Store radius.
	ld (g_ellipseRX),de
	ld (g_ellipseRY),de
	
	; Is the angle between the start and end of the arc >180 degrees?
	
	; Assume it isn't by default.
	res fClipArcBigAngle,(iy+ellipseFlags)
	
	ld bc,(g_ellipseClipADY)
	ld de,(g_ellipseClipBDX)
	call Maths.SMulDEBC
	
	push de
	push hl
	
	ld bc,(g_ellipseClipADX)
	ld de,(g_ellipseClipBDY)
	call Maths.SMulDEBC
	
	or a
	pop bc
	sbc hl,bc
	
	ex de,hl
	pop bc
	sbc hl,bc
	
	ret m
	set fClipArcBigAngle,(iy+ellipseFlags)
	ret

; ==========================================================================
; PlotDrawArc
; --------------------------------------------------------------------------
; Draws a circular arc.
; --------------------------------------------------------------------------
; Inputs:     VisitedPoint0: End of the arc
;             VisitedPoint1: Start of the arc (providing the radius)
;             VisitedPoint2: Centre
; Destroyed:  AF, HL, DE, BC.
; ==========================================================================
PlotDrawArc:	
	push iy
	call InitialiseCircleArcSector
	res fFillEllipse,(iy+ellipseFlags)
	call DrawEllipse
	pop iy
	ret

; ==========================================================================
; PlotFillSector
; --------------------------------------------------------------------------
; Fills a circular sector.
; --------------------------------------------------------------------------
; Inputs:     VisitedPoint0: End of the sector
;             VisitedPoint1: Start of the sector (providing the radius)
;             VisitedPoint2: Centre
; Destroyed:  AF, HL, DE, BC.
; ==========================================================================
PlotFillSector:
	push iy
	call InitialiseCircleArcSector
	set fFillEllipse,(iy+ellipseFlags)
	call DrawEllipse
	pop iy
	ret

; ==========================================================================
; PlotFillSegment
; --------------------------------------------------------------------------
; Fills a circular segment.
; --------------------------------------------------------------------------
; Inputs:     VisitedPoint0: End of the sector
;             VisitedPoint1: Start of the sector (providing the radius)
;             VisitedPoint2: Centre
; Destroyed:  AF, HL, DE, BC, H'L', D'E', B'C', A'F'.
; ==========================================================================
PlotFillSegment:
	push iy
	
	; Preserve the most recent point on the stack.
	ld hl,(VisitedPoint0X)
	push hl
	ld hl,(VisitedPoint0Y)
	push hl
	
	; Set up the plotting flags to clip the ellipse.
	ld iy,PlotShape
	set fClipArc,(iy+ellipseFlags)
	set fClipSegment,(iy+ellipseFlags)
	
	; Calculate the untransformed radius of the complete circle.
	
	; Midpoint
	ld hl,(VisitedPoint2X)
	ld (TransformedPoint2X),hl
	ld hl,(VisitedPoint2Y)
	ld (TransformedPoint2Y),hl
	
	; Start point + radius
	ld hl,(VisitedPoint1X)
	ld (TransformedPoint1X),hl
	ld hl,(VisitedPoint1Y)
	ld (TransformedPoint1Y),hl
	
	call GetTransformedCircleStartArcSegmentSectorRadius
	
	; DE = untransformed starting radius, save that for later.
	ld (g_ellipseA2),de
	
	; Calculate the untransformed radius of the ending point.
	
	; End point (radius is set by start point, so can't be trusted).
	ld hl,(VisitedPoint0X)
	ld (TransformedPoint1X),hl
	ld hl,(VisitedPoint0Y)
	ld (TransformedPoint1Y),hl
	
	call GetTransformedCircleStartArcSegmentSectorRadius
	ld (g_ellipseB2),de
	
	; Are we already on the circumference?
	ld hl,(g_ellipseA2)
	or a
	sbc hl,de
	jp z,PlotFillSegmentEndOnCircumference
	
	; Is the ending radius 0?
	ld a,d
	or e
	jr nz,PlotFillSegmentEndRadiusNonZero
	
	; If it's zero, move the end to the right hand side.
	ld hl,(VisitedPoint2X)
	ld de,(g_ellipseA2)
	add hl,de
	ld (VisitedPoint0X),hl
	
	ld hl,(VisitedPoint2Y)
	ld (VisitedPoint0Y),hl
	jr PlotFillSegmentEndOnCircumference

PlotFillSegmentEndRadiusNonZero:

	; Now we need to scale the radius of the ending point.
	; To do this, multiply it by start radius / end radius.
	
	ld hl,(g_ellipseA2)
	ld de,(g_ellipseB2)
	
	exx
	
	ld hl,0
	ld d,h
	ld e,l
	ld b,h
	ld c,l
	
	; Floating point division
	ld a,15 ; /
	call Basic.BBCBASIC_FPP
	
	ld (g_ellipseError+0),hl
	ld b,c ; We'll load into DE D'E' B later, so move C to B then zero C.
	ld c,0
	ld (g_ellipseError+4),bc
	exx
	ld (g_ellipseError+2),hl
	
	; Scale the X coordinate.
	
	ld hl,(VisitedPoint0X)
	ld de,(VisitedPoint2X)
	or a
	sbc hl,de
	
	call ScaleSegmentEndingRadius
	
	ld de,(VisitedPoint2X)
	add hl,de
	ld (VisitedPoint0X),hl
	
	; Scale the Y coordinate.
	
	ld hl,(VisitedPoint0Y)
	ld de,(VisitedPoint2Y)
	or a
	sbc hl,de
	ld de,(g_ellipseError+2)
	
	call ScaleSegmentEndingRadius
	
	ld de,(VisitedPoint2Y)
	add hl,de
	ld (VisitedPoint0Y),hl

PlotFillSegmentEndOnCircumference:

	; Now initialise the circle as normal.
	call InitialiseCircleArcSegmentSectorRadius
	
	; Save the radius for later.
	push de
	
	; Store the starting coordinate of the clipping chord.
	
	ld hl,(TransformedPoint1X)
	ld (g_ellipseClipAX),hl
	
	ld hl,(TransformedPoint1Y)
	ld (g_ellipseClipAY),hl
	
	; Store the slope of the clipping chord.
	
	ld hl,(g_ellipseClipAX)
	ld de,(TransformedPoint0X)
	or a
	sbc hl,de
	ld (g_ellipseClipADX),hl
	
	ld hl,(g_ellipseClipAY)
	ld de,(TransformedPoint0Y)
	or a
	sbc hl,de
	ld (g_ellipseClipADY),hl
	
	pop de

	; Copy the centre of the ellipse into the right field.
	ld hl,(TransformedPoint2X)
	ld (g_ellipseCX),hl
	ld hl,(TransformedPoint2Y)
	ld (g_ellipseCY),hl

	; Store radius.
	ld (g_ellipseRX),de
	ld (g_ellipseRY),de
	
	; Draw the ellipse (at long last!)
	set fFillEllipse,(iy+ellipseFlags)
	call DrawEllipse
	
	; Restore the most recent points from the stack.
	pop hl
	ld hl,(VisitedPoint0Y)
	pop hl
	ld hl,(VisitedPoint0X)
	
	pop iy
	ret

ScaleSegmentEndingRadius:

	ld de,(g_ellipseError+2)
	
	ld a,h
	
	exx
	
	add a,a
	sbc a,a
	ld h,a
	ld l,a
	
	ld de,(g_ellipseError+0)
	ld bc,(g_ellipseError+4)
	
	ld a,10 ; *
	call Basic.BBCBASIC_FPP
	
	ld a,23 ; INT
	call Basic.BBCBASIC_FPP
	
	exx
	
	ret

; ==========================================================================
; PlotTriangle
; --------------------------------------------------------------------------
; Fills a triangle.
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Triangle type to plot.
;             VisitedPoint0: First coordinate.
;             VisitedPoint1: Second coordinate.
;             VisitedPoint2: Third coordinate.
; Destroyed:  AF, HL, DE, BC.
; ==========================================================================
PlotTriangle:
	ld b,3
	call TransformPoints
	jp Triangle.Fill

; ==========================================================================
; PlotParallelogram
; --------------------------------------------------------------------------
; Fills a parallelogram.
; --------------------------------------------------------------------------
; Inputs:     PlotShape: Triangle type to plot.
;             VisitedPoint0: First coordinate.
;             VisitedPoint1: Second coordinate.
;             VisitedPoint2: Third coordinate.
; Destroyed:  AF, HL, DE, BC.
; ==========================================================================
PlotParallelogram:
	call PlotTriangle
	
	; Fourth point = point0-point1+point2
	ld b,3
	call TransformPoints
	
	
	; Redraw the central line (fixes overdraw problems).
	
	ld hl,TransformedPoint0
	ld de,TransformedPoint2
	call Clip.Clip2DLine16
	jr c,+
	
	ld h,b
	ld l,c

	call SetLine

+:
	
	ld hl,(TransformedPoint0X)
	ld de,(TransformedPoint1X)
	or a
	sbc hl,de
	ld de,(TransformedPoint2X)
	add hl,de
	ld (TransformedPoint1X),hl
	
	ld hl,(TransformedPoint0Y)
	ld de,(TransformedPoint1Y)
	or a
	sbc hl,de
	ld de,(TransformedPoint2Y)
	add hl,de
	ld (TransformedPoint1Y),hl
	
	jp Triangle.Fill
	
; ==========================================================================
; ClampTransformedHLX
; --------------------------------------------------------------------------
; Clamps transformed HL X value to be within the mode's bounds defined by
; the OffsetX and the MaxWidth.
; --------------------------------------------------------------------------
; Inputs:     HL: X value to clamp.
; Outputs:    L: Clamped value between OffsetX and MaxWidth-OffsetX.
;             F: C set if value was out of range and had to be clamped.
; Destroyed:  AF.
; ==========================================================================
ClampTransformedHLX:
	push de
	ld de,(OffsetX)
	ld d,0
	or a
	sbc hl,de
	call ClampTransformedHLXWidth
	push af
	add hl,de
	pop af
	pop de
	ret

; ==========================================================================
; ClampTransformedHLXWidth
; --------------------------------------------------------------------------
; Clamps transformed HL X value to mode maximum width.
; --------------------------------------------------------------------------
; Inputs:     HL: X value to clamp.
; Outputs:    L: Clamped value between 0 and MaxWidth inclusive.
;             F: C set if value was out of range and had to be clamped.
; Destroyed:  AF.
; ==========================================================================
ClampTransformedHLXWidth:
	bit 7,h
	jr z,+
	ld l,0
	scf
	ret
+:	ld a,h
	or a
	jr z,+
	ld hl,(MaxWidth)
	scf
	ret
+:	ld a,(MaxWidth)
	cp l	
	ret nc
	ld l,a
	ret

; ==========================================================================
; ClampTransformedDEY
; --------------------------------------------------------------------------
; Clamps transformed DE Y value to be within the mode's bounds defined by
; the OffsetY and the MaxHeight.
; --------------------------------------------------------------------------
; Inputs:     DE: Y value to clamp.
; Outputs:    E: Clamped value between OffsetY and MaxHeight-OffsetY.
;             F: C set if value was out of range and had to be clamped.
; Destroyed:  AF.
; ==========================================================================
ClampTransformedDEY:
	push hl
	
	ex de,hl
	ld de,(OffsetY)
	ld d,0
	or a
	sbc hl,de
	ex de,hl
	
	call ClampTransformedDEYHeight
	push af
	add hl,de
	ex de,hl
	pop af
	pop hl
	ret

; ==========================================================================
; ClampTransformedDEYHeight
; --------------------------------------------------------------------------
; Clamps transformed DE Y value to mode maximum height.
; --------------------------------------------------------------------------
; Inputs:     DE: Y value to clamp.
; Outputs:    E: Clamped value between 0 and MaxHeight inclusive.
;             F: C set if value was out of range and had to be clamped.
; Destroyed:  AF.
; ==========================================================================
ClampTransformedDEYHeight:
	bit 7,d
	jr z,+
	ld e,0
	scf
	ret
+:	ld a,d
	or a
	jr z,+
	ld de,(MaxHeight)
	scf
	ret
+:	ld a,(MaxHeight)
	cp e
	ret nc
	ld e,a
	ret
	
.endmodule