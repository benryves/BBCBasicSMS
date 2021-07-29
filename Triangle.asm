.module Triangle

; We'll store the triangle-filling variables in the same block as the ellipses.
EllipseVars = g_ellipseA2
EndOfEllipseVars = g_finishEllipseX + 2

MainVariables = Variables
Variables = EllipseVars

EdgeY = allocVar(2)
EdgeCounter = allocVar(2)

LongEdgeX = allocVar(2)
LongEdgeDX = allocVar(2)
LongEdgeError = allocVar(2)
LongEdgeErrorDX = allocVar(2)
LongEdgeErrorDY = allocVar(2)

ShortEdgeX = allocVar(2)
ShortEdgeDX = allocVar(2)
ShortEdgeError = allocVar(2)
ShortEdgeErrorDX = allocVar(2)
ShortEdgeErrorDY = allocVar(2)

.if Variables > EndOfEllipseVars
	.fail "Too many triangle-filling variables!"
.endif

; Restore back to the list of free variables.
Variables = MainVariables

Fill:

	; Sort so that point 0 is above point 1, which is above point 2.
	
	push ix
	push iy

ResortPoints:

	ld b,2
	ld ix,TransformedPoint0
	ld iy,TransformedPoint1
	
SortPoints:
	ld e,(ix+2) \ ld d,(ix+3)
	ld l,(iy+2) \ ld h,(iy+3)
	or a
	sbc hl,de
	add hl,de
	jr z,PointsSorted
	jp m,+
	
	jp po,PointsSorted
	jr SwapPoints
	
+:	jp pe,PointsSorted

SwapPoints:
	; Swap Y.
	ld (ix+2),l \ ld (ix+3),h
	ld (iy+2),e \ ld (iy+3),d

	; Swap X.
	ld e,(ix+0) \ ld d,(ix+1)
	ld l,(iy+0) \ ld h,(iy+1)
	ld (ix+0),l \ ld (ix+1),h
	ld (iy+0),e \ ld (iy+1),d
	
	; As we needed to sort the points at least once, try re-sorting from scratch.
	jr ResortPoints
	
PointsSorted:

	ld de,4
	add ix,de
	add iy,de
	djnz SortPoints
	
	pop iy
	pop ix
	
	; We now have a "long" edge (point 0 to point 2) and two "short" edges (point 0 to 1, point 1 to 2).
	
	; We'll have the same Y variable regardless of which edge we're filling.
	ld hl,(TransformedPoint0Y)
	ld (EdgeY),hl
	
	
	; Long and short edges start from the same point.
	ld de,(TransformedPoint0X)
	ld (LongEdgeX),de
	ld (ShortEdgeX),de
	
	; Calculate the long edge error DX and actual DX.
	ld hl,(TransformedPoint2X)
	or a
	sbc hl,de
	ld de,+1
	jp p,+
	dec de
	dec de
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
+:	ld (LongEdgeErrorDX),hl
	ld (LongEdgeDX),de
	
	; Calculate the long edge error DY.
	ld de,(TransformedPoint0Y)
	ld hl,(TransformedPoint2Y)
	or a
	sbc hl,de
	ld (LongEdgeErrorDY),hl
	
	; Initial error = -|DY|/2
	srl h \ rr l
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
	ld (LongEdgeError),hl
	
	; Calculate the short edge
	ld de,(TransformedPoint0X)
	
	ld hl,(TransformedPoint1X)
	or a
	sbc hl,de
	ld de,+1
	jp p,+
	dec de
	dec de
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
+:	ld (ShortEdgeErrorDX),hl
	ld (ShortEdgeDX),de
	
	; Calculate the short edge error DY.
	ld de,(TransformedPoint0Y)
	ld hl,(TransformedPoint1Y)
	or a
	sbc hl,de
	ld (ShortEdgeErrorDY),hl
	
	; As we'll be going from 0->1 then 1->2 we want to skip one row.
	; Otherwise the line corresponding to Y=1Y will be drawn twice.
	jr z,SkipTopHalf
	dec hl
	ld a,h
	or l
	jr z,SkipTopHalf
	
	;  We'll be filling in the top edge.
	ld (EdgeCounter),hl
	inc hl
	
	; Initial error = -|DY|/2
	srl h \ rr l
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
	ld (ShortEdgeError),hl
	
	call FillHalf

SkipTopHalf:

	; Now re-calculate the short edge for the bottom half of the triangle.
	
	; Calculate the short edge
	ld de,(TransformedPoint1X)
	ld (ShortEdgeX),de
	ld hl,(TransformedPoint2X)
	or a
	sbc hl,de
	ld de,+1
	jp p,+
	dec de
	dec de
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
+:	ld (ShortEdgeErrorDX),hl
	ld (ShortEdgeDX),de
	
	; Calculate the short edge error DY.
	ld de,(TransformedPoint1Y)
	ld hl,(TransformedPoint2Y)
	or a
	sbc hl,de
	ld (ShortEdgeErrorDY),hl
	
	;  We'll be filling in the bottom half.
	ld (EdgeCounter),hl
	
	; Initial error = -|DY|/2
	srl h \ rr l
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
	ld (ShortEdgeError),hl
	
	call FillHalf
	
	ret

FillHalf:
	
FillHalfLoop:
	; Clip the span.
	
	; Start with Y and a quick range check (Y<0 or Y>255, it's definitely off-screen!)
	ld a,(EdgeY+1)
	or a
	jr nz,SpanOffScreen
	
	; Is Y < MinY?
	ld a,(EdgeY+0)
	ld c,a
	ld a,(MinY)
	cp c
	jr z,+
	jr nc,SpanOffScreen
+:	
	ld a,(MaxY)
	cp c
	jr c,SpanOffScreen
	
	; Y is OK. 
	
	; Now clip/cull X.
	
	; Sort DE <= HL
	ld de,(LongEdgeX)
	ld hl,(ShortEdgeX)
	call SortDEHL
	
	; Is left edge > right viewport bound?
	bit 7,d
	jr nz,+ ; If DE<0, we can't be past the right edge.
	ld a,d
	or a
	jr nz,SpanOffScreen ; If D is non-zero, DE>=256 and must be off-screen.
	ld a,(MaxX)
	cp e
	jr c,SpanOffScreen
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
	jr nz,SpanOffScreen ; If HL<0, we must be off-screen.
	ld a,h
	or a
	jr nz,+ ; If H is non-zero, HL>=256 and can't be to the left of the left viewport edge.
	ld a,(MinX)
	cp l
	jr z,+
	jr nc,SpanOffScreen
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
	; We need to fill (DE,C)-(HL,C)
	; PlotTransformedHorizontalSpan goess from (D,E)-(H,E)
	ld d,e
	ld h,l
	ld e,c
	
	; At last, we're ready to draw the horizontal span!
	call PlotTransformedHorizontalSpan

SpanOffScreen:

	; Move down.
	ld hl,(EdgeY)
	inc hl
	ld (EdgeY),hl

	; Do we need to advance the long edge's X?
	; We always run this as the "long" edge is always safe (non-zero-height).
	ld hl,(LongEdgeError)
	ld de,(LongEdgeErrorDX)
	add hl,de
	jr nc,+
	
-:	push hl
	ld hl,(LongEdgeX)
	ld de,(LongEdgeDX)
	add hl,de
	ld (LongEdgeX),hl
	pop hl
	
	or a
	ld de,(LongEdgeErrorDY)
	sbc hl,de
	jr nc,-
	
+:	ld (LongEdgeError),hl

	; Is there more of the half to draw?
	ld hl,(EdgeCounter)
	ld a,h
	or l
	ret z
	dec hl
	ld (EdgeCounter),hl
	
	; Do we need to advance the short edge's X?
	; We only run this now as zero-height short sections can cause infinite loops here.
	ld hl,(ShortEdgeError)
	ld de,(ShortEdgeErrorDX)
	add hl,de
	jr nc,+
	
-:	push hl
	ld hl,(ShortEdgeX)
	ld de,(ShortEdgeDX)
	add hl,de
	ld (ShortEdgeX),hl
	pop hl
	
	or a
	ld de,(ShortEdgeErrorDY)
	sbc hl,de
	jr nc,-
	
+:	ld (ShortEdgeError),hl
	
	jp FillHalfLoop

.endmodule