;-------------------------------------------------------------------------------
;
; === ellipsePutPixel ===
;
;  ellipsePutPixel a pixel plotting function.
;
;  The co-ordinates are passed in D (x) and E (y)
;
;  The routine must preserve BC, HL and IX
;
;
; === ellipseHLineFill ===
;
;  ellipseHLineFill is a horizontal line filler.
;
;  The y co-ordinate is passed in A. B and C contain x1 and x2. C will always
;  be larger than B.
;
;  The routine must preserve BC and IX.
; 
;-------------------------------------------------------------------------------

ellipseFlags        =    0
fClipArcBigAngle    =    1
fClipArc            =    2
fRejectC            =    3
fRejectB            =    4
fRejectL            =    5
fRejectH            =    6
fFillEllipse        =    7

.define g_ellipseCX TransformedPoint0X
.define g_ellipseCY TransformedPoint0Y
.define g_ellipseRX TransformedPoint1X
.define g_ellipseRY TransformedPoint1Y

.define g_ellipseX TransformedPoint2X
.define g_ellipseY TransformedPoint2Y

g_ellipseA2           = allocVar(4)
g_ellipseB2           = allocVar(4)
g_ellipseCounter      = allocVar(6)
g_ellipseXChange      = allocVar(6)
g_ellipseYChange      = allocVar(6)
g_ellipseError        = allocVar(6)
g_finishEllipseX      = allocVar(2)

g_ellipseClipADX      = allocVar(2)
g_ellipseClipADY      = allocVar(2)

g_ellipseClipBDX      = allocVar(2)
g_ellipseClipBDY      = allocVar(2)

;-------------------------------------------------------------------------------
; === DrawEllipse ===
;
; INPUTS:
;
;  MEMORY
;  * (g_wndXMin)    - Drawing window left X co-ordinate (0..95)
;  * (g_wndXMax)    - Drawing window right X co-ordinate (0..95)
;  * (g_wndYMin)    - Drawing window top Y co-ordinate (0..63)
;  * (g_wndYMax)    - Drawing window bottom Y co-ordinate (0..63)
;  * (g_ellipseCX)  - Ellipse centre X co-ordinate (16-bits signed)
;  * (g_ellipseCY)  - Ellipse centre Y co-ordinate (16-bits signed)
;  * (g_ellipseRX)  - Ellipse X radius (16-bit unsigned)
;  * (g_ellipseRY)  - Ellipse Y radius (16-bit unsigned)
;
;  FLAGS
;  * fFillEllipse   - Draws a filled ellipse
;
; LIMITS:
;  
;  Ellipses with a zero radii value are not drawn.
; 
; DESTROYED:
;
;  REGISTERS
;  * AF, BC, DE, HL, IX
;-------------------------------------------------------------------------------

DrawEllipse:
	;-------------------------------------------------------------------
	; g_ellipseX  = XRadius
	; g_ellipseB2 = YRadius * YRadius
	;-------------------------------------------------------------------
	ld      bc, (g_ellipseRX)               ; [20]
	ld      a, b
	or      c
	ret     z
	ld      (g_ellipseX), bc                ; [20]
	call    SquareBC                        ; [*] XRadius * XRadius
	ld      (g_ellipseA2 + $00), de         ; [20]
	ld      (g_ellipseA2 + $02), hl         ; [20]

	;-------------------------------------------------------------------
	; g_ellipseY = 0
	; g_ellipseYChange = 0
	;-------------------------------------------------------------------
	ld      bc, $0000                       ; [10]
	ld      (g_ellipseY), bc                ; [20]
	ld      (g_ellipseYChange + $00), bc    ; [20]
	ld      (g_ellipseYChange + $02), bc    ; [20]
	ld      (g_ellipseYChange + $04), bc    ; [20]

	;-------------------------------------------------------------------
	; g_ellipseA2 = XRadius * XRadius
	;-------------------------------------------------------------------
	ld      bc, (g_ellipseRY)               ; [20]
	ld      a, b
	or      c
	ret     z
	call    SquareBC                        ; [*] YRadius * YRadius
	ld      (g_ellipseB2 + $00), de         ; [20]
	ld      (g_ellipseB2 + $02), hl         ; [20]

	;-------------------------------------------------------------------
	; g_ellipseCounter = g_ellipseB2 * XRadius
	;-------------------------------------------------------------------
	ld      bc, (g_ellipseRX)               ; [20]
	call    UMul32x16                       ; [*] YRadius2 * XRadius
	ld      (g_ellipseCounter + $00), bc    ; [20]
	ld      (g_ellipseCounter + $02), de    ; [20]
	ld      (g_ellipseCounter + $04), hl    ; [20]

	;-------------------------------------------------------------------
	; g_ellipseXChange = g_ellipseB2 * XRadius
	;-------------------------------------------------------------------
	ld      (g_ellipseXChange + $00), bc    ; [20]
	ld      (g_ellipseXChange + $02), de    ; [20]
	ld      (g_ellipseXChange + $04), hl    ; [20]

	;-------------------------------------------------------------------
	; g_ellipseError = g_ellipseB2 * XRadius / 2
	;-------------------------------------------------------------------
	srl     h                               ; [8]
	rr      l                               ; [8]
	rr      d                               ; [8]
	rr      e                               ; [8]
	rr      b                               ; [8]
	rr      c                               ; [8]
	ld      (g_ellipseError + $04), hl      ; [20]
	ld      (g_ellipseError + $02), de      ; [20]
	ld      (g_ellipseError + $00), bc      ; [20]

	;-------------------------------------------------------------------
	; BEGIN: 1st set of points
	;
	; g_ellipseCounter -= g_ellipseA2
	;-------------------------------------------------------------------
_set1Loop:
	call    Plot4EllipsePoints            

	;-------------------------------------------------------------------
	; Increment Y co-ordinate
	;-------------------------------------------------------------------
	ld      hl, (g_ellipseY)
	inc     hl
	ld      (g_ellipseY), hl

	;-------------------------------------------------------------------
	; Increment g_ellipseYChange by g_ellipseA2
	;-------------------------------------------------------------------
	ld      de, g_ellipseYChange
	ld      hl, g_ellipseA2
	call    EllipseInc32

	;-------------------------------------------------------------------
	; Decrement g_ellipseError by g_ellipseYChange
	;-------------------------------------------------------------------
	ld      de, g_ellipseError
	ld      hl, g_ellipseYChange
	call    EllipseDec48
	jr      nc, _set1Next

	;-------------------------------------------------------------------
	; Decrement X co-ordinate
	;-------------------------------------------------------------------
	ld      hl, (g_ellipseX)
	ld      (g_finishEllipseX),hl ; Hack
	dec     hl
	ld      (g_ellipseX), hl

	;-------------------------------------------------------------------
	; Decrement g_ellipseCounter by g_ellipseB2
	;-------------------------------------------------------------------
	ld      de, g_ellipseCounter
	ld      hl, g_ellipseB2
	call    EllipseDec32
	jr      c, _set1Done

	;-------------------------------------------------------------------
	; Decrement g_ellipseXChange by g_ellipseB2
	;-------------------------------------------------------------------
	ld      de, g_ellipseXChange
	ld      hl, g_ellipseB2
	call    EllipseDec32

	;-------------------------------------------------------------------
	; Increment g_ellipseError by g_ellipseXChange
	;-------------------------------------------------------------------
	ld      de, g_ellipseError
	ld      hl, g_ellipseXChange
	call    EllipseInc48

_set1Next:
	ld      hl, g_ellipseA2                 ; [10]
	ld      de, g_ellipseCounter            ; [10]
	call    EllipseDec32                    ; [219]
	jr      nc, _set1Loop                   ; [12/7]

_set1Done:  
	;-------------------------------------------------------------------
	; g_ellipseX = 0
	; g_ellipseXChange = 0
	;-------------------------------------------------------------------
	ld      bc, $0000
	ld      (g_ellipseX), bc                ; [20]
	ld      (g_ellipseXChange + $00), bc    ; [20]
	ld      (g_ellipseXChange + $02), bc    ; [20]
	ld      (g_ellipseXChange + $04), bc    ; [20]


	;-------------------------------------------------------------------
	; g_ellipseY = g_ellipseRY
	; g_ellipseCounter = g_ellipseRY * g_ellipseA2
	;-------------------------------------------------------------------
	ld      bc, (g_ellipseRY)               ; [20]
	ld      (g_ellipseY), bc                ; [20]
	ld      de, (g_ellipseA2 + $00)         ; [20]
	ld      hl, (g_ellipseA2 + $02)         ; [20]
	call    UMul32x16                       ; [*]
	ld      (g_ellipseCounter + $00), bc    ; [20]
	ld      (g_ellipseCounter + $02), de    ; [20]
	ld      (g_ellipseCounter + $04), hl    ; [20]

	;-------------------------------------------------------------------
	; g_ellipseYChange = g_ellipseA2 * g_ellipseRY
	;-------------------------------------------------------------------
	ld      (g_ellipseYChange + $00), bc    ; [20]
	ld      (g_ellipseYChange + $02), de    ; [20]
	ld      (g_ellipseYChange + $04), hl    ; [20]

	;-------------------------------------------------------------------
	; g_ellipseError = g_ellipseA2 * g_ellipseRY / 2
	;-------------------------------------------------------------------
	srl     h                               ; [8]
	rr      l                               ; [8]
	rr      d                               ; [8]
	rr      e                               ; [8]
	rr      b                               ; [8]
	rr      c                               ; [8]
	ld      (g_ellipseError + $04), hl      ; [20]
	ld      (g_ellipseError + $02), de      ; [20]
	ld      (g_ellipseError + $00), bc      ; [20]


	;-------------------------------------------------------------------
	; BEGIN: 2nd set of points
	;
	; g_ellipseCounter -= g_ellipseB2
	;-------------------------------------------------------------------


_set2Loop:
	bit     fFillEllipse, (iy+ellipseFlags) ; [20]
	call    z, Plot4EllipsePoints           ; [*]

	;-------------------------------------------------------------------
	; Increment X co-ordinate
	;-------------------------------------------------------------------
	ld      hl, (g_ellipseX)
	inc     hl
	ld      (g_ellipseX), hl

	ld      de,(g_finishEllipseX) ; Hack
	or      a
	sbc     hl,de
	jr      nz,_set2NotFinished
	bit     fFillEllipse, (iy+ellipseFlags)
	call    nz, Plot4EllipsePointsDecX
	jr      _set2Done

_set2NotFinished:

	;-------------------------------------------------------------------
	; Increment g_ellipseXChange by g_ellipseB2
	;-------------------------------------------------------------------
	ld      de, g_ellipseXChange
	ld      hl, g_ellipseB2
	call    EllipseInc32

	;-------------------------------------------------------------------
	; Decrement g_ellipseError by g_ellipseXChange
	;-------------------------------------------------------------------
	ld      de, g_ellipseError
	ld      hl, g_ellipseXChange
	call    EllipseDec48
	jr      nc, _set2Next

	bit     fFillEllipse, (iy+ellipseFlags) ; [20]
	call    nz, Plot4EllipsePointsDecX
	
	;-------------------------------------------------------------------
	; Decrement Y co-ordinate
	;-------------------------------------------------------------------
	ld      hl, (g_ellipseY)
	dec     hl
	ld      (g_ellipseY), hl

	;-------------------------------------------------------------------
	; Decrement g_ellipseCounter by g_ellipseA2
	;-------------------------------------------------------------------
	ld      de, g_ellipseCounter
	ld      hl, g_ellipseA2
	call    EllipseDec32
	jr      c, _set2Done

	;-------------------------------------------------------------------
	; Decrement g_ellipseYChange by g_ellipseA2
	;-------------------------------------------------------------------
	ld      de, g_ellipseYChange
	ld      hl, g_ellipseA2
	call    EllipseDec32

	;-------------------------------------------------------------------
	; Increment g_ellipseError by g_ellipseYChange
	;-------------------------------------------------------------------
	ld      de, g_ellipseError
	ld      hl, g_ellipseYChange
	call    EllipseInc48



_set2Next:
	ld      hl, g_ellipseB2                 ; [10]
	ld      de, g_ellipseCounter            ; [10]
	call    EllipseDec32                    ; [219]
	jr      nc, _set2Loop                   ; [12/7]            

_set2Done:
	ret

Plot4EllipsePointsDecX:

	ld      hl, (g_ellipseX)
	push    hl
	dec     hl
	ld      (g_ellipseX), hl
	call    Plot4EllipsePoints
	pop     hl
	ld      (g_ellipseX), hl
	ret

;-------------------------------------------------------------------------------
; INPUTS:
;
;  MEMORY
;  * (g_wndXMin)    - Drawing window left X co-ordinate (0..95)
;  * (g_wndXMax)    - Drawing window right X co-ordinate (0..95)
;  * (g_wndYMin)    - Drawing window top Y co-ordinate (0..63)
;  * (g_wndYMax)    - Drawing window bottom Y co-ordinate (0..63)
;  * (g_ellipseCX)  - Centre X co-ordinate (16-bits signed)
;  * (g_ellipseCY)  - Centre Y co-ordinate (16-bits signed)
;  * (g_ellipseX)   - X displacement value (16-bits unsigned)
;  * (g_ellipseY)   - Y displacement value (16-bits unsigned)
;
;
; DESTROYED:
;
;  REGISTERS
;  * AF, BC, DE, HL, IX
;-------------------------------------------------------------------------------

Plot4EllipsePoints:
	;-------------------------------------------------------------------
	; Check if CY + Y is visible
	;-------------------------------------------------------------------
	ld      hl, (g_ellipseCY)           ; [20]
	ld      de, (g_ellipseY)            ; [20]
	ld      bc, (g_wndYMin)             ; [20]
	ld      ix, $ffff                   ; [14]
	xor     a                           ; [4]

	res fRejectB,(iy+ellipseFlags)
	res fRejectC,(iy+ellipseFlags)

	bit     7, d                        ; [8]
	jp      z, _chkYMax                 ; [10]
	add     hl, de                      ; [11] always positive
	jp      _chkYMaxx                   ; [10]

_chkYMax:
	adc     hl, de                      ; [15] HL = Cy + Y
	jp      pe, _chkYMin                ; [10] if Cy + Y > 32767
	ret     m                           ; [11/5] if Cy + Y < 0
_chkYMaxx:
	or      h                           ; [4]
	jr      nz, _chkYMin                ; [12/7] if Cy + Y > 255
	ld      a, b                        ; [4]
	cp      l                           ; [4] g_wndYMax - (Cy + Y)
	jr      c, _chkYMin                 ; [12/7] if (Cy + Y) > g_wndYMax
	ld      a, l                        ; [4]
	cp      c                           ; [4] (Cy + Y) - g_wndYMin
	ret     c                           ; [11/5] if (Cy + Y) < g_wndYMin
	ld      ixh, a                      ; [8] store Cy + Y

	;-------------------------------------------------------------------
	; Check if CY - Y is visible
	;-------------------------------------------------------------------
_chkYMin:
	bit     7, d                        ; [8]
	jr      nz, _chkYBoth               ; [12/7]
	xor     a                           ; [4]
	sbc     hl, de                      ; [15]
	or      a                           ; [4]
	sbc     hl, de                      ; [15]
	jp      pe, _chkYBoth               ; [10] if Cy - Y < -32768
	jp      m, _chkYBoth                ; [10] if Cy - Y < 0
	or      h                           ; [4]
	ret     nz                          ; [11/5] if Cy - Y > 255
	ld      a, b                        ; [4]
	cp      l                           ; [4] g_wndYMax - (Cy - Y)
	ret     c                           ; [11/5] if (Cy - Y) > g_wndYMax
	ld      a, l                        ; [4]
	cp      c                           ; [4] (Cy - Y) - g_wndYMin
	jr      c, _chkYBoth                ; [12/7] if (Cy - Y) < g_wndYMin
	ld      ixl, a                      ; [8]

;-------------------------------------------------------------------
; Return if both IXL and IXH still have the sign bit set.
;-------------------------------------------------------------------
_chkYBoth:
	ld      a, ixl                      ; [8]
	;and     ixh                         ; [8]
	;ret     m                           ; [8]

	inc a
	jr nz,+
	set fRejectC,(iy+ellipseFlags)
+:
	ld a,ixh
	inc a
	jr nz,+
	set fRejectB,(iy+ellipseFlags)
+:

	ld      hl, (g_ellipseCX)                   ; [20]
	ld      de, (g_ellipseX)                    ; [20]
	ld      bc, (g_wndXMin)                     ; [10] c = xmin, b = xmax
	bit     fFillEllipse, (iy + ellipseFlags)   ; [23]
	jr      z, _chkXMinPx                       ; [12/7]

	xor     a                           ; [4]
	bit     7, d                        ; [8]
	jp      z, _chkXMin2                ; [10]
	add     hl, de                      ; [11]
	jp      _chkXMin3                   ; [10]
_chkXMin2:
	adc     hl, de                      ; [15] Cx + X
	jp      pe, _forceXMax              ; [10] if Cx + X > 32767
	ret     m                           ; [11/5] if Cx + X < 0
_chkXMin3:
	or      h                           ; [4]
	jr      nz, _forceXMax              ; [12/7] if Cx + X > 255
	ld      a, l                        ; [4]
	cp      c                           ; [4] (Cx + X) - g_wndXMin
	ret     c                           ; [11/5] if (Cx + X) < g_wndXMin
	ld      a, b                        ; [4]
	cp      l                           ; [4]
	jr      c, _forceXMax               ; [12/7] if (Cx + X) > g_wndXMax
	ld      a, l                        ; [4]
	jp      _chkXMin                    ; [10] skip next instruction
_forceXMax:
	ld      a, b                        ; [4]

_chkXMin:
	bit     7, d                        ; [8]
	jr      nz, _forceXMin              ; [12/7]
	or      a                           ; [4] A = Cx + X
	sbc     hl, de                      ; [15] Cx + X - X
	or      a                           ; [4]
	sbc     hl, de                      ; [15] Cx - X
	jp      pe, _forceXMin              ; [10] if (Cx - X) < -32768
	jp      m, _forceXMin               ; [10] if (Cx - X) < 0
	rlc     h                           ; [8]
	ret     nz                          ; [11/5] if (Cx - X) > 255
	ld      h, a                        ; [4]
	ld      a, b                        ; [4]
	cp      l                           ; [4]
	ret     c                           ; [11/5] if (Cx - X) > g_wndXMax
	ld      a, l                        ; [4]
	cp      c                           ; [4] (Cx - X) - g_wndXMin
	jr      nc, _plotHLine              ; [12/7]
	ld      a, h                        ; [4]
_forceXMin:
	ld      h, a                        ; [4]
	ld      l, c                        ; [8]

_plotHLine:
	; L = x1
	; H = x2
	; IXL = y1
	; IXH = y2

	ld      b, l
	ld      c, h
_fillYMin:
	ld      a, ixh
	;or      a
	;jp      m, _fillYMax
	bit fRejectB,(iy+ellipseFlags)
	jr nz,_fillYMax
	call    _hFill
_fillYMax:
	ld      a, ixl
	;or      a
	;ret     m
	bit fRejectC,(iy+ellipseFlags)
	ret nz
	cp      ixh
	ret     z
_hFill:
	push bc
	push ix
	ld d,b
	ld h,c
	ld e,a
	bit fClipArc,(iy+ellipseFlags)
	call nz,ClipArcSpan
	call z,PlotTransformedHorizontalSpan
	pop ix
	pop bc
	ret

;-------------------------------------------------------------------
; The ellipse is being drawn in pixel plotting mode. Instead of
; clipping the x co-ordinates, we reject them if they lie off
; screen.
;-------------------------------------------------------------------
_chkXMinPx:
	xor     a                           ; [4]
	res fRejectH,(iy+ellipseFlags)
	res fRejectL,(iy+ellipseFlags)
	
	bit     7, d                        ; [8]
	jp      z, _chkXMinP2               ; [10]
	add     hl, de                      ; [11]
	jp      _chkXMinP3                  ; [10]
_chkXMinP2:
	adc     hl, de                      ; [15] Cx + X
	jp      pe, _chkXOutP               ; [10] if Cx + X > 32767
	ret     m                           ; [11/5] if Cx + X < 0
_chkXMinP3:
	or      h                           ; [4]
	jr      nz, _chkXOutP               ; [12/7] if Cx + X > 255
	ld      a, l                        ; [4]
	cp      c                           ; [4] (Cx + X) - g_wndXMin
	ret     c                           ; [11/5] if (Cx + X) < g_wndXMin
	ld      a, b                        ; [4]
	cp      l                           ; [4]
	jr      c, _chkXOutP                ; [12/7] if (Cx + X) > g_wndXMax
	ld      a, l                        ; [4]
	jp      _chkXMinPt                  ; [10] skip next instruction
_chkXOutP:
	ld      a, $ff
	set fRejectH,(iy+ellipseFlags)

_chkXMinPt:
	bit     7, d                        ; [8]
	jr      nz, _chkXOutL               ; [12/7]
	or      a                           ; [4] A = Cx + X
	sbc     hl, de                      ; [15] Cx + X - X
	or      a                           ; [4]
	sbc     hl, de                      ; [15] Cx - X
	jp      pe, _chkXOutL               ; [10] if (Cx - X) < -32768
	jp      m, _chkXOutL                ; [10] if (Cx - X) < 0
	rlc     h                           ; [8]
	ret     nz                          ; [11/5] if (Cx - X) > 255
	ld      h, a                        ; [4]
	ld      a, b                        ; [4]
	cp      l                           ; [4]
	ret     c                           ; [11/5] if (Cx - X) > g_wndXMax
	ld      a, l                        ; [4]
	cp      c                           ; [4] (Cx - X) - g_wndXMin
	jr      nc, _plotYMin               ; [12/7] if (Cx - X) < g_wndXMin
	ld      l, $ff                      ; [4]
	set fRejectL,(iy+ellipseFlags)
	jp      _plotYMin                   ; [10]
_chkXOutL:
	ld      h, a                        ; [4]
	ld      l, $ff                      ; [4]
	set fRejectL,(iy+ellipseFlags)

_plotYMin:
	ld      b, ixh                      ; [8] HL = xmax:xminn
	ld      c, ixl                      ; [8] BC = ymax:ymin

	;bit     7, c
	bit fRejectC,(iy+ellipseFlags)
	jr      nz, _plotYMax           

_plotYMinL:
	;bit     7, l
	bit fRejectL,(iy+ellipseFlags)
	jr      nz, _plotYMinR
	ld      d, l
	ld      e, c
	call    _putPixel

_plotYMinR:
	;bit     7, h
	bit fRejectH,(iy+ellipseFlags)
	jr      nz, _plotYMax

	; Patch to prevent overdraw.
	ld de,(g_ellipseX)
	ld a,d
	or e
	jr z,_plotYMax

	ld      d, h
	ld      e, c
	call    _putPixel

_plotYMax:
	; Patch to prevent overdraw.
	ld a,b
	cp c
	ret z

	;bit     7, b
	bit fRejectB,(iy+ellipseFlags)
	ret     nz

_plotYMaxL:
	;bit     7, l
	bit fRejectL,(iy+ellipseFlags)
	jr      nz, _plotYMaxR
	ld      d, l
	ld      e, b
	call    _putPixel

_plotYMaxR:
	;bit     7, h
	bit fRejectH,(iy+ellipseFlags)
	ret     nz

; Patch to prevent overdraw.
	ld de,(g_ellipseX)
	ld a,d
	or e
	ret z

	ld      d, h
	ld      e, b

_putPixel:
	push bc
	push hl
	push ix
	bit fClipArc,(iy+ellipseFlags)
	call nz,ClipArcPixel
	call z,SetPixel
	pop ix
	pop hl
	pop bc
	ret

EllipseDec48:
	ld      b, 6
	or      a
	ld      a, (de)
	sbc     a, (hl)
	ld      (de), a
	inc     de
	inc     hl
	djnz    EllipseDec48 + $03
	ret

EllipseInc48:
	ld      b, 6
	or      a
	ld      a, (de)
	adc     a, (hl)
	ld      (de), a
	inc     de
	inc     hl
	djnz    EllipseInc48 + $03
	ret


;-------------------------------------------------------------------------------
;@doc:begin ellipse routine
;
; === EllipseDec32 ===
;
;  Subtracts a 32-bit value from a 48-bit value
;
; INPUTS:
;
;  REGISTERS
;  * DE - Address of 48-bit variable to decrement
;  * HL - Address of 32-bit decrement amount
;
; DESTROYED:
;
;  REGISTERS
;  * AF, DE
;
; TIMINGS:
;  * 202 T-States
;
;@doc:end
;-------------------------------------------------------------------------------
EllipseDec32:
	ld      a, (de)         ; [7]
	sub     (hl)            ; [7]
	ld      (de), a         ; [7]
	inc     de              ; [6]
	inc     hl              ; [6]

	ld      a, (de)         ; [7]
	sbc     a, (hl)         ; [7]
	ld      (de), a         ; [7]
	inc     de              ; [6]
	inc     hl              ; [6]

	ld      a, (de)         ; [7]
	sbc     a, (hl)         ; [7]
	ld      (de), a         ; [7]
	inc     de              ; [6]
	inc     hl              ; [6]

	ld      a, (de)         ; [7]
	sbc     a, (hl)         ; [7]
	ld      (de), a         ; [7]
	inc     de              ; [6]
	dec     hl              ; [6]
	dec     hl              ; [6]
	dec     hl              ; [6]                        

	ld      a, (de)         ; [7]
	sbc     a, 0            ; [7]
	ld      (de), a         ; [7]
	inc     de              ; [6]

	ld      a, (de)         ; [7]
	sbc     a, 0            ; [7]
	ld      (de), a         ; [7]
	ret                     ; [10]
;-------------------------------------------------------------------------------
; End of EllipseDec32
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;@doc:begin ellipse routine
;
; === EllipseInc32 ===
;
;  Adds a 32-bit value to a 48-bit value
;
; INPUTS:
;
;  REGISTERS
;  * DE - Address of 48-bit variable to increment
;  * HL - Address of 32-bit increment amount
;
; DESTROYED:
;
;  REGISTERS
;  * AF, DE
;
; TIMINGS:
;  * 202 T-States
;
;@doc:end
;-------------------------------------------------------------------------------
EllipseInc32:
	ld      a, (de)         ; [7]
	add     a, (hl)         ; [7]
	ld      (de), a         ; [7]
	inc     de              ; [6]
	inc     hl              ; [6]

	ld      a, (de)         ; [7]
	adc     a, (hl)         ; [7]
	ld      (de), a         ; [7]
	inc     de              ; [6]
	inc     hl              ; [6]

	ld      a, (de)         ; [7]
	adc     a, (hl)         ; [7]
	ld      (de), a         ; [7]
	inc     de              ; [6]
	inc     hl              ; [6]

	ld      a, (de)         ; [7]
	adc     a, (hl)         ; [7]
	ld      (de), a         ; [7]
	inc     de              ; [6]
	dec     hl              ; [6]
	dec     hl              ; [6]
	dec     hl              ; [6]

	ld      a, (de)         ; [7]
	adc     a, 0            ; [7]
	ld      (de), a         ; [7]
	inc     de              ; [6]

	ld      a, (de)         ; [7]
	adc     a, 0            ; [7]
	ld      (de), a         ; [7]
	ret                     ; [10]
;-------------------------------------------------------------------------------
; End of EllipseInc32
;-------------------------------------------------------------------------------



;-------------------------------------------------------------------------------
;
; === UMul32x16 ===
;
;  Unsigned multiplication of the 32-bit value in HL:DE by the 16-bit value in
;  BC, returning a 48-bit result in HL:DE:BC.
;
; INPUTS:
;
;  REGISTERS
;  * HL:DE  - 32-bit multiplicand
;  * BC     - 16-bit multiplier
;
; OUTPUTS:
;
;  REGISTERS
;  * HL:DE:BC   - 48-bit result
;
; DESTROYED:
;
;  REGISTERS
;  * AF
;
;-------------------------------------------------------------------------------
UMul32x16:
	push    hl              ; [11]
	ld      hl, $0000       ; [10]
	call    _mulBCxE        ; [*]
	call    _mulBCxE        ; [*]
	ex      de, hl          ; [4]
	ex      (sp), hl        ; [19]
	ex      de, hl          ; [4]
	call    _mulBCxE        ; [*]
	call    _mulBCxE        ; [*]
	pop     bc              ; [10]
	ret                     ; [10]

; Subroutine that multiplies BC by E. The result is in HL:D. On
; exit, E will contain the original value of D.

; overhead = 26
; 
_mulBCxE:
	xor     a               ; [4]
	scf                     ; [4] for marker bit
_nextBit:
	rr      e               ; [8]
	jr      z, _endByte     ; [12/7]
	jr      nc, $ + $03     ; [12/7]
	add     hl, bc          ; [11]
	rr      h               ; [8]
	rr      l               ; [8]
	rra                     ; [4]
	jp      _nextBit        ; [10]
_endByte:
	ld      e, d            ; [4]
	ld      d, a            ; [4]
	ret                     ; [10]



;-------------------------------------------------------------------------------
;
;
; INPUTS:
;
;  * BC - Number to square
;
; OUTPUTS:
;
;  * HL - High word of square(BC)
;  * DE - Low word of square(BC)
;
; DESTROYED:
;
;  * AF
;-------------------------------------------------------------------------------
SquareBC:
;-------------------------------------------------------------------
; Process the low 4 bits of BC using 8-bit arithmetic.
;
; A = sqrsum
; D = sqrbit
; E = result
;-------------------------------------------------------------------
	push    bc              ; [11]
	xor     a               ; [4]
	ld      de, $0100       ; [10]

_sqrLoop8:
	add     a, a            ; [4] sqrsum * 2
	sra     c               ; [8]
	jr      nc, _nextBit8   ; [12/7]

	;-------------------------------------------------------------------
	; When the next bit of BC is set...
	;-------------------------------------------------------------------
	ld      l, a            ; [4] save sqrsum
	add     a, e            ; [4] += result
	add     a, d            ; [4] += sqrbit
	ld      e, a            ; [4] save result
	ld      a, l            ; [4] restore sqrsum
	sla     d               ; [8] sqrbit * 2
	add     a, d            ; [4] sqrsum + sqrbit * 2
	sla     d               ; [8]
	jp      nc, _sqrLoop8   ; [10]
	jp      _sqrDone8       ; [10]           


	;-------------------------------------------------------------------
	; When the next bit of BC is reset...
	;-------------------------------------------------------------------
_nextBit8:
	sla     d               ; [8]
	sla     d               ; [8]
	jp      nc, _sqrLoop8   ; [10]

	;-------------------------------------------------------------------
	; Clean up from 8-bit mode and check if there is more to do
	;-------------------------------------------------------------------
_sqrDone8:
	ld      h, d            ; [4] move sqrsuml into HL (D is zero).
	ld      l, a            ; [4]
	ld      a, c            ; [4] check if there is anything left
	or      b               ; [4]
	jp      nz, _sqrGo16    ; [10]
	ld      l, d            ; [4] zero HL and return
	pop     bc              ; [10]
	ret                     ; [10]

	;-------------------------------------------------------------------
	; Process the next 4 bits with 16-bit arithmetic
	;
	; HL = sqrsum
	; DE = result
	; BC = sqrbit
	;-------------------------------------------------------------------
_sqrGo16:
	ld      a, c            ; [4]
	push    bc              ; [11]
	ld      b, 1            ; [7]
	ld      c, d            ; [4]


_sqrLoop16:
	add     hl, hl          ; [11] sqrsuml *= 2
	rra                     ; [4] shift next bit
	jr      nc, _nextBit16  ; [12/7]

	;-------------------------------------------------------------------
	; When the next bit of BC is set...
	;-------------------------------------------------------------------
	ex      de, hl          ; [4]  sqrsum <-> result
	add     hl, de          ; [11] result += sqrsum
	add     hl, bc          ; [15] result += sqrbit
	ex      de, hl          ; [4]  result += sqrsum
	sla     b               ; [8]
	add     hl, bc          ; [11] sqrsum += sqrbit
	sla     b               ; [8]
	jp      nc, _sqrLoop16  ; [10]
	jp      _sqrDone16      ; [10]

	;-------------------------------------------------------------------
	; When the next bit of BC is reset...
	;-------------------------------------------------------------------
_nextBit16:
	sla     b               ; [8]
	sla     b               ; [8]
	jp      nc, _sqrLoop16  ; [10]


	;-------------------------------------------------------------------
	; Check if there are more bits to process before continuing.
	;-------------------------------------------------------------------
_sqrDone16:
	pop     af              ; [10] restore high byte of initial BC
	or      a               ; [4]
	jp      nz, _sqrGo32    ; [10]
	ld      h, b            ; [4] B is zero
	ld      l, b            ; [4]
	pop     bc              ; [10]
	ret                     ; [10]


;-------------------------------------------------------------------
; Process the remaining 16 bits with 32-bit arithmetic
;
; DE:HL     = sqrsum
; (SP):IX   = result
; BC        = sqrbit
;-------------------------------------------------------------------
_sqrGo32:
	push    ix              ; [15] preserve IX
	ld      ixh, d          ; [8] move resultl into IX
	ld      ixl, e          ; [8]
	ld      d, b            ; [4] zero sqrsumh
	ld      e, b            ; [4]
	push    de              ; [11] initial resulth = 0
	ld      c, $01          ; [7] initial sqrbit




_sqrLoop32:
	add     hl, hl          ; [11] sqrsum * 2
	rl      e               ; [8]
	rl      d               ; [8]
	sra     a               ; [8]
	jr      nc, _nextBit32  ; [12/7]

	;-------------------------------------------------------------------
	; When the next bit of BC is set...
	;-------------------------------------------------------------------
	ex      de, hl          ; [4]  sqrsuml <-> sqrsumh
	add     ix, de          ; [15] resultl += sqrsuml
	ex      de, hl          ; [4]  sqrsumh <-> sqrsuml
	ex      (sp), hl        ; [19] sqrsuml <-> resulth
	adc     hl, de          ; [15] resulth += sqrsumh
	add     hl, bc          ; [11] resulth += sqrbit
	ex      (sp), hl        ; [19] resulth <-> sqrsuml
	ex      de, hl          ; [4]  sqrsuml <-> sqrsumh
	sla     c               ; [8]  sqrbit *= 2
	rl      b               ; [8]
	add     hl, bc          ; [11] sqrsumh += sqrbit
	ex      de, hl          ; [4]  sqrsumh <-> sqrsuml
	sla     c               ; [8]  squarebit *= 2
	rl      b               ; [8]
	jp      nc, _sqrLoop32  ; [10]
	jp      _sqrDone32      ; [10]


	;-------------------------------------------------------------------
	; When the next bit of BC is reset...
	;-------------------------------------------------------------------
_nextBit32:
	jr      z, _sqrDone32   ; [12/7]
	sla     c               ; [8] sqrbit * 4
	rl      b               ; [8]
	sla     c               ; [8]
	rl      b               ; [8]
	jp      nc, _sqrLoop32  ; [10]

_sqrDone32:
	ld      d, ixh          ; [8] move result into HL:DE
	ld      e, ixl          ; [8]
	pop     hl              ; [10] pop high result
	pop     ix              ; [14]
	pop     bc              ; [10]
	ret                     ; [10] 
	;-------------------------------------------------------------------------------
	; End of SquareBC
	;-------------------------------------------------------------------------------


.define ClipArcDX TempTile+0 ; 2 bytes
.define ClipArcDY TempTile+2 ; 2 bytes
ClipSectorACounter = allocVar(4)
ClipSectorBCounter = allocVar(4)

; ==========================================================================
; ClipArcPixel
; --------------------------------------------------------------------------
; Clips a pixel between the two lines that define a circular arc.
; --------------------------------------------------------------------------
; Inputs:     D: X coordinate.
;             E: Y coordinate.
; Outputs:    F: Z if the pixel can be drawn, NZ if not.
; ==========================================================================
ClipArcPixel:
	push de
	
	; Get the pixel DX/DY
	
	ld l,d
	ld h,0
	ld bc,(g_ellipseCX)
	or a
	sbc hl,bc
	ld (ClipArcDX),hl
	
	ld l,e
	ld h,0
	ld bc,(g_ellipseCY)
	or a
	sbc hl,bc
	ld (ClipArcDY),hl
	
	; Do we have a big angle or a small angle?
	bit fClipArcBigAngle,(iy+ellipseFlags)
	jr nz,ClipArcBig

ClipArcSmall: ; Result = correct side of A AND correct side of B.

	; Are we on the correct side of clip line A?
	call ClipArcLineSideA
	jr nz,+
	; Are we on the correct side of clip line B?
	call ClipArcLineSideB
+:	pop de
	ret

ClipArcBig: ; Result = correct side of A OR correct side of B.

	; Are we on the correct side of clip line A?
	call ClipArcLineSideA
	jr z,+
	; Are we on the correct side of clip line B?
	call ClipArcLineSideB
+:	pop de
	ret
	
ClipArcLineSideA:

	; Calculate pixel DY * clip line DX
	
	ld bc,(ClipArcDY)
	ld de,(g_ellipseClipADX)
	call Maths.SMulDEBC
	
	push de
	push hl
	
	; Calculate pixel DX * clip DY
	
	ld bc,(ClipArcDX)
	ld de,(g_ellipseClipADY)
	call Maths.SMulDEBC
	
	; Subtract previous product.
	
	or a
	pop bc
	sbc hl,bc
	
	ex de,hl
	pop bc
	sbc hl,bc
	
	; We're on the correct side if HL>=0
	bit 7,h
	ret

ClipArcLineSideB:

	; Calculate pixel DX * clip DY
	
	ld bc,(ClipArcDX)
	ld de,(g_ellipseClipBDY)
	call Maths.SMulDEBC
	
	push de
	push hl
	
	; Calculate pixel DY * clip line DX
	
	ld bc,(ClipArcDY)
	ld de,(g_ellipseClipBDX)
	call Maths.SMulDEBC
	
	; Subtract previous product.
	
	or a
	pop bc
	sbc hl,bc
	
	ex de,hl
	pop bc
	sbc hl,bc
	
	; We're on the correct side if HL>=0
	bit 7,h
	ret

; ==========================================================================
; ClipArcSpan
; --------------------------------------------------------------------------
; Clips a span between the two lines that define a circular arc.
; --------------------------------------------------------------------------
; Inputs:     D, B: X1 coordinate.
;             H, C: X2 coordinate.
;             E, A: Y coordinate.
; Outputs:    F: Z if the span can be drawn, NZ if not.
; ==========================================================================
ClipArcSpan:
	push de

	; How many pixels are we due to draw on the line?
	ld a,h
	sub d
	inc a
	push af
	
	; Get the pixel DX/DY from the centre position.
	ld l,d
	ld h,0
	ld bc,(g_ellipseCX)
	or a
	sbc hl,bc
	ld (ClipArcDX),hl
	
	ld l,e
	ld h,0
	ld bc,(g_ellipseCY)
	or a
	sbc hl,bc
	ld (ClipArcDY),hl
	
	; Calculate the initial line side A counter.
	
	call ClipArcLineSideA
	
	ld (ClipSectorACounter+0),de
	ld (ClipSectorACounter+2),hl
	
	; Calculate the initial line side B counter.
	
	call ClipArcLineSideB
	
	ld (ClipSectorBCounter+0),de
	ld (ClipSectorBCounter+2),hl
	
	; Restore number of pixels to try and coordinates.
	pop bc
	pop de
	
	; Now we can try to clip each pixel in a loop.
	
ClipArcSpanLoop:
	push bc
	push de
	
	; Do we have a big angle or a small angle?
	bit fClipArcBigAngle,(iy+ellipseFlags)
	jr nz,ClipArcSpanBig

ClipArcSpanSmall: ; Result = correct side of A AND correct side of B.

	; Are we on the correct side of clip line A?
	ld a,(ClipSectorACounter+3)
	bit 7,a
	jr nz,ClipArcSpanClipped
	
	; Are we on the correct side of clip line B?
	ld a,(ClipSectorBCounter+3)
	bit 7,a
	jr ClipArcSpanClipped

ClipArcSpanBig: ; Result = correct side of A OR correct side of B.

	; Are we on the correct side of clip line A?
	ld a,(ClipSectorACounter+3)
	bit 7,a
	jr z,ClipArcSpanClipped
	
	; Are we on the correct side of clip line B?
	ld a,(ClipSectorBCounter+3)
	bit 7,a

ClipArcSpanClipped:
	
	pop de
	push de
	call z,SetPixel
	
	; Now increment the clipping counters.
	
	; Load and sign extend line A clipping Y delta into BCDE
	
	ld de,(g_ellipseClipADY)
	ld a,d
	add a,a
	sbc a,a
	ld b,a
	ld c,a
	
	; Add to A counter.
	
	ld hl,(ClipSectorACounter+0)
	add hl,de
	ld (ClipSectorACounter+0),hl
	
	ld hl,(ClipSectorACounter+2)
	adc hl,bc
	ld (ClipSectorACounter+2),hl
	
	; Load and sign extend line B clipping Y delta into BCDE
	
	ld de,(g_ellipseClipBDY)
	ld a,d
	add a,a
	sbc a,a
	ld b,a
	ld c,a
	
	; Subtract from B counter.
	
	or a
	ld hl,(ClipSectorBCounter+0)
	sbc hl,de
	ld (ClipSectorBCounter+0),hl
	
	ld hl,(ClipSectorBCounter+2)
	sbc hl,bc
	ld (ClipSectorBCounter+2),hl
	
	pop de
	inc d
	pop bc
	djnz ClipArcSpanLoop
	
	; We've handled clipping/drawing ourselves, so return NZ to skip the standard scanline drawing.
	xor a
	inc a
	ret