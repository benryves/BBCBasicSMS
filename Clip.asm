;------------------------------------------------------------------------------- 
;@doc:file
; 
; === Clip.asm ===
;
;   Provides methods for clipping. Written by qarnos.
;
;@doc:end
;------------------------------------------------------------------------------- 
.module Clip

;-------------------------------------------------------------------------------
;
; === Variables ===
;
;-------------------------------------------------------------------------------

g_line16X1 = TempTile+0
g_line16Y1 = TempTile+2
g_line16X2 = TempTile+4
g_line16Y2 = TempTile+6

g_line16Pt1 = g_line16X1
g_line16Pt2 = g_line16X2

;-------------------------------------------------------------------------------
;
; === Clip2DLine16 ===
;
; WARNING:
;
;  All Y co-ordinates are reversed from TI-OS _ILine.
;
; INPUTS:
;
;  REGISTERS
;  * DE - address of point 1
;  * HL - address of point 2
;
; OUTPUTS
;
;  REGISTERS
;  * B - x1
;  * C - y1
;  * D - x2
;  * E - y2
;  * F - Carry flag set if line was culled.
;
;  MEMORY
;  * (g_line16X1)   - clipped x co-ordinate of point 1 (16-bit version)
;  * (g_line16Y1)   - clipped y co-ordinate of point 1 (16-bit version)
;  * (g_line16X2)   - clipped x co-ordinate of point 2 (16-bit version)
;  * (g_line16Y2)   - clipped y co-ordinate of point 2 (16-bit version)
;
; DESTROYED:
;  * A, HL, IX
;
; === Clip2DLine16Ex ===
;
; INPUTS:
;
;  MEMORY
;  * (g_line16X1)   - x co-ordinate of point 1
;  * (g_line16Y1)   - y co-ordinate of point 1
;  * (g_line16X2)   - x co-ordinate of point 2
;  * (g_line16Y2)   - y co-ordinate of point 2
;
; OUTPUTS:
;
;  Same as Clip3DLine16.
;
; DESTROYED:
;
;  Same as Clip3DLine16.
;
;-------------------------------------------------------------------------------
Clip2DLine16:

            ;-------------------------------------------------------------------
            ; Copy 16-bit co-ordinates to temporary buffer
            ;-------------------------------------------------------------------
            push    de                      ; [11]
            ld      de, g_line16Pt2         ; [10]
            ld      bc, 4                   ; [10]
            ldir                            ; [79]
            pop     hl                      ; [10]
            ld      de, g_line16Pt1         ; [10]
            ld      c, 4                    ; [7]
            ldir                            ; [79]

Clip2DLine16Ex:
            
            ;-------------------------------------------------------------------
            ; Generate initial clipping codes for points.
            ;-------------------------------------------------------------------
            ld      de, g_line16Pt1         ; [10]
            ld      hl, g_line16Pt2         ; [10]
            call    MakeClippingCode        ; [*] make code for g_line16Pt2
            ld      c, a                    ; [4]
            ex      de, hl                  ; [4] g_line16Pt2 <-> g_line16Pt1
            call    MakeClippingCode        ; [*]
            
            ;-------------------------------------------------------------------
            ; Loop while point 1 is out
            ;-------------------------------------------------------------------
_pt1Loop:   jr      z, _pt2Loop             ; [12/7]

            ;-------------------------------------------------------------------
            ; Reject the line if both end points lie outside a common plane.
            ;-------------------------------------------------------------------
            ld      b, a                    ; [4]
            and     c                       ; [4]
            scf                             ; [4]
            ret     nz                      ; [11/5]

            ;-------------------------------------------------------------------
            ; Exit the loop if point 1 is in.
            ;-------------------------------------------------------------------
            or      b                       ; [4]
            jr      z, _pt2Loop             ; [12/7]

            
            ;-------------------------------------------------------------------
            ; Interpolate point 1 towards point 2 and store result
            ;-------------------------------------------------------------------
            push    bc                      ; [11] save g_line16Pt2 code
            call    InterpolateLine         ; [*]
            jr      nc, $ + $0004           ; [12/7] just in case...
            pop     bc                      ; [10]
            ret                             ; [10]
            ld      (g_line16X1), bc        ; [20]
            ld      (g_line16Y1), de        ; [20]
            
            ;-------------------------------------------------------------------
            ; Generate new code for point 1 and loop
            ;-------------------------------------------------------------------
            call    MakeClippingCodeEx      ; [*]
            pop     bc                      ; [10]
            ld      hl, g_line16Pt1         ; [10]
            ld      de, g_line16Pt2         ; [10]
            jp      _pt1Loop                ; [10]
            
            ;-------------------------------------------------------------------
            ; Loop while point 2 is out
            ;-------------------------------------------------------------------
_pt2Loop:   ld      a, c                    ; [4] assume clipping code for 
            or      a                       ; [4] point 1 is zero
            jr      z, _allDone             ; [12/7]
            
            ;-------------------------------------------------------------------
            ; Interpolate point 2 towards point 1 and store result
            ;-------------------------------------------------------------------
            ld      hl, g_line16Pt2         ; [10]
            ld      de, g_line16Pt1         ; [10]
            call    InterpolateLine         ; [*]
            ret     c                       ; [11/5] just in case...
            ld      (g_line16X2), bc        ; [20]
            ld      (g_line16Y2), de        ; [20]
            
            ;-------------------------------------------------------------------
            ; Generate new code for point 2 and loop
            ;-------------------------------------------------------------------
            call    MakeClippingCodeEx      ; [*]
            jp      _pt2Loop + 1            ; [10]
            
            ;-------------------------------------------------------------------
            ; Load the low bytes of the 16-bit co-ordinates into output
            ; registers (high bytes are now all zero).
            ;-------------------------------------------------------------------
_allDone:   or      a                       ; [4]  reset carry
            ld      a, (g_line16X1)         ; [13]
            ld      b, a                    ; [4]  x1
            ld      a, (g_line16Y1)         ; [13]
            ld      c, a                    ; [4]  y1
            ld      a, (g_line16X2)         ; [13]
            ld      d, a                    ; [4]  x2
            ld      a, (g_line16Y2)         ; [13]
            ld      e, a                    ; [4]  y2
            ret                             ; [10]
;-------------------------------------------------------------------------------
; End of Clip2DLine16/Ex
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
;
; === NegateHL ===
;
;-------------------------------------------------------------------------------
NegateHL:   ld      a, h    ; [4]
            cpl             ; [4]
            ld      h, a    ; [4]
            ld      a, l    ; [4]
            cpl             ; [4]
            ld      l, a    ; [4]
            inc     hl      ; [6]
            ret             ; [10]
;-------------------------------------------------------------------------------
; End of NegateHL
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
;
; === MakeClippingCode ===
;
;  See MakeClippingCodeEx for description.
;
; INPUTS:
;
;   * HL - Address of x,y coordinates
;
; OUTPUTS:
;
;   * A - Clipping code.
;   * F - Zero flag set for A.
;
;-------------------------------------------------------------------------------
MakeClippingCode:
            push    hl                  ; [11]
            push    de                  ; [11]
            push    bc                  ; [11]
            ld      c, (hl)             ; [7] load x low
            inc     hl                  ; [6]
            ld      b, (hl)             ; [7] load h high
            inc     hl                  ; [6]
            ld      e, (hl)             ; [7] load y low
            inc     hl                  ; [6]
            ld      d, (hl)             ; [7] load y high
            call    MakeClippingCodeEx  ; [*]
            pop     bc                  ; [10]
            pop     de                  ; [10]
            pop     hl                  ; [10]
            ret                         ; [10]
;-------------------------------------------------------------------------------
; End of MakeClippingCode
;-------------------------------------------------------------------------------



;-------------------------------------------------------------------------------
; === MakeClippingCodeEx ===
;
; INPUTS:
;
;  REGISTERS
;  * BC - 16-bit x-coordinate (signed)
;  * DE - 16-bit y-coordinate (signed)
;
;  MEMORY
;  * (g_wndXMin) - X co-ordinate of top left corner clipping window
;  * (g_wndXMax) - X co-ordinate of bottom right corner clipping window
;  * (g_wndYMin) - Y co-ordinate of top left corner clipping window
;  * (g_wndYMax) - Y co-ordinate of bottom right corner clipping window
;
; OUTPUTS:
;
;  REGISTERS
;  * A  - Clipping flags in bits 7-4, bits 3-0 zero
;       - Bit 7 - point lies outside g_wndXMin
;       - Bit 6 - point lies outside g_wndXMax
;       - Bit 5 - point lies outside g_wndYMin
;       - Bit 4 - point lies outside g_wndYMax
;  * F  - Zero flag set according to A
;
; DESTROYED:
;
;  REGISTERS
;  * HL
;-------------------------------------------------------------------------------
MakeClippingCodeEx:
            ;-------------------------------------------------------------------
            ; Check Y against (g_wndYMax)
            ;-------------------------------------------------------------------
            ld      h, $80          ; [7] for speed (since we trash L anyway)
            ld      a, (g_wndYMax)  ; [13]
            sub     e               ; [4]
            ld      a, $00          ; [7]
            sbc     a, d            ; [4]
            jp      po, $ + $0004   ; [10] invert sign when overflow TRUE
            xor     h               ; [4]
            and     h               ; [4] sign bit set if out
            rrca                    ; [4]
            ld      l, a            ; [4]

            ;-------------------------------------------------------------------
            ; Check Y against (g_wndYMin)
            ;-------------------------------------------------------------------
            ld      a, (g_wndYMin)  ; [13]
            scf                     ; [4]
            sbc     a, e            ; [4]
            ld      a, 0            ; [7]
            sbc     a, d            ; [4]
            jp      pe, $ + $0004   ; [10] invert sign when overflow FALSE
            xor     h               ; [4]
            and     h               ; [4] sign bit set when out
            or      l               ; [4]
            rrca                    ; [4]
            ld      l, a            ; [4]

            ;-------------------------------------------------------------------
            ; Check X against (g_wndXMax)
            ;-------------------------------------------------------------------
            ld      a, (g_wndXMax)  ; [13]
            sub     c               ; [4]
            ld      a, $00          ; [7]
            sbc     a, b            ; [4]
            jp      po, $ + $0004   ; [10] invert sign when overflow TRUE
            xor     h               ; [4]
            and     h               ; [4] sign bit set if out
            or      l               ; [4]
            rrca                    ; [4]
            ld      l, a            ; [4]

            ;-------------------------------------------------------------------
            ; Check X against (g_wndXMin)
            ;-------------------------------------------------------------------
            ld      a, (g_wndXMin)  ; [13]
            scf                     ; [4]
            sbc     a, c            ; [4]
            ld      a, 0            ; [7]
            sbc     a, b            ; [4]
            jp      pe, $ + $0004   ; [10] invert sign when overflow FALSE
            xor     h               ; [4]
            and     h               ; [4] sign bit set when out
            or      l               ; [4]
            ret                     ; [10]
;-------------------------------------------------------------------------------
; End of MakeClippingCodeEx
;-------------------------------------------------------------------------------

          


;-------------------------------------------------------------------------------
;
; === InterpolateLine ===
;
; INPUTS:
;
;  REGISTERS
;  * A  - candidate clipping planes
;  * DE - address of point 1
;  * HL - address of point 2
;
; OUTPUTS:
;
;  REGISTERS
;  * BC - interpolated x
;  * DE - interpolated y
;
; DEESTROYED:
;
;  REGISTERS
;  * AF, HL, IX
;
;-------------------------------------------------------------------------------
InterpolateLine:

            ;-------------------------------------------------------------------
            ; Rotate through the clipping flags until we find one which is set.
            ;-------------------------------------------------------------------
            ld      bc, g_wndXMin   ; [10]
            ld      ix, $0002       ; [14]
            add     a, a            ; [4] rotate g_wndXMin flag to carry
            jr      c, _intrpForX   ; [12/7]
            inc     bc              ; [6] advance BC to g_wndXMax
            add     a, a            ; [4] rotate g_wndXMan flag to carry
            jr      c, _intrpForX   ; [12/7]
            inc     bc              ; [6] advance BC to g_wndYMin
            inc     de              ; [6]
            inc     de              ; [6] advance DE to v1->y
            inc     hl              ; [6]
            inc     hl              ; [6] advance HL to v2->y
            ld      ix, $fffe       ; [14]
            add     a, a            ; [4] rotate g_wndYMin flag to carry
            jr      c, _intrpForY   ; [12/7]
            inc     bc              ; [6] advance BC to g_wndYMax
            add     a, a            ; [4] rotate g_wndYMax flag to carry
            jr      c, _intrpForY   ; [12/7]
            
            ;-------------------------------------------------------------------
            ; This should never happen, but return with carry just in case.
            ;-------------------------------------------------------------------
            scf                     ; [4] set error state
            ret                     ; [10]

    
            ;-------------------------------------------------------------------
            ; Handle interpolation through the x-planes
            ;-------------------------------------------------------------------
_intrpForX: call    _interpol8      ; [*]
            ld      b, 0            ; [7]
            ld      c, a            ; [4]
            ret                     ; [10]

            ;-------------------------------------------------------------------
            ; Handle interpolation through the y-planes
            ;-------------------------------------------------------------------
_intrpForY: call    _interpol8      ; [*]
            ld      b, d            ; [4]
            ld      c, e            ; [4]
            ld      d, 0            ; [7]
            ld      e, a            ; [4]
            ret                     ; [10]



            ;-------------------------------------------------------------------
            ; This is the actual interpolation code
            ;-------------------------------------------------------------------
_interpol8: or      a
            ld      a, (bc)         ; [7] load plane value
            push    af              ; [11] save plane value
            push    hl              ; [11] save v2
            push    de              ; [11] save v1
            
            ;-------------------------------------------------------------------
            ; Calculate interpolation factor as proper fraction:
            ;
            ;   abs(plane - v1->n) / abs(v1->n - v2->n)
            ;
            ; Result in DE/HL
            ;-------------------------------------------------------------------
            ld      c, (hl)         ; [7] load v2->n low
            inc     hl              ; [6]
            ld      b, (hl)         ; [7] load v2->n high
            ex      de, hl          ; [4] v2 <-> v1
            ld      e, (hl)         ; [7] load v1->n low
            inc     hl              ; [6]
            ld      d, (hl)         ; [7] load v1->n high
            
            ld      l, a            ; [4]
            xor     a               ; [4]
            ld      h, a            ; [4]            
            sbc     hl, de          ; [15] plane - v1->n
            jp      po, $ + $0005   ; [10] jump if overflow FALSE
            or      $80             ; [7]
            xor     h               ; [4]
            call    m, NegateHL     ; [57/10]
            
            ex      de, hl          ; [4] d1 <-> v1->n
            xor     a               ; [4]
            sbc     hl, bc          ; [15] v1->n - v2->n
            jp      po, $ + $0005   ; [10] jump if overflow FALSE
            or      $80             ; [7]
            xor     h               ; [4]
            call    m, NegateHL     ; [57/10]
            
            ld      b, ixh          ; [4]
            ld      c, ixl          ; [4]
            pop     ix              ; [14] restore v1
            ex      (sp), hl        ; [19] denominator <-> v2
            add     ix, bc          ; [15]
            add     hl, bc          ; [11]

            ;-------------------------------------------------------------------
            ; Calculate (v1 - v2)            
            ;-------------------------------------------------------------------
            ld      a, (ix + $00)   ; [19]
            sub     (hl)            ; [7]
            inc     hl              ; [6]
            push    af              ; [11]
            ld      a, (ix + $01)   ; [19]
            sbc     a, (hl)         ; [7]
            pop     hl              ; [10]
            ld      l, h            ; [4]
            ld      h, a            ; [4]
            
            ;-------------------------------------------------------------------
            ; Call the clipping function
            ;-------------------------------------------------------------------
            jp      po, $ + $0005   ; [10] jump if overflow FALSE
            xor     $80             ; [7]
            pop     bc              ; [19] restore denominator
            push    af              ; [11] save sign flag
            call    m, NegateHL     ; [57/10]
            call    UMulByFraction  ; [*]
            pop     af              ; [10] restore sign flag
            call    m, NegateHL     ; [57/10]
            

            ;-------------------------------------------------------------------
            ; Add result to origin
            ;-------------------------------------------------------------------
            ld      a, (ix + $00)   ; [19]
            sub     l               ; [4]
            ld      e, a            ; [4]
            ld      a, (ix + $01)   ; [19]
            sbc     a, h            ; [4]
            ld      d, a            ; [4]
            pop     af              ; [11] restore x/y plane value
            ret                     ; [10]
;-------------------------------------------------------------------------------
; End of InterpolateLine            
;-------------------------------------------------------------------------------

           
            
;-------------------------------------------------------------------------------
;
; === UMulByFraction ===
;
;  Multiplies HL by proper fraction DE/BC.
;
; LIMITS:
;  * BC > 0
;  * DE < BC
;
; INPUTS:
;
;  REGISTERS
;  * HL - multiplicand
;  * DE - numerator
;  * BC - denominator
;
; OUTPUTS:
;
;  REGISTERS
;  * HL - quotient of HL * DE / BC
;  * DE - remainder of HL * DE / BC
;
; DESTROYED:
;
;  REGISTERS
;  * AF
;
;-------------------------------------------------------------------------------
UMulByFraction:
            ;-------------------------------------------------------------------
            ; This initial block of code is used to return the output in more
            ; convenient registers. It may be removed, in which case the
            ; output will be returned as followed:
            ;
            ;  IX  - quotient
            ;  HL  - remainder            
            ;  DE  - numerator (unchanged)
            ;  BC  - denominator (unchanged)
            ;-------------------------------------------------------------------
            push    ix                  ; [15] preserve IX
            call    _mainRtn            ; [17] run main code.
            ex      (sp), ix            ; [23] quotient <-> preserved IX
            ex      de, hl              ; [4] remainder <-> numerator
            pop     hl                  ; [10] pop quotient
            ret                         ; [10]

            ;-------------------------------------------------------------------
            ; The actual routine starts here.
            ;-------------------------------------------------------------------
_mainRtn:   ld      ix, $0000           ; [14] initial quotient
            ld      a, h                ; [4]
            or      a                   ; [4]
            jr      z, _highZero        ; [12/7] jump if high byte is zero.
            
            ld      h, l                ; [4] push contents of L to stack so
            push    hl                  ; [11] we can pop it into A later on.
            ld      hl, $0000           ; [10] initial remainder
            
            ;-------------------------------------------------------------------
            ; Skip to first significant bit to avoid useless work. We also
            ; set the low bit of A to act as a marker bit for the main loop.
            ;-------------------------------------------------------------------
            scf                         ; [4] for marker bit
_findHiBit: adc     a, a                ; [4] searching for first significant
            jp      nc, _findHiBit      ; [10] bit to avoid useless work.
            call    _mainLoop           ; [*] process high byte.
            pop     af                  ; [10] pop low byte
            
            
            ;------------------------------------------------------------------- 
            ; Now process the low byte. 
            ; 
            ; Here we duplicate a small part of the the main loop. This is 
            ; because we need to run this code, but we want to shift a 1 
            ; into the accumulator instead of a 0. 
            ;------------------------------------------------------------------- 
            add     ix, ix              ; [15] quotient * 2 
            add     hl, hl              ; [11] remainder * 2 
            jr      c, _ov3             ; [12/7] << 
            sbc     hl, bc              ; [15] remainder -= denominator 
            jr      nc, _nc3            ; [12/7] 
            add     hl, bc              ; [11] remainder += denominator 
            jp      _rotMul2            ; [10] 
_nc3:       inc     ix                  ; [10] quotient + 1 
_rotMul2:   sl1     a                   ; [8] 
            jp      _mainLoop           ; [10] 
_ov3:       or      a                   ; [4] << 
            sbc     hl, bc              ; [15] << 
            jp      _nc3                ; [10] <<
            
            ;-------------------------------------------------------------------
            ; High byte was zero.
            ; Go straight to low byte. Do not collect $200.
            ;-------------------------------------------------------------------
_highZero:  ld      a, l                ; [4] load low byte
            or      a                   ; [4]
            ld      hl, $0000           ; [10] initial remainder
            ret     z                   ; [11/5] return if multiplicand zero
            
            ;-------------------------------------------------------------------
            ; Search for first significant bit to avoid useless work. We also
            ; set the low bit of A to act as a marker bit for the main loop.
            ;-------------------------------------------------------------------
            scf                         ; [4] for marker bit
_findLoBit: adc     a, a                ; [4] searching for first significant
            jp      nc, _findLoBit      ; [10] bit to avoid useless work.

            ;-------------------------------------------------------------------
            ; Main processing loop. We loop here for each significant bit
            ; of the multiplicand (hence, small multiplicands are fast).
            ;-------------------------------------------------------------------
_mainLoop:  jr      nc, _loopTail       ; [12/7]
            add     hl, de              ; [11] remainder += numerator
            jr      c, _ov2             ; [12/7]
            sbc     hl, bc              ; [15] remainder -= denominator
            jr      nc, _nc2            ; [12/7]
            add     hl, bc              ; [11] remainder += denominator
            jp      _loopTail           ; [10]
_nc2:       inc     ix                  ; [10]            
_loopTail:  cp      $80                 ; [7] once only the marker bit is
            ret     z                   ; [10] left we end the loop
            add     ix, ix              ; [15] quotient * 2
            add     hl, hl              ; [11] remainder * 2
            jr      c, _ov1             ; [12/7]
            sbc     hl, bc              ; [15] remainder -= denominator
            jr      nc, _nc1            ; [12/7]
            add     hl, bc              ; [11] remainder += denominator
            jp      _rotMulBit          ; [10]
_nc1:       inc     ix                  ; [10] quotient + 1
_rotMulBit: add     a, a                ; [4] shift next bit and loop
            jp      _mainLoop           ; [10]
_ov1:       or      a                   ; [4]
            sbc     hl, bc              ; [15] remainder -= denominator
            jp      _nc1                ; [10]
_ov2:       or      a                   ; [4]
            sbc     hl, bc              ; [15] remainder -= denominator
            jp      _nc2                ; [10]

;-------------------------------------------------------------------------------
; End of UMulByFraction
;-------------------------------------------------------------------------------
.endmodule