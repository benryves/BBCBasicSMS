.module Maths

; ---------------------------------------------------------
; AbsHL -> HL = |HL|
; ---------------------------------------------------------
; Inputs:   hl = value.
; Outputs:  hl = |value|.
; Destroys: f.
; ---------------------------------------------------------
AbsHL:
	bit 7,h
	ret z
	; Fall-through to NegHL
	
; ---------------------------------------------------------
; NegHL -> HL = -HL
; ---------------------------------------------------------
; Inputs:   hl = value.
; Outputs:  hl = -value.
; Destroys: f.
; ---------------------------------------------------------
NegHL:
	push af
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
	pop af
	ret

; ---------------------------------------------------------
; AbsBC -> BC = |BC|
; ---------------------------------------------------------
; Inputs:   bc = value.
; Outputs:  bc = |value|.
; Destroys: f.
; ---------------------------------------------------------
AbsBC:
	bit 7,b
	ret z
	; Fall-through to NegBC
	
; ---------------------------------------------------------
; NegBC -> BC = -BC
; ---------------------------------------------------------
; Inputs:   bc = value.
; Outputs:  bc = -value.
; Destroys: f.
; ---------------------------------------------------------
NegBC:
	push af
	ld a,b \ cpl \ ld b,a
	ld a,c \ cpl \ ld c,a
	inc bc
	pop af
	ret

Sqrt32:		; DE = sqrt(HL:DE)

			;-------------------------------------------------------------------
			; Process the high 16-bits of square using 8-bit registers. This is
			; faster and also allows us to clear some registers for the 16-bit
			; code later on.
			;-------------------------------------------------------------------
			xor     a               ; [4] initial remainder
			ld      c, a            ; [4] initial root
			ld      b, 4            ; [7] counter
			call    _sqrt8          ; [*] process H
			ld      h, l            ; [4]
			ld      b, 4            ; [7] counter
			call    _sqrt8          ; [*] process L
			
						
			;-------------------------------------------------------------------
			; The last iteration of the 8-bit code might carry. It will return
			; immediately with the carry flag set if this happens. Here we
			; catch and correct for it.
			;-------------------------------------------------------------------
			jr      nc, _8bitok     ; [12/7]
			sub     c               ; [4] remainder > C (bit 8 "set")
			ccf                     ; [4]
			rl      h               ; [8]
			adc     a, a            ; [4]
			rl      h               ; [8]
			ld      l, a            ; [4]
			dec     hl              ; [6]
			inc     c               ; [4]
			dec     a               ; [4] dec a to allow fall-through...
			

			;-------------------------------------------------------------------
			; We are now ready to do process the low 16-bits using 16-bit
			; registers. But the registers need a bit of juggling.
			;-------------------------------------------------------------------

_8bitok:    ld      l, a            ; [4] HL = remainder
			ld      a, e            ; [4]
			ld      e, c            ; [4]
			ld      c, a            ; [4] C = final byte holding location
			ld      a, d            ; [4] A  = next byte
			ld      d, 0            ; [7] DE = root
			ld      b, 4            ; [4]
			call    _sqrt16         ; [*]
			ld      a, c            ; [4]
			ld      b, 4            ; [7]
			call    _sqrt16         ; [*]
			ret     nc              ; [11/5]
			inc     e               ; [4]
			ret                     ; [10]
			

			;-------------------------------------------------------------------
			; _sqrt8 - performs the square root whilst the variables are
			; restrained to 8 bits (first 16 bits of squared value).
			;
			; B - counter
			; C = root
			; A = remainder
			; H = input
			;-------------------------------------------------------------------
_sqrt8:     sla     c               ; [8] root *= 2
			sla     h               ; [8] shift bit into remainder
			rla                     ; [4]
			ret     c               ; [11/5] can only happen on last iteration.
			sub     c               ; [4] remainder -= root
			jr      c, _ltZero8     ; [12/7]
			jr      nz, _gtZero8    ; [12/7]
			
			;-------------------------------------------------------------------
			; remainder -= root == 0
			;-------------------------------------------------------------------
			sla     h               ; [8] rotate next bit
			jr      c, _gtZero8 + 2 ; [12/7]
			add     a, c            ; [4] restore remainder and shift zero.
			add     a, a            ; [4]
			djnz    _sqrt8          ; [13/8]
			rl      h               ; [8] rotate carry to H after final loop.
			ret                     ; [10]

			;-------------------------------------------------------------------
			; remainder -= root > 0            
			;-------------------------------------------------------------------
_gtZero8:   sla     h               ; [8]
			adc     a, a            ; [4] watch for carry on last iteration
			dec     a               ; [4]
			inc     c               ; [4]
			djnz    _sqrt8          ; [13/8]
			rl      h               ; [8] rotate carry to H after final loop.
			ret                     ; [10]

			
			;-------------------------------------------------------------------
			; remainder -= root carried: restore remainder and rotate square.
			;-------------------------------------------------------------------
_ltZero8:   add     a, c            ; [4]
			sla     h               ; [8]
			rla                     ; [4]
			djnz    _sqrt8          ; [13/8]
			ret                     ; [10]

			;-------------------------------------------------------------------
			; _sqrt16 - performs the square root whilst the variables are
			; restrained to 16 bits (last 16 bits of squared value).
			;
			; B     = counter
			; DE    = root
			; A     = input byte
			; HL    = remainder
			;-------------------------------------------------------------------
_sqrt16:    sla     e               ; [8] root *= 2
			rl      d               ; [8]
			add     a, a            ; [4] rotate next bit of square
			adc     hl, hl          ; [15]
			ret     c               ; [11/5] can only happen on final iteration
			sbc     hl, de          ; [11]
			jr      c, _ltZero16    ; [12/7]
			jr      nz, _gtZero16   ; [12/7]
			
			;-------------------------------------------------------------------
			; remainder -= root == 0
			;-------------------------------------------------------------------
			add     a, a            ; [4]
			jr      c, _gtZero16 +1 ; [12/7]
			add     hl, de          ; [11] restore remainder
			add     hl, hl          ; [11] and shift zero
			djnz    _sqrt16         ; [13/8]
			ret                     ; [10]
			
			;-------------------------------------------------------------------
			; remainder -= root > 0            
			;-------------------------------------------------------------------
_gtZero16:  add     a, a            ; [4]
			adc     hl, hl          ; [15]
			dec     hl              ; [6]
			inc     e               ; [4]
			djnz    _sqrt16         ; [13/8]
			ret                     ; [10]
			
			;-------------------------------------------------------------------
			; remainder -= root carried: restore remainder and rotate square.
			;-------------------------------------------------------------------
_ltZero16:  add     hl, de          ; [11] restore remainder
			add     a, a            ; [4]
			adc     hl, hl          ; [15]
			djnz    _sqrt16         ; [13/8]
			ret                     ; [10]

.endmodule