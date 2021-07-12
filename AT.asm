; =========================================================
; Module: AT
; =========================================================
; Description:
;     Routines for sending and receiving bytes using the AT
;     protocol.
; Functions:
;     SendByte: Send a byte.
;     GetByte:  Receive a byte.
; =========================================================

.module AT

Timeout = 128
Retries = 8

; .........................................................
; Equates for the various AT command bytes
; .........................................................

.module Command
    Reset    = $FF        ; Reset connected device.
    Resend   = $FE        ; Resend the last byte.
    Identify = $F2        ; Get the device ID.
    Enable   = $F4        ; Enable the device.
    Disable  = $F5        ; Disable the device.
    Echo     = $EE        ; Check for echo.
    Ack      = $FA        ; Acknowledgement.
    Post     = $AA        ; Power-on self test passed.
.endmodule

; ---------------------------------------------------------
; SendByte -> Send a byte.
; ---------------------------------------------------------
; Inputs:   a = byte to send.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de
; Remarks:  Disables interrupts. Holds clock line low at
;           the end, stopping the connected device from
;           sending any more data until the function is
;           called again.
; ---------------------------------------------------------

SendByte:
	di
	ld c,a

	; Issue RTS:

	; Set clock low
	ld a,(IOControl)
	or %00100010 ; A.TH (data) = input high
	and %11101110 ; A.TR (clock) = output low
	ld (IOControl),a
	out ($3F),a

	nop

	; Set data low too
	ld a,(IOControl)
	and %11001100 ; A.TH (data) and A.TR (clock) = output low
	ld (IOControl),a
	out ($3F),a
	
	nop

	; Release clock again
	ld a,(IOControl)
	or %00010001 ; A.TR (clock) = input high
	and %11011101 ; A.TH (data) = output low
	ld (IOControl),a
	out ($3F),a

	ld e,8
	ld d,c ; Original value

-:	call WaitBitLow

	ld a,(IOControl)
	srl c
	jr c,SendHighBit
SendLowBit:
	and %11011101
	jr +
SendHighBit:
	or %00100010
+:	ld (IOControl),a
	out ($3F),a
	call WaitBitHigh
	
	dec e
	jr nz,-

	; Send the parity bit

	call WaitBitLow
	
	ld a,(IOControl)
	ld c,a
	ld a,d
	or a
	ld a,c
	jp po,SendParityOdd
SendParityEven:
	or %00100010	
	jr +
SendParityOdd:
	and %11011101
+:	ld (IOControl),a
	out ($3F),a
	call WaitBitHigh

	; Send the stop bit

	call WaitBitLow
	ld a,(IOControl)
	or %00110011 ; A.TH, A.TR = input, high
	ld (IOControl),a
	out ($3F),a
	call WaitBitHigh

	; Send the ACK bit

	call WaitBitLow
	ld a,(IOControl)
	or %00010001 ; A.TR (clock) = input high
	and %11011101 ; A.TH (data) = output low
	ld (IOControl),a
	out ($3F),a
	call WaitBitHigh

	ld a,(IOControl)
	or %00100010 ; A.TH (data) = input high
	and %11101110 ; A.TR (clock) = output low
	ld (IOControl),a
	out ($3F),a
	
	xor a
	ret

; ---------------------------------------------------------
; SendAckByte -> Send a byte and check acknowledgement.
; ---------------------------------------------------------
; Inputs:   a = byte to send.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de
; Remarks:  Disables interrupts. Holds clock line low at
;           the end, stopping the connected device from
;           sending any more data until the function is
;           called again.
; ---------------------------------------------------------

SendAckByte:
	call SendByte
	ret nz
	call GetByte
	ret nz
	cp Command.Ack
	ret

; ---------------------------------------------------------
; SendSafeByte -> Send a byte securely, checking resend.
; ---------------------------------------------------------
; Inputs:   a = byte to send.
; Outputs:  z on success, nz on failure.
; Destroys: af, bc, de
; Remarks:  Disables interrupts. Holds clock line low at
;           the end, stopping the connected device from
;           sending any more data until the function is
;           called again.
; ---------------------------------------------------------
	
SendSafeByte:
	ld c,a
	ld b,Retries

-:	push bc
	ld a,c
	call SendAckByte
	pop bc
	ret z
	cp Command.Resend
	ret nz
	djnz -

; ---------------------------------------------------------
; GetByte -> Receives a byte.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  a = received byte.
;           z on success, nz on failure.
; Destroys: af, bc, e
; Remarks:  Disables interrupts. Holds clock line low at
;           the end, stopping the connected device from
;           sending any more data until the function is
;           called again.
; ---------------------------------------------------------

GetByte:
	di
	
	; Clear Link port
	ld a,(IOControl)
	or %00110011 ; A.TH, A.TR = input, high
	out ($3F),a

	; Get the start bit:

	call WaitBitLow
	call WaitBitHigh

	; Now we need to get the 8 bits for the byte

	; Reset the output byte
	ld c,0
	ld e,8

-:
	call WaitBitLow

	; Now we get the bit itself
	
	in a,($DD)
	
	; Data bit is in 6th bit (A.TH)

	rlca
	rlca
	rr c

	call WaitBitHigh

	dec e
	jr nz,-

	; Get the parity/stop bits

	call WaitBitLow
	call WaitBitHigh
	call WaitBitLow
	call WaitBitHigh

	; Clear flags, load code into accumulator and exit
	ld a,(IOControl)
	or %00100010 ; A.TH (data) = input high
	and %11101110 ; A.TR (clock) = output low
	out ($3F),a
	ld (IOControl),a
	xor a
	ld a,c
	ret

; For internal use only.
; Waits until the clock line falls low.
WaitBitLow:
	in a,($DC)
	and %00100000 ; A.TR = clock
	ret z
	ld b,Timeout
-:	in a,($DC)
	and %00100000 ; A.TR = clock
	ret z
	call WaitDelay
	djnz -
	pop bc
	jr Fail

; For internal use only.
; Waits until the clock line goes high.
WaitBitHigh:
	in a,($DC)
	and %00100000 ; A.TR = clock
	ret nz
	ld b,Timeout
-:	in a,($DC)
	and %00100000 ; A.TR = clock
	ret nz
	call WaitDelay
	djnz -
	pop bc
	jr Fail

; For internal use only.
; A short delay when waiting for the next clock.
WaitDelay:
	push af
	inc hl
	nop
	dec hl
	pop af
	ret

; For internal use only.
; Stops device sending data, returns nz (failure).
Fail:
	; Set nz to indicate failure, return.
	ld a,(IOControl)
	or %00100010 ; A.TH (data) = input high
	and %11101110 ; A.TR (clock) = output low
	out ($3F),a
	ld (IOControl),a
	or 1
	ret

.endmodule