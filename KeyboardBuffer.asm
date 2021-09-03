.module KeyboardBuffer

; These two must be contiguous.
HeldKeyCount = 6
HeldKeys = allocVar(HeldKeyCount)
KeyboardBufferSize = 6
KeyboardBuffer = allocVar(KeyboardBufferSize)

Reset:
	; Clear the held keys + keyboard buffer.
	; Empty the keyboard buffer.
	xor a
	ld hl,HeldKeys
	ld de,HeldKeys+1
	ld bc,HeldKeyCount+KeyboardBufferSize-1
	ld (hl),a
	ldir
	ret

CheckKeyboardWithInterrupt:
.if AllowCheckingKeyboardByInterrupt
	push af
	ld a,(Video.Registers+$00)
	and %00010000
	jr nz,+
	push bc
	call Video.EnableLineInterrupt
	pop bc
+:	pop af
	ei
	ret
.endif

CheckKeyboardByPolling:
.if AllowCheckingKeyboardByInterrupt
	push af
	ld a,(Video.Registers+$00)
	and %00010000
	jr z,+
	push bc
	call Video.DisableLineInterrupt
	pop bc
	di
	ld a,(Host.Flags)
	or 1<<Host.GetKeyPending
	ld (Host.Flags),a
+:	pop af
	ei
.endif
	ret

; ---------------------------------------------------------
; GetDeviceKey -> Gets a key from the keyboard and stores
; the device code.
; ---------------------------------------------------------
; This is a wrapper around Keyboard.GetKey that stores the
; state of any pressed/released keys.
; ---------------------------------------------------------
; Outputs:  z = if key pressed or released, nz if no key.
;           c = if key not pressed, nc if key pressed.
;           p = if printable key, m if extended key.
; Destroys: af.
; ---------------------------------------------------------
GetDeviceKey:
	push de
	
	di
	ld a,(Host.Flags)
	bit Host.GetKeyPending,a
	jp z,GetDeviceKey.Skip
	
	call Keyboard.GetKey
	jr nz,GetDeviceKey.NoKey
	
	push af
	call nc,HoldDeviceKey
	call c,ReleaseDeviceKey
	pop af
	
	; Ignore all released keys.
	jr c,GetDeviceKey.NoKey
	
	; Is it Escape?
	push af
	jp m,GetDeviceKeyExtended ; Check extended keys.
	
	cp 27
	jr nz,ExitGetDeviceKey
	
	; Set the Escape condition.
	call Host.PressEscapeKey
	
	; Have we also disabled the ASCII code?
	ld a,(Host.Flags)
	and (1<<Host.EscapeKeyDisabled)|(1<<Host.EscapeErrorDisabled)
	cp (0<<Host.EscapeKeyDisabled)|(1<<Host.EscapeErrorDisabled)
	
	jr nz,ExitGetDeviceKey
	ld e,0
	jr ChangeGetDeviceKey

GetDeviceKeyExtended:
	
	; Is it Insert?
	cp Keyboard.KeyCode.Insert
	jr nz,GetDeviceKeyNotInsert
	
	; Toggle insert/overwrite, then pretend no key happened.
	ld a,(VDU.Console.ConsoleFlags)
	xor 1 << VDU.Console.Overwrite
	ld (VDU.Console.ConsoleFlags),a
	pop af
	
	jr GetDeviceKey.Skip

GetDeviceKeyNotInsert:
	
	; Is it Pause?
	cp Keyboard.KeyCode.Pause
	jp z,Host.PressBreakKey
	
	; Is it a cursor key?
	ld e,a
	ld a,(VDU.Console.ConsoleFlags)
	bit VDU.Console.CursorEditingDisabled,a
	ld a,e
	jr z,ExitGetDeviceKey
	ld e,136
	cp Keyboard.KeyCode.Left
	jr z,ChangeGetDeviceKey
	inc e
	cp Keyboard.KeyCode.Right
	jr z,ChangeGetDeviceKey	
	inc e
	cp Keyboard.KeyCode.Down
	jr z,ChangeGetDeviceKey
	inc e
	cp Keyboard.KeyCode.Up
	jr z,ChangeGetDeviceKey
	
	jr ExitGetDeviceKey
	
ChangeGetDeviceKey:
	pop af
	xor a
	ld a,e
	jr ++
ExitGetDeviceKey:
	pop af
++:	
	; Now we have our device key, store it in the keyboard buffer.
	
	; If it's an extended key, set the top two bits.	
	jp p,+
	or %11000000
+:
	
	push hl
	push bc
	; Store our key in C.
	ld c,a
	
	; Can we store the key in our buffer?
	ld hl,KeyboardBuffer
	ld b,KeyboardBufferSize
	
-:	ld a,(hl)
	or a
	jr z,+
	inc hl
	djnz -
	jr BufferFull ; No room in the buffer!
	
+:	; There is room in the buffer at (hl)
	ld (hl),c
	
BufferFull:
	pop bc
	pop hl
		
	pop de
	ei
	ret

GetDeviceKey.NoKey:
	ld a,(Host.Flags)
	res Host.GetKeyPending,a
	ld (Host.Flags),a
GetDeviceKey.Skip:
	xor a
	dec a
	pop de
	ei
	ret

; ---------------------------------------------------------
; HoldDeviceKey -> Hold a key via its device code.
; ---------------------------------------------------------
; Inputs:   d = device code.
; Outputs:  None.
; Destroys: None.
; ---------------------------------------------------------
HoldDeviceKey:
	push af
	push hl
	push de
	push bc
	
	ld hl,HeldKeys
	ld b,HeldKeyCount
	
-:	ld a,(hl)
	cp d
	jr z,++ ; Already holding
	or a
	jr z,+ ; A free slot!
	inc hl
	djnz -
	
	; No free slot.
	; Shift the entire buffer left.
	ld a,d
	
	ld hl,HeldKeys+1
	ld de,HeldKeys+0
	ld bc,HeldKeyCount-1
	ldir
	ld (de),a
	jr ++

+:	ld (hl),d
++:	pop bc
	pop de
	pop hl
	pop af
	ret

; ---------------------------------------------------------
; ReleaseDeviceKey -> Releases a key via its device code.
; ---------------------------------------------------------
; Inputs:   d = device code.
; Outputs:  None.
; Destroys: None.
; ---------------------------------------------------------
ReleaseDeviceKey:
	push af
	push hl
	push de
	push bc
	
	ld a,d
	ld hl,HeldKeys
	ld bc,HeldKeyCount
	cpir
	jr nz,++ ; It's not currently held.
	
	ld a,b
	or c
	jr z,+
	ld d,h
	ld e,l
	dec de
	ldir
+:	ex de,hl
	ld (hl),0
++:
	pop bc
	pop de
	pop hl
	pop af
	ret
	
; ---------------------------------------------------------
; GetKey -> Gets a key from the keyboard.
; ---------------------------------------------------------
; Outputs:  nz = if a key was returned.
;           z = if no keys were returned.
; Destroys: af.
; ---------------------------------------------------------
GetKey:
	
	push hl
	push de
	push bc
	
	; First things first, always process the incoming key.
	; We do this even if the buffer is full.
	call GetDeviceKey
	
	ld a,(KeyboardBuffer)
	or a
	jr z,GetKeyGotKey
	
	; We have our key in A!
	
	; Shift the buffer <- 1
	ld hl,KeyboardBuffer+1
	ld de,KeyboardBuffer
	ld bc,KeyboardBufferSize-1
	ldir
	
	jp p,GetKeyGotKey
	
	bit 6,a
	jr nz,GetKeyIsExtendedKey
	
	ld b,a
	xor a
	inc a
	ld a,b
	jr GetKeyGotKey

GetKeyIsExtendedKey:
	or a
	res 6,a
	res 7,a
	
GetKeyGotKey:
	pop bc
	pop de
	pop hl
	ret

.endmodule