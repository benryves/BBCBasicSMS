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

; ==========================================================================
; GetDeviceKeyImmediate
; --------------------------------------------------------------------------
; Gets a key from the keyboard immediately and stores the device code.
; --------------------------------------------------------------------------
; Outputs:    A: ASCII code of the pressed key.
;             F: Z if key pressed or released, NZ if no key.
;                C if key not pressed, NC if key pressed.
;                P if printable key, M if extended key.
; Destroyed:  AF.
; ==========================================================================
GetDeviceKeyImmediate:
	di
	ld a,(Host.Flags)
	or 1<<Host.GetKeyPending
	ld (Host.Flags),a
	; Fall-through to GetDeviceKey

; ==========================================================================
; GetDeviceKey
; --------------------------------------------------------------------------
; Gets a key from the keyboard and stores the device code.
; --------------------------------------------------------------------------
; Outputs:    A: ASCII code of the pressed key.
;             F: Z if key pressed or released, NZ if no key.
;                C if key not pressed, NC if key pressed.
;                P if printable key, M if extended key.
; Destroyed:  AF.
; ==========================================================================
GetDeviceKey:
	push de
	
	di
	ld a,(Host.Flags)
	bit Host.GetKeyPending,a
	jp z,GetDeviceKey.Skip
	
	call Keyboard.GetKey
	jp nz,GetDeviceKey.NoKey
	
	push af
	call nc,HoldDeviceKey
	call c,ReleaseDeviceKey
	pop af
	
	; Ignore all released keys.
	jp c,GetDeviceKey.NoKey
	
	; Is it Escape?
	push af
	jp m,GetDeviceKeyExtended ; Check extended keys.
	
	cp 27
	jr z,GetDeviceKey.Escape
	
	bit 7,a
	jr z,ExitGetDeviceKey

GetDeviceKey.FunctionKey:
	
	; At this point we need to handle the function keys $8x and $Cx.
	pop af
	ld e,a
	
	; Check that it's either %1000xxxx or %1100xxxx
	; We know the top bit is 1 already.
	and %00110000
	jr z,GetDeviceKey.IsFunctionKey
	
	; If we get here, it's by mistake, so drop the key.
	jp GetDeviceKey.Skip

GetDeviceKey.IsFunctionKey:
	
	; Are we translating the keys?
	ld a,(Host.Flags)
	bit Host.CursorKeysFixed,a
	jr z,GetDeviceKey.TranslateFunctionKey
	
GetDeviceKey.FixedCursorKey:

	; Here we return fixed cursor key values.
	bit 6,e
	jr z,GetDeviceKey.Skip
	
	ld a,e
	cp $C9 ; END
	jr nz,GetDeviceKey.NotEnd
	
	xor a
	ld a,$87 ; COPY
	push af
	jr ExitGetDeviceKey

GetDeviceKey.NotEnd:
	
	; Check it's in the range left/right/down/up
	cp $CC
	jr c,GetDeviceKey.Skip
	
	; Transform the key.
	add a,$88-$CC
	ld e,a
	xor a
	ld a,e
	push af
	jr ExitGetDeviceKey


GetDeviceKey.TranslateFunctionKey:
	; Now we have a function key, we might need to offset it by the current status.
	ld a,(Keyboard.Status)
	and %111
	jr z,GetDeviceKey.GotFunctionKeyModifier
	
	cp %001 ; Shift
	jr z,GetDeviceKey.ShiftFunctionKeyModifier
	cp %010 ; Alt
	jr z,GetDeviceKey.AltFunctionKeyModifier
	cp %100 ; Ctrl
	jr z,GetDeviceKey.CtrlFunctionKeyModifier
	
	; Invalid combination of modifiers, so skip the key.
	jr GetDeviceKey.Skip

GetDeviceKey.ShiftFunctionKeyModifier:
	ld a,$10
	jr GetDeviceKey.GotFunctionKeyModifier
GetDeviceKey.CtrlFunctionKeyModifier:
	ld a,$20
	jr GetDeviceKey.GotFunctionKeyModifier
GetDeviceKey.AltFunctionKeyModifier:
	ld a,$30
GetDeviceKey.GotFunctionKeyModifier:
	add a,e
	ld e,a
	xor a
	ld a,e
	push af
	jr ExitGetDeviceKey

GetDeviceKey.Escape:
	
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
	
	; Is it Pause?
	cp Keyboard.KeyCode.Pause
	jp z,Host.PressBreakKey
	
	; Ignore all other "special" keys.
	pop af
	jr GetDeviceKey.Skip
	
ChangeGetDeviceKey:
	pop af
	xor a
	ld a,e
	jr ++
ExitGetDeviceKey:
	pop af
++:	
	; Now we have our device key, store it in the keyboard buffer.
	
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

; ==========================================================================
; HoldDeviceKey
; --------------------------------------------------------------------------
; Holds a key via its device code.
; --------------------------------------------------------------------------
; Inputs:     D: Device code of the key.
; ==========================================================================
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

; ==========================================================================
; ReleaseDeviceKey
; --------------------------------------------------------------------------
; Releases a key via its device code.
; --------------------------------------------------------------------------
; Inputs:     D: Device code of the key.
; ==========================================================================
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

; ==========================================================================
; GetKeyImmediate
; --------------------------------------------------------------------------
; Gets a key from the keyboard immediately.
; --------------------------------------------------------------------------
; Outputs:    A: ASCII code of the key that was pressed.
;             F: Z if no key was returned, NZ if a key was returned.
; ==========================================================================
GetKeyImmediate:
	di
	ld a,(Host.Flags)
	or 1<<Host.GetKeyPending
	ld (Host.Flags),a
	; Fall-through to GetKey

; ==========================================================================
; GetKey
; --------------------------------------------------------------------------
; Gets a key from the keyboard.
; --------------------------------------------------------------------------
; Outputs:    A: ASCII code of the key that was pressed.
;             F: Z if no key was returned, NZ if a key was returned.
; ==========================================================================
GetKey:
	
	push hl
	push de
	push bc
	
	ld a,(Host.ExecHandle)
	or a
	jr z,GetKey.Keyboard
	
	call File.GetHandle
	jr nz,GetKey.HandleClosed
	
	ld de,(Host.ExecHandle)
	call File.IsEOF
	jr nz,+
	
	ld de,(Host.ExecHandle)
	xor a
	ld (Host.ExecHandle),a
	call File.Close
	
	jr GetKey.Keyboard

+:	ld de,(Host.ExecHandle)
	call File.GetByte

	pop bc
	pop de
	pop hl
	ret

GetKey.HandleClosed:
	xor a
	ld (Host.ExecHandle),a

GetKey.Keyboard:
	
	
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
	
	ex de,hl
	ld (hl),0
	
	jp p,GetKeyGotKey
	
	ld b,a
	xor a
	inc a
	ld a,b
	
GetKeyGotKey:
	pop bc
	pop de
	pop hl
	ret

.endmodule