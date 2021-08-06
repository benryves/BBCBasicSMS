; =========================================================
; Module: Keyboard
; =========================================================
; Description:
;     Routines for handling an AT or PS/2 keyboard.
; Functions:
;     Initialise: Detect, reset, initialise the mouse.
;     Update:     Read the status of the mouse and update
;                 variables.
; Variables:
;     Position:   Position of the mouse and scrollwheel.
;     Status:     Status of the buttons and mouse mode.
; =========================================================

.module Keyboard
; ---------------------------------------------------------
; GetScancode -> Retrieve a scancode from the keyboard.
; ---------------------------------------------------------
; Inputs:   None.
; Outputs:  z on success, nz on failure.
;           a    = scancode
;           c    = reset for press, set for release.
;           s    = set for extended key.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------

GetScancode:
	ld l,64
	call AT.GetByte
	ret nz
	cp $E0
	jr nz,+
	ld l,128|64
	call AT.GetByte
	ret nz
+:	cp $F0
	jr nz,+
	inc l
	call AT.GetByte
	ret nz
+:	ld h,a
	push hl
	pop af
	ret

LayoutFileDescription = allocVar(2)

LayoutFile = allocVar(0)
LayoutFile.StandardTableSize = allocVar(2)    ; Number of standard scancodes.
LayoutFile.ExtendedTableSize = allocVar(2)    ; Number of extended scancodes.
LayoutFile.ModifiersLookup = allocVar(2)      ; Address of modifier lookup list.
LayoutFile.ModifiersJumpTable = allocVar(2)   ; Address of modifier jump table.
LayoutFile.StandardTableAddress = allocVar(2) ; Address of standard scancode table.
LayoutFile.ExtendedTableAddress = allocVar(2) ; Address of extended scancode table.
LayoutFile.KeyOffsetTable = allocVar(2)       ; Address of key jump table.
LayoutFile.KeyDataTable = allocVar(2)         ; Address of key data table.
LayoutFile.KeyTypeTable = allocVar(2)         ; Address of the key type table.
LayoutFile.Size = allocVar(0) - LayoutFile

Status = allocVar(1)
LedState = allocVar(1)

OriginalScancode = allocVar(2)


; ---------------------------------------------------------
; LoadManualLayout -> Loads a keyboard layout definition.
; ---------------------------------------------------------
; Inputs:   hl = address of layout definition data.
; Destroys: af, bc, de, hl, ix
; ---------------------------------------------------------

LoadManualLayout:
	ld (OriginalScancode),hl ; Temp
	ld de,LayoutFile
	ld bc,LayoutFile.Size
	ldir
	ld (LayoutFileDescription),hl
	
	ld ix,LayoutFile + 4
	
	ld b,(LayoutFile.Size / 2) - 2
-:	ld e,(ix+0) \ ld d,(ix+1)
	ld hl,(OriginalScancode)
	add hl,de
	ld (ix+0),l \ ld (ix+1),h
	inc ix \ inc ix
	djnz -

	xor a
	ld (Status),a
	ld (LedState),a
	ret
	
; ---------------------------------------------------------
; ConvertScancode -> Convert a scancode to a key code.
; ---------------------------------------------------------
; Inputs:   a    = scancode.
;           c    = reset for press, set for release.
;           s    = set for extended key.
; Outputs:  z on matched key.
;           a    = key code.
;           c    = reset for press, set for release.
;           s    = set for non-printable key.
; Destroys: af, bc, de, hl
; ---------------------------------------------------------

ConvertScancode:
	push af
	pop hl
	ld (OriginalScancode),hl	
	ld hl,(LayoutFile.StandardTableAddress)
	ld d,h \ ld e,l
	ld bc,(LayoutFile.StandardTableSize)
	jp p,+
	ld hl,(LayoutFile.ExtendedTableAddress)
	ld bc,(LayoutFile.ExtendedTableSize)
+	
	push bc
	cpir
	pop bc
	ret nz
+	
	push hl
	push hl
	ld hl,(LayoutFile.StandardTableSize)
	ld b,h \ ld c,l
	ld hl,(LayoutFile.ExtendedTableSize)
	add hl,bc
	ld b,h \ ld c,l
	pop hl
	scf
	sbc hl,bc
	ld a,(hl)
	pop hl
	push af
	
	scf
	sbc hl,de
	add hl,hl
	ld de,(LayoutFile.KeyOffsetTable)
	add hl,de
	ld e,(hl) \ inc hl
	ld a,(hl)
	push af
	and ~%11000000
	ld d,a
	add hl,de
	; hl->key info block.

	pop af
	bit 7,a
	jr z,IsNotModifier
	bit 6,a
	jr z,NotToggle
	
	ld a,(OriginalScancode)
	bit 0,a
	jp nz,ExitNoKey

	ld a,(Status)
	xor (hl)
	ld (Status),a
	inc hl
	ld a,(LedState)
	xor (hl)
	ld (LedState),a
	
	ld a,$ED
	call AT.SendSafeByte
	ld a,(LedState)
	call AT.SendSafeByte
	
	inc hl
	jr IsNotModifier
	
NotToggle
	ld a,(OriginalScancode)
	bit 0,a
	jr nz,StatusOff
	ld a,(Status)
	or (hl)
	jr +
StatusOff
	ld a,(Status)
	ld b,a
	ld a,(hl)
	cpl
	and b
+:	ld (Status),a
	inc hl

IsNotModifier
	ld a,(Status)
	and (hl)
	jr z,NoStatusModifiers
	
	; Now we need to work out the index:
	
	ld b,(hl)
	push hl

	push af
	ld a,b

	ld bc,0
	ld hl,(LayoutFile.ModifiersLookup)
	ld d,h \ ld e,l
	cpir
	dec hl
	or a
	sbc hl,de
	add hl,hl
	ld de,(LayoutFile.ModifiersJumpTable)
	add hl,de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	add hl,de

	ld d,h \ ld e,l
	
	pop af
	cpir
	or a
	sbc hl,de
	ld d,h \ ld e,l
	dec de		
	pop hl
	add hl,de
	
NoStatusModifiers:
	inc hl
	ld a,(hl)
	cp $FF
	jr z,ExitNoKey

	ld d,a
	
	ld bc,(LayoutFile.KeyDataTable)
	or a
	sbc hl,bc	
	ld a,l
	and 7
	inc a
	ld e,a
	
	srl h \ rr l
	srl h \ rr l
	srl h \ rr l
	
	ld bc,(LayoutFile.KeyTypeTable)
	add hl,bc
	ld a,(hl)
	ld b,e
-:	sla a
	djnz -
	
	; Carry = non-printable key.

	ld a,(OriginalScanCode)
	
	res 7,a ; S
	jr nc,+
	set 7,a
+:
	
	set 6,a ; Z
	ld e,a
	push de
	pop af
	pop de
	ret

ExitNoKey:
	xor a
	dec a
	pop de
	ret

; ---------------------------------------------------------
; GetKey -> Retrieves a key from the keyboard.
; ---------------------------------------------------------
; Inputs:   None
; Outputs:  z on valid key input.
;           a    = key code.
;           c    = reset for press, set for release.
;           s    = set for non-printable key.
; Destroys: af
; ---------------------------------------------------------
	
GetKey:
	push hl
	push bc
	call GetScancode
	jr nz,+
	call ConvertScancode
+:	pop bc
	pop hl
	ret

; .........................................................
; Equates for the non-printable key codes
; .........................................................
.module KeyCode
    F1           = $00
    F2           = $01
    F3           = $02
    F4           = $03
    F5           = $04
    F6           = $05
    F7           = $06
    F8           = $07
    F9           = $08
    F10          = $09
    F11          = $0A
    F12          = $0B
    
    Up           = $0C
    Down         = $0D
    Left         = $0E
    Right        = $0F
    
    PageUp       = $10
    PageDown     = $11
    
    Insert       = $12
    
    PrintScreen  = $13
    
    Home         = $14
    End          = $15
    
    CtrlLeft     = $16
    CtrlRight    = $17
    
    Alt          = $18
    AltGr        = $19
    
    ShiftLeft    = $1A
    ShiftRight   = $1B
    
    WindowsLeft  = $1C
    WindowsRight = $1D
    
    CapsLock     = $1E
    NumLock      = $1F
    ScrollLock   = $20
    
    Application  = $21
    
    Power        = $22
    Sleep        = $23
    Wake         = $24
    
    Escape       = $25
.endmodule

.endmodule
