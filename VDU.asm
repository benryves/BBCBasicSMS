.module VDU

.module Modes

.include "Text.asm"
.include "Mode4.asm"

Count = 2

.endmodule

.var ubyte[3] PutMap

Reset:
	xor a
SetMode:
	di
	push af
	
	ld a,$C3 ; JP
	ld (PutMap),a
	
	; Reset all video settings to their defaults.
	call Video.Reset
	
	pop af
	
	; Mode-specific initialisation.
	call SetModeInitialize

	; Get the VDU queue ready
	call VDU.ClearQueue
	
	; Move to the top-left of the screen.
	call HomeUp

	; Screen on, enable frame interrupts.
	call Video.DisplayOn
	call Video.EnableFrameInterrupts
	ei
	
	ret

SetModeInitialize:
	or a
	jp z,Modes.Text.Initialise
	dec a
	jp z,Modes.Mode4.Initialise
	ret

QueueCapacity = 32

.var ubyte[QueueCapacity] Queue
.var ubyte QueueSize
.var uword QueueRead
.var ubyte QueueReadToEnd
.var uword QueueWrite
.var ubyte QueueWriteToEnd

ClearQueue:
	xor a
	ld (QueueSize),a
	
	ld hl,Queue
	ld (QueueRead),hl
	ld (QueueWrite),hl
	
	ld a,QueueCapacity
	ld (QueueReadToEnd),a
	ld (QueueWriteToEnd),a
	ret

Enqueue:
	push af
-:	ld a,(QueueSize)
	cp QueueCapacity
	jr nz,+
	ei
	halt
	jr -
+:	di
	inc a
	ld (QueueSize),a
	pop af
	push hl
	
	ld hl,(QueueWrite)
	ld (hl),a
	inc hl
	
	ld a,(QueueWriteToEnd)
	dec a
	jr nz,+
	ld hl,Queue
	ld a,QueueCapacity
+:	ld (QueueWrite),hl
	ld (QueueWriteToEnd),a
	
	pop hl
	ei
	ret
	
	
Dequeue:
	ld a,(QueueSize)
	or a
	jr nz,+
	dec a
	ret
+:
	di
	
	dec a
	ld (QueueSize),a
	
	push hl
	ld hl,(QueueRead)
	ld a,(hl)
	
	inc hl
	
	push af
	
	ld a,(QueueReadToEnd)
	dec a
	jr nz,+
	ld hl,Queue
	ld a,QueueCapacity
+:	ld (QueueRead),hl
	ld (QueueReadtoEnd),a
	
	pop af
	
	pop hl
	
	cp a
	ret

WaitForEmptyQueue:
	ld a,(QueueSize)
	or a
	ret z
	ei
	halt
	jr WaitForEmptyQueue

FontTileIndex = 0
FontCharOffset = FontTileIndex-' '

.var ubyte CurRow, CurCol
.var ubyte MinRow, MaxRow, MinCol, MaxCol

HomeUp:
	ld a,(MinCol)
	ld (CurCol),a
	ld a,(MinRow)
	ld (CurRow),a
	ret
	

PutChar:
	cp '\r'
	jr nz,+
	ld a,(MinCol)
	ld (CurCol),a
	ret

+:	cp '\n'
	jr nz,+
	
	ld a,(MinCol)
	ld (CurCol),a
	jr NewLine

+:	call PutMap
	; Fall-through to CursorRight

CursorRight:
	ld a,(CurCol)
	inc a
	push bc
	ld bc,(MaxCol)
	cp c
	pop bc
	jr nz,+
	ld a,(MinCol)
+:	ld (CurCol),a
	ret nz
	; Fall-through to NewLine

NewLine:
	ld a,(CurRow)
	inc a
	push bc
	ld bc,(MaxRow)
	cp c
	pop bc
	jr nz,+
	ld a,(MinRow)
+:	ld (CurRow),a
	ret

CursorLeft:
	ld a,(CurCol)
	push bc
	ld bc,(MinCol)
	cp c
	pop bc
	jr nz,+
	ld a,(MaxCol)
+:	push af
	dec a
	ld (CurCol),a
	pop af
	ret nz
+:	; Move up a row
	ld a,(CurRow)
	push bc
	ld bc,(MinRow)
	cp c
	pop bc
	jr nz,+
	ld a,(MaxRow)
+:	push af
	dec a
	ld (CurRow),a
	pop af
	ret

PutString:
	ld a,(hl)
	inc hl
	or a
	ret z
	push hl
	call PutChar
	pop hl
	jr PutString

	

.endmodule