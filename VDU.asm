.module VDU

QueueCapacity = 8

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

FontTileIndex = 0
FontCharOffset = FontTileIndex-' '

.var ubyte CurRow, CurCol
.var ubyte MinRow, MaxRow, MinCol, MaxCol

PutMap:
	push bc
	push af
	ld a,(CurRow)
	ld l,a
	ld h,0
	ld b,6
-:	add hl,hl
	djnz -
	ld a,(CurCol)
	add a,a
	ld e,a
	ld d,0
	add hl,de
	ld de,$3800
	add hl,de
	call Video.GotoHL
	pop af
	add a,FontCharOffset
	out (Ports.Video.Data),a
	xor a
	out (Ports.Video.Data),a
	pop bc
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