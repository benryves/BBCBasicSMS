.module MasterSystem16ColoursReduced

Vectors:
	jp Execute
	jp PutMap
	jp MasterSystem16Colours.BeginPlot
	jp MasterSystem16Colours.SetAlignedHorizontalLineSegment

Execute:
	or a \ jr z,Initialise
	dec a \ ret z
	dec a \ ret z
	dec a \ jp z,ScrollDown
	dec a \ jp z,ScrollUp
	dec a \ jp z,MasterSystem16Colours.GetUserDefinedCharacter
	dec a \ jp z,MasterSystem16Colours.SetUserDefinedCharacter
	dec a \ jp z,PreserveUnderCursor
	dec a \ jp z,RestoreUnderCursor
	dec a \ jp z,MasterSystem16Colours.SelectPalette
	dec a \ jp z,VDU.ResetMasterSystemPalette
	dec a \ jp z,ResetConsoleViewport
	dec a \ jp z,ResetGraphicsViewport
	dec a \ jp z,MasterSystem16Colours.GetPixel
	ret

Initialise:
	call MasterSystem16Colours.Initialise
	ld hl,Graphics.DivideBy8
	ld (Graphics.DivideCoordinate+1),hl
	ld hl,Graphics.MultiplyBy8
	ld (Graphics.MultiplyCoordinate+1),hl
	
	; Fill the nametable with graphics tiles.
	ld hl,MasterSystem16Colours.NameTable+(6+4*32)*2
	ld de,MasterSystem16Colours.MinGraphicsTile
	ld c,16
	
--:	push bc

	ld b,20
	call Video.SetWriteAddress
	
-:	ld a,e             ; 4
	out (Video.Data),a ; 11
	ld a,d             ; 4
	push hl            ; 11
	pop hl             ; 10
	out (Video.Data),a ; 11
	inc de             ; 6
	djnz -             ; 12
	
	ld bc,32*2
	add hl,bc
	
	pop bc
	dec c
	jr nz,--
	
	; Clear the pattern generator.
	ld hl,MasterSystem16Colours.PatternGenerator+MasterSystem16Colours.MinGraphicsTile*32
	call Video.SetWriteAddress
	ld bc,20*16*32
	
-:	xor a
	out (Video.Data),a
	dec bc
	ld a,b
	or c
	jr nz,-
	
	ret
	
ResetConsoleViewport:
	; Resolution = 20 * 16
	ld a,6
	ld (Console.MinCol),a
	ld (Console.OriginX),a
	ld a,25
	ld (Console.MaxCol),a
	ld a,20
	ld (Console.MaxWidth),a
	
	ld a,4
	ld (Console.MinRow),a
	ld (Console.OriginY),a
	ld a,19
	ld (Console.MaxRow),a
	ld a,16
	ld (Console.MaxHeight),a
	ret

ResetGraphicsViewport:
	; Resolution = 160 * 128
	ld a,48
	ld (Graphics.MinX),a
	ld (Graphics.OffsetX),a
	ld a,255-48
	ld (Graphics.MaxX),a
	
	ld a,32
	ld (Graphics.MinY),a
	ld (Graphics.OffsetY),a
	ld a,191-32
	ld (Graphics.MaxY),a
	ret

PutMap:
	cp 32
	ret c
	
	push hl
	push de
	push bc
	
	call VDU.GetCharacterData
	push hl
	
	call GetPatternAddressForCursor
	call Video.SetWriteAddress
	
	pop hl

	ld a,(Console.Colour)
	ld d,a
	rrca
	rrca
	rrca
	rrca
	ld e,a
	
	ld c,8
--:	push de
	
	ld b,4
	
-:	push bc
	
	ld a,(hl)
	cpl
	srl e
	jr c,+
	xor a
+:	ld c,a
	
	srl d
	sbc a,a
	and (hl)
	or c
	
	out (Video.Data),a
	
	pop bc
	djnz -
	
	inc hl
	
	pop de
	dec c
	jr nz,--
	
	pop bc
	pop de
	pop hl
	ei
	ret

GetPatternAddressForCursor:
	call MasterSystem16Colours.GetNameTableAddressForCursor
	call Video.SetReadAddress
	
	in a,(Video.Data)
	ld l,a
	push hl
	pop hl
	in a,(Video.Data)
	ld h,a
	
	ld b,5
-:	add hl,hl
	djnz -
	
	.if MasterSystem16Colours.PatternGenerator != 0
	ld bc,MasterSystem16Colours.PatternGenerator
	add hl,bc
	.endif
	ret

PreserveUnderCursor:
	call GetPatternAddressForCursor
	ld de,MasterSystem16Colours.PatternGenerator+32
	call CopyPatternData
	ei
	ret

RestoreUnderCursor:
	call GetPatternAddressForCursor
	call Video.SetWriteAddress
	ld de,MasterSystem16Colours.PatternGenerator+32
	ex de,hl
	call CopyPatternData
	ei
	ret

CopyPatternData:
	call Video.SetReadAddress
	
	ld hl,32
	call Host.GetSafeScratchMemoryHL
	ret c
	
	push hl

	ld b,32	
-:	in a,(Video.Data)
	ld (hl),a
	inc hl
	djnz -
	
	pop hl
	
	ex de,hl
	call Video.SetWriteAddress
	
	ld b,32
-:	ld a,(de)
	out (Video.Data),a
	inc de
	djnz -
	
	ret


ScrollDown:
	push bc
	push de
	push hl
	
	; Each row is 64 bytes long.
	ld de,-64
	
	; Get the pointer to the bottom left corner.
	ld a,(Console.MaxRow)
	jr ScrollFromRow
	
	
ScrollUp:
	push bc
	push de
	push hl
	
	; Each row is 64 bytes long.
	ld de,64
	
	; Get the pointer to the top left corner.
	ld a,(Console.MinRow)
	
ScrollFromRow:
	call MasterSystem16Colours.AMul64
	ld a,(Console.MinCol)
	add a,a
	
	ld c,a
	ld b,0
	add hl,bc
	
.if MasterSystem16Colours.NameTable != 0
	ld bc,MasterSystem16Colours.NameTable
	add hl,bc
.endif
	
	
	; How many columns will we need to move?
	ld a,(Console.MinCol)
	ld c,a
	ld a,(Console.MaxCol)
	sub c
	inc a
	add a,a
	ld c,a
	
	; How many rows will we need to move?
	ld a,(Console.MinRow)
	ld b,a
	ld a,(Console.MaxRow)
	sub b
	inc a
	ld b,a
	
	call Console.RotateBlock
	
	; Clear the bottom row by temporarily setting a viewport where minrow = maxrow.
	ld bc,(Console.MinRow)
	push bc
	
	bit 7,d
	jr z,+
	ld b,c
	jr ++
+:	ld c,b
++:
	ld (Console.MinRow),bc
	call Console.Clear
	
	pop bc
	ld (Console.MinRow),bc
	
	pop hl
	pop de
	pop bc
	ei
	ret

.endmodule