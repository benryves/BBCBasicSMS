.module Video

	Reset ; Preload the VDP registers with sensible data.
		ld hl,ResetData 
		ld b,ResetDataEnd-ResetData
		ld c,$BF
		otir
		jp ClearAll
		ResetData .db $04,$80,$84,$81,$FF,$82,$FF,$85,$FF,$86,$FF,$87,$00,$88,$00,$89,$FF,$8A \ ResetDataEnd
		
	GotoHL ; Set the VRAM pointer to the address in HL.
		in a,($BF)
		ld a,l
		out ($BF),a
		ld a,h
		or %01000000
		out ($BF),a
		ret
		
	ScreenOn ; Switch the screen on.
		ld a,%01000000
		jr +
	ScreenOff ; Switch the screen off
		xor a
	+	ld b,1
	
	SetReg ; Set register B to value A.
		out ($BF),a
		ld a,%10000000
		or b
		out ($BF),a
		ret
		
	GotoPalette ; Set the CRAM pointer to colour a.
		out ($BF),a
		ld a,$C0
		out ($BF),a
		ret
	
	ClearAll
		ld hl,$0000
		call GotoHL
		ld hl,16*1024
	-	xor a
		out ($BE),a
		dec hl
		ld a,h \ or l
		jr nz,-
		call GotoPalette
		xor a
		ld b,64
	-	out ($BE),a
		djnz -
		ret
	
	WaitVBlank:
		in a,($BF)
		bit 7,a
		ret nz
		jr WaitVBlank
		
.endmodule