.module VDU

; Temporary storage for a single 8x8 tile.
TempTile = allocVar(8)

FontTileIndex = 0
FontCharOffset = FontTileIndex-' '

; Dependencies
.include "Console.asm"
.include "Graphics.asm"

; Mode driver functions.
Function.End = 0
Function.Initialise = 1
Function.Clear = 2
Function.PutMap = 3
Function.Scroll = 4
Function.BeginPlot = 5
Function.SetPixel = 6
Function.SetAlignedHorizontalLineSegment = 7
Function.SetUserDefinedCharacter = 8
Function.SetConsoleColour = 9
Function.SetGraphicsColour = 10
Function.ResetConsoleViewport = 11
Function.SelectPalette = 12

Functions.Count = 12
FunctionVectors = allocVar(Functions.Count * 3)

.function VDUFunctionAddress(function)
	VDUFunctionAddress = FunctionVectors + 3 * (function - 1)
.endfunction

; Mode files
.module Modes

	; Vectors usable by graphics drivers.
	ManipulatePixelBitmask = allocVar(3)
	ManipulatePixelColour = allocVar(3)


	.include "Text.asm"
	.include "GraphicsII.asm"
	.include "Mode4.asm"

	Count = 3
	
	Functions:
		.dw Text.Functions
		.dw GraphicsII.Functions
		.dw Mode4.Functions


.endmodule

Clear = VDUFunctionAddress(Function.Clear)
PutMap = VDUFunctionAddress(Function.PutMap)
Scroll = VDUFunctionAddress(Function.Scroll)
BeginPlot = VDUFunctionAddress(Function.BeginPlot)
SetPixel = VDUFunctionAddress(Function.SetPixel)
SetAlignedHorizontalLineSegment = VDUFunctionAddress(Function.SetAlignedHorizontalLineSegment)
SetUserDefinedCharacter = VDUFunctionAddress(Function.SetUserDefinedCharacter)
SetConsoleColour = VDUFunctionAddress(Function.SetConsoleColour)
SetGraphicsColour = VDUFunctionAddress(Function.SetGraphicsColour)
ResetConsoleViewport = VDUFunctionAddress(Function.ResetConsoleViewport)
SelectPalette = VDUFunctionAddress(Function.SelectPalette)

LoadModeFunctions:
	
	push hl
	push de
	push bc
	
	push hl
	
	ld hl,FunctionVectors
	ld de,FunctionVectors+1
	ld bc,(Functions.Count*3)-1
	ld a,$C9 ; RET
	ld (hl),a
	ldir
	
	pop hl
	
	jr LoadFunctionLoop

AppendModeFunctions:

	push hl
	push de
	push bc
	
LoadFunctionLoop:
	ld a,(hl)
	
	or a
	jr z,LoadedAllFunctions
	
	add a,a
	add a,(hl)
	ld e,a
	ld d,0
	
	push hl
	ld hl,FunctionVectors-3
	add hl,de
	ex de,hl
	pop hl
	
	ld a,$C3 ; JP
	ld (de),a
	
	inc hl
	inc de
	
	ldi
	ldi
	
	jr LoadFunctionLoop
	
LoadedAllFunctions:	

	pop bc
	pop de
	pop hl
	
	ret

DefaultFunctions:
	.db Function.Clear \ .dw DefaultClear
	.db Function.ResetConsoleViewport \ .dw DefaultResetConsoleViewport
	.db Function.End

; Font data
.module Fonts

	Font8x8:
	.incbin "Font8x8.bin"

	Font6x8:
	.incbin "Font6x8.bin"

.endmodule

; Palette data
.module Palettes
	
	; BBC BASIC "physical" palette is:
	;  0 = Black
	;  1 = Red
	;  2 = Green
	;  3 = Yellow
	;  4 = Blue
	;  5 = Magenta
	;  6 = Cyan
	;  7 = White
	;  8 = Grey
	;  9 = Bright red
	; 10 = Bright green
	; 11 = Bright yellow
	; 12 = Bright blue
	; 13 = Bright magenta
	; 14 = Bright cyan
	; 15 = Bright white

	TMS9918A:
	; TMS9918A has:
	;  0 = Transparent
	;  1 = Black
	;  2 = Medium Green
	;  3 = Light Green
	;  4 = Dark Blue
	;  5 = Medium Blue
	;  6 = Maroon
	;  7 = Cyan
	;  8 = Dark red
	;  9 = Bright red
	; 10 = Dark yellow
	; 11 = Yellow
	; 12 = Dark green
	; 13 = Magenta
	; 14 = Grey
	; 15 = White
	.db  1 ;  0 = Black
	.db  8 ;  1 = Red
	.db  2 ;  2 = Green
	.db 11 ;  3 = Yellow
	.db  5 ;  4 = Blue
	.db 13 ;  5 = Magenta
	.db  7 ;  6 = Cyan
	.db 15 ;  7 = White
	.db 14 ;  8 = Grey
	.db  9 ;  9 = Bright red
	.db  3 ; 10 = Bright green
	.db 11 ; 11 = Bright yellow
	.db  5 ; 12 = Bright blue
	.db 13 ; 13 = Bright magenta
	.db  7 ; 14 = Bright cyan
	.db 15 ; 15 = Bright white
	
	SegaMasterSystem:
	.db %000000 ;  0 = Black
	.db %000010 ;  1 = Red
	.db %001000 ;  2 = Green
	.db %001010 ;  3 = Yellow
	.db %100000 ;  4 = Blue
	.db %100010 ;  5 = Magenta
	.db %101000 ;  6 = Cyan
	.db %101010 ;  7 = White
	.db %010101 ;  8 = Grey
	.db %000011 ;  9 = Bright red
	.db %001100 ; 10 = Bright green
	.db %001111 ; 11 = Bright yellow
	.db %110000 ; 12 = Bright blue
	.db %110011 ; 13 = Bright magenta
	.db %111100 ; 14 = Bright cyan
	.db %111111 ; 15 = Bright white

.endmodule

; Mode-specific vectors.
CommandQueue.Capacity = 10

CommandQueue = allocVar(CommandQueue.Capacity)
CommandQueue.Waiting = allocVar(1)

Reset:
	xor a
SetMode:
	di
	push af
	
	; Reset all video settings to their defaults.
	call Video.Reset
	
	pop af
	
	; Mode-specific initialisation.
	call SetModeInitialize
	
	; Reset console and graphics now the mode is loaded.
	call Console.Reset
	call Graphics.Reset
	
	; Clear the command queue.
	xor a
	ld (CommandQueue),a
	ld (CommandQueue.Waiting),a
	
	; Reset colours to their defaults.
	call ResetColoursCommand
	
	; Screen on, enable frame interrupts.
	call Video.DisplayOn
	call Video.EnableFrameInterrupt
	ei
	
Stub:
	ret

SetModeInitialize:
	cp Modes.Count
	ret nc
	
	push af
	ld hl,DefaultFunctions
	call LoadModeFunctions
	pop af
	
	add a,a
	ld e,a
	ld d,0
	ld hl,Modes.Functions
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	
	call AppendModeFunctions
	call FunctionVectors
	ret

; ---------------------------------------------------------
; WriteWord -> Writes a word to the VDU.
; ---------------------------------------------------------
; Inputs:   hl = word to write, writing L before H.
; Outputs:  None.
; Destroys: None.
; ---------------------------------------------------------
WriteWord:
	push hl
	push de
	push bc
	push af
	
	push hl
	
	ld a,l
	call WriteByteDestructive
	
	pop hl
	
	ld a,h
	call WriteByteDestructive
	
	pop af
	pop bc
	pop de
	pop hl
	ret

; ---------------------------------------------------------
; WriteByte -> Writes a character to the VDU, converting
;              CR to an LF, CR sequence.
; ---------------------------------------------------------
; Inputs:   a = value to output.
; Outputs:  None.
; Destroys: f.
; ---------------------------------------------------------
PutChar:
	cp '\r'
	jr nz,WriteByte
	
	ld a,'\n'
	call WriteByte
	ld a,'\r'
	; Fall-through to WriteByte.

; ---------------------------------------------------------
; WriteByte -> Writes a byte to the VDU.
; ---------------------------------------------------------
; Inputs:   a = value to output.
; Outputs:  None.
; Destroys: None.
; ---------------------------------------------------------
WriteByte:
	push hl
	push de
	push bc
	push af
	call WriteByteDestructive
	pop af
	pop bc
	pop de
	pop hl
	ret

; ---------------------------------------------------------
; WriteByteDestructive -> Writes a byte to the VDU.
; ---------------------------------------------------------
; Inputs:   a = value to output.
; Outputs:  None.
; Destroys: af, bc, de, hl.
; ---------------------------------------------------------
WriteByteDestructive:
	; Check to see if we're waiting for anything in the command queue.
	ld c,a
	ld a,(CommandQueue.Waiting)
	or a
	jr z,VDU.WriteByteNotWaiting
	
	; Calculate the offset to the command queue.
	ld hl,CommandQueue + CommandQueue.Capacity
	ld e,a
	ld d,-1
	add hl,de
	
	; Store the received value in the command queue.
	ld (hl),c
	inc a
	ld (CommandQueue.Waiting),a
	
	ret nz ; Still waiting for more data...
	
HandleCommand:
	
	; At this point we have a multi-byte VDU command stored in the queue.
	ld a,(CommandQueue)
	
	; Quick sanity check... VDU commands must be below 32.
	cp 32
	ret nc
	
	; Find the address of the command handler in the jump table.
	
	call GetCommandJumpTable
	jp (hl)

WriteByteNotWaiting:
	
	; We're not waiting to fill up the command queue before doing something exciting.
	ld a,c
	ld (CommandQueue),a
	
	; Is it a special command?
	cp 32
	jr nc,NotCommand
	
	; It is!
	
	; Find the address of the command handler in the jump table.
	call GetCommandJumpTable
	
	; How long are we waintg?
	ld (CommandQueue.Waiting),a
	ret nz
	
	; It's a zero-byte command, so execute immediately.
	jp (hl)

NotCommand:

	cp 127 ; DELETE
	jp z,Delete

PutLiteralChar:
	call PutMap
	jp Console.CursorRight

GetCommandJumpTable:
	ld l,a
	add a,a
	add a,l
	ld l,a
	ld h,0
	ld de,CommandJumpTable
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld a,(hl)
	ex de,hl
	or a
	ret
	
CommandJumpTable:
	.dw Stub                    \ .db  0 ;  0 NUL
	.dw Stub                    \ .db  0 ;  1 Data -> Printer
	.dw Stub                    \ .db  0 ;  2 Enable printer.
	.dw Stub                    \ .db  0 ;  3 Disable printer.
	.dw Stub                    \ .db  0 ;  4 Write text at text cursor position.
	.dw Stub                    \ .db  0 ;  5 Write text at graphics cursor position.
	.dw Stub                    \ .db  0 ;  6 Enable output to the screen.
	.dw Stub                    \ .db  0 ;  7 BEL
	.dw Console.CursorLeft      \ .db  0 ;  8 Move text cursor backwards one character.
	.dw Console.CursorRight     \ .db  0 ;  9 Move text cursor forwards one character.
	.dw Console.CursorDown      \ .db  0 ; 10 Move text cursor down a line.
	.dw Console.CursorUp        \ .db  0 ; 11 Move text cursor up a line.
	.dw Clear                   \ .db  0 ; 12 Clear the text area (CLS).
	.dw Console.HomeLeft        \ .db  0 ; 13 Move text cursor to start of current line.
	.dw Stub                    \ .db  0 ; 14 Enable the auto-paging mode.
	.dw Stub                    \ .db  0 ; 15 Disable the auto-paging mode.
	.dw Graphics.Clear          \ .db  0 ; 16 Clear the graphics area (CLG).
	.dw TextColourCommand       \ .db -1 ; 17 Define a text colour (COLOUR).
	.dw GraphicsColourCommand   \ .db -2 ; 18 Define a graphics colour (CGOL).
	.dw SelectPaletteCommand    \ .db -5 ; 19 Select a colour palette.
	.dw ResetColoursCommand     \ .db  0 ; 20 Restore default logical colours.
	.dw Stub                    \ .db  0 ; 21 Disable output to the screen.
	.dw ModeCommand             \ .db -1 ; 22 Set the screen mode (MODE).
	.dw UserCommand             \ .db -9 ; 23 User-defined characters and screen modes.
	.dw GraphicsViewportCommand \ .db -8 ; 24 Define a graphics viewport.
	.dw PlotCommand             \ .db -5 ; 25 PLOT
	.dw Stub                    \ .db  0 ; 26 Restore default viewports.
	.dw EscapeCharCommand       \ .db -1 ; 27 Send the next character to the screen.
	.dw ConsoleViewportCommand  \ .db -4 ; 28 Define a text viewport.
	.dw SetOriginCommand        \ .db -4 ; 29 Set the graphics origin.
	.dw Console.HomeUp          \ .db  0 ; 30 Home the cursor to the top-left of the screen.
	.dw TabCommand              \ .db -2 ; 31 Move the text cursor (TAB(x,y)).

.function VDUQ(offset, commandLength)
	VDUQ = VDU.CommandQueue + offset + VDU.CommandQueue.Capacity - commandLength
.endfunction

; ========================================================================================
; VDU 4                                                           DRAW TEXT AT TEXT CURSOR
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 5                                                       DRAW TEXT AT GRAPHICS CURSOR
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 13                                                                   CARRIAGE RETURN
; ========================================================================================

; ========================================================================================
; VDU 14                                                           ENABLE AUTO-PAGING MODE
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 15                                                          DISABLE AUTO-PAGING MODE
; ========================================================================================
;;; TODO

; ========================================================================================
; VDU 16                                                           CLEAR GRAPHICS VIEWPORT
; ========================================================================================
ClearGraphicsCommand = Graphics.Clear

; ========================================================================================
; VDU 17,<colour>                                                          SET TEXT COLOUR
; ========================================================================================
TextColourCommand:
	ld a,(VDUQ(0, 1))
	jp SetConsoleColour
	
; ========================================================================================
; VDU 18,<mode>,<colour>                                               SET GRAPHICS COLOUR
; ========================================================================================
GraphicsColourCommand:
	ld a,(VDUQ(1, 2))
	jp SetGraphicsColour

; ========================================================================================
; VDU 19,<logical>,<physical>,<r>,<g>,<b>                                   SELECT PALETTE
; ========================================================================================
SelectPaletteCommand:
	ld bc,(VDUQ(0, 5)) ; C = logical colour, B = physical colour.
	ld a,b ; A = "actual" :)
	ld hl,VDUQ(2, 5) ; hl -> RGB colour.
	call SelectPalette
	ret	

; ========================================================================================
; VDU 20                                                                     RESET COLOURS
; ========================================================================================
ResetColoursCommand:
	
	; Reset to default colours.
	ld a,7
	call SetConsoleColour
	ld a,128
	call SetConsoleColour
	ld a,7
	call SetGraphicsColour
	ld a,128
	call SetGraphicsColour
	
	; Reset to default palette.
	ld bc,16*256
-:	ld a,c
	ld (VDUQ(0, 5)),a
	ld (VDUQ(1, 5)),a
	xor a
	ld (VDUQ(2, 5)),a
	ld (VDUQ(3, 5)),a
	ld (VDUQ(4, 5)),a
	push bc
	call SelectPaletteCommand
	pop bc
	djnz -
	ret

; ========================================================================================
; VDU 22                                                                          SET MODE
; ========================================================================================
ModeCommand:
	ld a,(VDUQ(0, 1))
	cp Modes.Count
	ret nc
	jp SetMode

; ========================================================================================
; VDU 23,<command>,<data0>,...,<data7>                                    USER DEFINITIONS
; ========================================================================================
UserCommand:
	ld a,(VDUQ(0, 9))
	
	; Is the command between 0..31?
	cp 32
	ret c
	
	; No, so it's a user-defined character.
	ld hl,VDUQ(1, 9)
	call SetUserDefinedCharacter
	
	ei
	ret


; ========================================================================================
; VDU 24,<left>;<bottom>;<right>;<top>;                              SET GRAPHICS VIEWPORT
; ========================================================================================
GraphicsViewportCommand:
	
	; Bottom left corner:
	ld hl,(VDUQ(2, 8))
	ld bc,(Graphics.OriginY)
	add hl,bc
	ex de,hl
	
	ld hl,(VDUQ(0, 8))
	ld bc,(Graphics.OriginX)
	add hl,bc
	
	call Graphics.TransformPoint
	
	call Graphics.ClampTransformedHLX
	call Graphics.ClampTransformedDEY
	
	ld d,l
	push de
	
	; Top right corner:
	ld hl,(VDUQ(6, 8))
	ld bc,(Graphics.OriginY)
	add hl,bc
	ex de,hl
	
	ld hl,(VDUQ(4, 8))
	ld bc,(Graphics.OriginX)
	add hl,bc
	
	call Graphics.TransformPoint
	
	call Graphics.ClampTransformedHLX
	call Graphics.ClampTransformedDEY
	
	ld d,l
	pop hl
	
	; New graphics viewport is between bottom left (h,l) and top right (d,e).
	
	; Is left edge > right edge?
	ld a,d
	cp h
	jp c,Graphics.ResetViewport
	
	; Is top edge > bottom edge?
	ld a,l
	cp e
	jp c,Graphics.ResetViewport
	
	ld a,h
	ld (Graphics.MinX),a
	ld a,d
	ld (Graphics.MaxX),a
	
	ld a,e
	ld (Graphics.MinY),a
	ld a,l
	ld (Graphics.MaxY),a
	ret

; ========================================================================================
; VDU 25,<command>,<x>;<y>;                                                           PLOT
; ========================================================================================
PlotCommand:
	ld a,(VDUQ(0, 5))
	ld (Graphics.PlotShape),a
	
	ld de,0
	ld b,d \ ld c,e
	bit 2,a
	jr nz,NotPlotBy
	
	; Is it a "PLOT BY"	command?
	; If so, coordinates are relative to the previous one, so adjust.
	
	ld hl,(Graphics.VisitedPoint0X)
	ld de,(Graphics.OriginX)
	or a
	sbc hl,de
	ld d,h
	ld e,l
	
	ld hl,(Graphics.VisitedPoint0Y)
	ld bc,(Graphics.OriginY)
	or a
	sbc hl,bc
	ld b,h
	ld c,l
	
NotPlotBy:
	
	ld hl,(VDUQ(3,5))
	add hl,bc
	push hl
	ld hl,(VDUQ(1,5))
	add hl,de
	pop de
	
	call Graphics.VisitPoint
	call Graphics.Plot
	
	ret

; ========================================================================================
; VDU 26                                                                   RESET VIEWPORTS
; ========================================================================================
ResetViewports:
	call Graphics.Reset
	call Console.Reset
	ret

; ========================================================================================
; VDU 27,<char>                                                     PRINT ESCAPE CHARACTER
; ========================================================================================
EscapeCharCommand:
	ld a,(VDUQ(0,1))
	jp PutLiteralChar

; ========================================================================================
; VDU 28,<left>,<bottom>,<right>,<top>                                   SET TEXT VIEWPORT
; ========================================================================================
ConsoleViewportCommand:

	; What's the maximum width?
	ld a,(Console.MaxWidth)
	ld b,a
	
	; Get left edge.
	ld a,(VDUQ(0,4))
	or a
	jp p,+
	ld a,0
+:	cp b
	jr c,+
	ld a,b
	dec a
+:	ld h,a

	; Get right edge.
	ld a,(VDUQ(2,4))
	or a
	jp p,+
	ld a,0
+:	cp b
	jr c,+
	ld a,b
	dec a
+:	ld d,a
	
	; Is left edge > right edge?
	ld a,d
	cp h
	jp c,Console.ResetViewport
	
	; What's the maximum height?
	ld a,(Console.MaxHeight)
	ld b,a
	
	; Get bottom edge.
	ld a,(VDUQ(1,4))
	or a
	jp p,+
	ld a,0
+:	cp b
	jr c,+
	ld a,b
	dec a
+:	ld l,a

	; Get top edge.
	ld a,(VDUQ(3,4))
	or a
	jp p,+
	ld a,0
+:	cp b
	jr c,+
	ld a,b
	dec a
+:	ld e,a
	
	; Is top edge > bottom edge?
	ld a,l
	cp e
	jp c,Console.ResetViewport
	
	; New text viewport is between bottom left (h,l) and top right (d,e).
	
	ld a,h
	ld (Console.MinCol),a
	ld a,d
	ld (Console.MaxCol),a
	
	ld a,e
	ld (Console.MinRow),a
	ld a,l
	ld (Console.MaxRow),a
	
	; Is the current cursor outside the new viewport?
	; If so, move it back inside.
	
	ld a,(Console.CurCol)
	cp h
	jr nc,+
	ld a,h
+:	cp d
	jr c,+
	ld a,h
+:	ld (Console.CurCol),a

	ld a,(Console.CurRow)
	cp e
	jr nc,+
	ld a,e
+:	cp l
	jr c,+
	ld a,e
+:	ld (Console.CurRow),a
	
	ret

; ========================================================================================
; VDU 29,<x>;<y>;                                                      SET GRAPHICS ORIGIN
; ========================================================================================
SetOriginCommand:
	ld hl,VDUQ(0,4)
	ld de,Graphics.OriginX
	ld bc,4
	ldir
	ret

; ========================================================================================
; VDU 30                                                           MOVE CURSOR TO TOP-LEFT
; ========================================================================================

; ========================================================================================
; VDU 31,<X>,<Y>                                                               MOVE CURSOR
; ========================================================================================
TabCommand:
	call Console.FlushPendingScroll

	ld de,(VDUQ(0,2))
	ld hl,(VDUQ(1,2))

	ld a,(Console.MinCol)
	ld d,a
	ld a,(Console.MaxCol)
	sub d
	cp e
	ret c
	
	ld a,(Console.MinRow)
	ld h,a
	ld a,(Console.MaxRow)
	sub h
	cp l
	ret c
	
	ld a,(Console.MinCol)
	add a,e
	ld (Console.CurCol),a
	
	ld a,(Console.MinRow)
	add a,l
	ld (Console.CurRow),a
	
	ret

; ========================================================================================
; VDU 127                                                                           DELETE
; ========================================================================================
Delete:
	call Console.FlushPendingScroll
	call Console.CursorLeft
	ld a,' '
	jp PutMap

; ---------------------------------------------------------
; PutString -> Sends a string to the display.
; ---------------------------------------------------------
; Inputs:   hl = pointer to NUL or CR-terminated string.
; Outputs:  hl = points to next string after terminator.
;           z if NUL-terminated, nz if CR-terminated.
; Destroys: hl, af.
; ---------------------------------------------------------
PutString:
	ld a,(hl)
	inc hl
	or a
	ret z
	
	cp '\r'
	jr nz,+
	
	push hl
	call PutChar
	pop hl
	or '\r'
	ret
	
+:	push hl
	call PutChar
	pop hl
	jr nz,PutString

; ---------------------------------------------------------
; PutHexNybble -> Puts a hex nybble (0..F) on the screen.
; ---------------------------------------------------------
; Inputs:   a = hex nybble (must only be in the range 0..F)
; Outputs:  None.
; Destroys: af.
; ---------------------------------------------------------
PutHexNybble:
	cp 10
	jr c,+
	add a,'A'-10
	jp VDU.PutChar
+:	add a,'0'
	jp VDU.PutChar

; ---------------------------------------------------------
; PutHexByte -> Puts a hex byte (00..FF) on the screen.
; ---------------------------------------------------------
; Inputs:   a = hex byte.
; Outputs:  None.
; Destroys: af.
; ---------------------------------------------------------
PutHexByte:
	push af
	srl a
	srl a
	srl a
	srl a
	call PutHexNybble
	pop af
	and %1111
	jr PutHexNybble

; ---------------------------------------------------------
; PutHexWord -> Puts a hex byte (0000..FFFF) on the screen.
; ---------------------------------------------------------
; Inputs:   hl = hex byte.
; Outputs:  None.
; Destroys: af.
; ---------------------------------------------------------
PutHexWord:
	push hl
	ld a,h
	call PutHexByte
	pop hl
	ld a,l
	jr PutHexByte


DefaultClear:

	ld a,(Console.MinRow)
	ld c,a
	ld a,(Console.MaxRow)
	sub c
	inc a
	ld c,a

	ld a,(Console.MinRow)
	ld (Console.CurRow),a

--:	ld a,(Console.MinCol)
	ld b,a
	ld a,(Console.MaxCol)
	sub b
	inc a
	ld b,a
	
	ld a,(Console.MinCol)
	ld (Console.CurCol),a
	
-:	push bc
	ld a,' '
	call PutMap
	ld a,(Console.CurCol)
	inc a
	ld (Console.CurCol),a
	pop bc
	
	djnz -
	
	ld a,(Console.CurRow)
	inc a
	ld (Console.CurRow),a
	
	dec c
	jr nz,--
	
	call Console.HomeUp
	
	ret

DefaultResetConsoleViewport:
	
	xor a
	ld (Console.MinRow),a
	ld a,23
	ld (Console.MaxRow),a
	inc a
	ld (Console.MaxHeight),a
	
	ld a,2
	ld (Console.MinCol),a
	ld a,29
	ld (Console.MaxCol),a
	ld a,32
	ld (Console.MaxWidth),a
	
	ret
	

.endmodule