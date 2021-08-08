.module TextDoubleHeight

Functions:
	.db Function.Initialise \ .dw Initialise
	.db Function.PutMap \ .dw PutMap
	.db Function.Scroll \ .dw Text.Scroll
	.db Function.ResetConsoleViewport \ .dw Text.ResetConsoleViewport
	.db Function.PreserveUnderCursor \ .dw Text.PreserveUnderCursor
	.db Function.RestoreUnderCursor \ .dw Text.RestoreUnderCursor
	.db Function.SelectPalette \ .dw Text.SelectPalette
	.db Function.SelectDefaultPalette \ .dw Text.SelectDefaultPalette
	.db Function.End

NameTable = $3800

Initialise:
	
	; Use the regular TEXT mode as an inspiration.
	call Text.Initialise
	
	; We'll need to undo those lovely inverted characters, though.
	ld hl,96*8
	call Video.SetWriteAddress

	ld c,95 ; Our biggest pattern index so far is 95.
	ld de,VDU.Fonts.Font6x8 ; We'll be stretching out this font.
	ld hl,DoubleHeightTiles ; Here's our lookup table for which tiles represent which parts of the double-height characters
	ld b,96*2 ; 96 character halves to stretch!

LoadPatternLoop:

	; Does the double-height tile we're interested in require a new pattern?
	ld a,c
	cp (hl)
	jr nc,UsesExistingPattern

CreateStretchedPattern:
	; Stretch out the font data to double height.
	ld c,(hl) ; This is our new biggest tile number.
	push bc
	ld b,4	
-:	ld a,(de)
	out (Video.Data),a
	out (Video.Data),a
	inc de
	djnz -
	pop bc
	jr SelectedPattern

UsesExistingPattern:
	; Skip over the font data, as it's covered elsewhere.
	inc de
	inc de
	inc de
	inc de
	
SelectedPattern:
	inc hl	
	djnz LoadPatternLoop
	
	; Now we'll need a couple of dummy characters for the double-height off/on codes:
	xor a	
	ld b,16
-:	out (Video.Data),a
	djnz -
	
	; Done!
	ret

PutMap:
	push hl
	push de
	push bc
	
	cp 140
	jr z,DoubleHeightCode
	cp 141
	jr z,DoubleHeightCode
	
	push af
	call GetDoubleHeightStatus
	jr z,PutMapSingleHeight

PutMapDoubleHeight:	
	pop af
	
	bit 0,c
	jr nz,+

	call GetTopHalfIndex
	jr GotPatternIndex

+:	call GetBottomHalfIndex
	jr GotPatternIndex

PutMapSingleHeight:
	pop af
	add a,FontCharOffset
	cp 96
	jr c,+
	xor a
+:	jr GotPatternIndex

DoubleHeightCode:

	add a,DoubleHeightOff-140

GotPatternIndex:

	push af
	call Text.GetNameTableAddressForCursor
	call Video.SetWriteAddress
	pop af
	
	out (Video.Data),a
	pop bc
	pop de
	pop hl
	ei
	ret

; ---------------------------------------------------------
; GetDoubleHeightStatus -> Gets whether the area under the
; current cursor is double-height or not.
; ---------------------------------------------------------
; Inputs:   (CurCol), (CurRow).
; Outputs:  z = zero if not double height, nz if it is.
;           c = number of rows above the current row that
;               are also double-height.
; Destroys: af, bc, de, hl.
; ---------------------------------------------------------
GetDoubleHeightStatus:
	call GetDoubleHeightStatusForSingleRow
	ret z
	
	; Don't lose the current row!
	ld a,(Console.CurRow)
	push af
	
	; Count the number of rows above that are also double height.
	ld c,0

CountRowsAboveDoubleHeight:	
	ld a,(Console.CurRow)
	or a
	jr z,RowAboveNotDoubleHeight
	dec a
	ld (Console.CurRow),a
	
	push bc
	call GetDoubleHeightStatusForSingleRow
	pop bc
	jr z,RowAboveNotDoubleHeight
	inc c
	jr nz,CountRowsAboveDoubleHeight

RowAboveNotDoubleHeight:

	pop af
	ld (Console.CurRow),a
	ret

; ---------------------------------------------------------
; GetDoubleHeightStatusForSingleRow -> Gets whether the
; area under the current cursor is double-height or not,
; ignoring the rows above.
; ---------------------------------------------------------
; Inputs:   (CurCol), (CurRow).
; Outputs:  z = zero if not double height, nz if it is.
; Destroys: af, bc, de, hl.
; ---------------------------------------------------------
GetDoubleHeightStatusForSingleRow:
	; Read back from the start of this line. Is it a double-heighter?
	ld a,(Console.CurCol)
	or a
	jr z,NotDoubleHeight
	
	push af
	xor a
	ld (Console.CurCol),a
	call Text.GetNameTableAddressForCursor
	call Video.SetReadAddress
	pop af
	ld (Console.CurCol),a
	
	; Now we need to read each character, one by one, to check the double height state.
	ld b,a ; Column count.
	ld c,0 ; Assume no double height by default.
	
-:	in a,(Video.Data)  ; 11
	cp DoubleHeightOff ; 7
	jr nz,+            ; 12/7
	ld c,0             ; 7
+:	cp DoubleHeightOn  ; 7
	jr nz,+            ; 12/7
	ld c,1             ; 7
+:	djnz -             ; 13/8 <- 62 in worst case
	
	ld a,c
	or a
	jr z,NotDoubleHeight
NotDoubleHeight:
	ret

; ---------------------------------------------------------
; GetTopHalfIndex -> Gets the pattern index for an ASCII
; character's top half.
; ---------------------------------------------------------
; Inputs:   a = character number.
; Outputs:  a = pattern index.
; Destroys: f.
; ---------------------------------------------------------
GetTopHalfIndex:
	push de
	add a,FontCharOffset
	cp 96
	jr c,+
	xor a
+:	add a,a
	ld e,a
	ld d,0
	jr GetHalfIndex

; ---------------------------------------------------------
; GetBottomHalfIndex -> Gets the pattern index for an ASCII
; character's bottom half.
; ---------------------------------------------------------
; Inputs:   a = character number.
; Outputs:  a = pattern index.
; Destroys: f.
; ---------------------------------------------------------
GetBottomHalfIndex:
	push de
	add a,FontCharOffset
	cp 96
	jr c,+
	xor a
+:	add a,a
	ld e,a
	ld d,0
	inc de
GetHalfIndex:
	push hl
	ld hl,DoubleHeightTiles
	add hl,de
	ld a,(hl)
	pop hl
	pop de
	ret

DoubleHeightTiles:
	.db 0 ; Existing tile for top of character ' ' ($20)
	.db 0 ; Existing tile for bottom of character ' ' ($20)
	.db 96 ; New tile for top of character '!' ($21)
	.db 97 ; New tile for bottom of character '!' ($21)
	.db 98 ; New tile for top of character '"' ($22)
	.db 0 ; Existing tile for bottom of character '"' ($22)
	.db 99 ; New tile for top of character '#' ($23)
	.db 100 ; New tile for bottom of character '#' ($23)
	.db 101 ; New tile for top of character '$' ($24)
	.db 102 ; New tile for bottom of character '$' ($24)
	.db 103 ; New tile for top of character '%' ($25)
	.db 104 ; New tile for bottom of character '%' ($25)
	.db 105 ; New tile for top of character '&' ($26)
	.db 106 ; New tile for bottom of character '&' ($26)
	.db 107 ; New tile for top of character ''' ($27)
	.db 0 ; Existing tile for bottom of character ''' ($27)
	.db 108 ; New tile for top of character '(' ($28)
	.db 109 ; New tile for bottom of character '(' ($28)
	.db 110 ; New tile for top of character ')' ($29)
	.db 111 ; New tile for bottom of character ')' ($29)
	.db 112 ; New tile for top of character '*' ($2A)
	.db 113 ; New tile for bottom of character '*' ($2A)
	.db 114 ; New tile for top of character '+' ($2B)
	.db 115 ; New tile for bottom of character '+' ($2B)
	.db 0 ; Existing tile for top of character ',' ($2C)
	.db 116 ; New tile for bottom of character ',' ($2C)
	.db 117 ; New tile for top of character '-' ($2D)
	.db 0 ; Existing tile for bottom of character '-' ($2D)
	.db 0 ; Existing tile for top of character '.' ($2E)
	.db 118 ; New tile for bottom of character '.' ($2E)
	.db 119 ; New tile for top of character '/' ($2F)
	.db 120 ; New tile for bottom of character '/' ($2F)
	.db 121 ; New tile for top of character '0' ($30)
	.db 122 ; New tile for bottom of character '0' ($30)
	.db 123 ; New tile for top of character '1' ($31)
	.db 124 ; New tile for bottom of character '1' ($31)
	.db 125 ; New tile for top of character '2' ($32)
	.db 126 ; New tile for bottom of character '2' ($32)
	.db 127 ; New tile for top of character '3' ($33)
	.db 128 ; New tile for bottom of character '3' ($33)
	.db 129 ; New tile for top of character '4' ($34)
	.db 130 ; New tile for bottom of character '4' ($34)
	.db 131 ; New tile for top of character '5' ($35)
	.db 128 ; Existing tile for bottom of character '5' ($35)
	.db 132 ; New tile for top of character '6' ($36)
	.db 133 ; New tile for bottom of character '6' ($36)
	.db 134 ; New tile for top of character '7' ($37)
	.db 135 ; New tile for bottom of character '7' ($37)
	.db 136 ; New tile for top of character '8' ($38)
	.db 133 ; Existing tile for bottom of character '8' ($38)
	.db 137 ; New tile for top of character '9' ($39)
	.db 138 ; New tile for bottom of character '9' ($39)
	.db 118 ; Existing tile for top of character ':' ($3A)
	.db 139 ; New tile for bottom of character ':' ($3A)
	.db 118 ; Existing tile for top of character ';' ($3B)
	.db 116 ; Existing tile for bottom of character ';' ($3B)
	.db 140 ; New tile for top of character '<' ($3C)
	.db 109 ; Existing tile for bottom of character '<' ($3C)
	.db 141 ; New tile for top of character '=' ($3D)
	.db 142 ; New tile for bottom of character '=' ($3D)
	.db 143 ; New tile for top of character '>' ($3E)
	.db 111 ; Existing tile for bottom of character '>' ($3E)
	.db 125 ; Existing tile for top of character '?' ($3F)
	.db 144 ; New tile for bottom of character '?' ($3F)
	.db 145 ; New tile for top of character '@' ($40)
	.db 146 ; New tile for bottom of character '@' ($40)
	.db 147 ; New tile for top of character 'A' ($41)
	.db 148 ; New tile for bottom of character 'A' ($41)
	.db 149 ; New tile for top of character 'B' ($42)
	.db 150 ; New tile for bottom of character 'B' ($42)
	.db 151 ; New tile for top of character 'C' ($43)
	.db 152 ; New tile for bottom of character 'C' ($43)
	.db 153 ; New tile for top of character 'D' ($44)
	.db 150 ; Existing tile for bottom of character 'D' ($44)
	.db 154 ; New tile for top of character 'E' ($45)
	.db 155 ; New tile for bottom of character 'E' ($45)
	.db 154 ; Existing tile for top of character 'F' ($46)
	.db 156 ; New tile for bottom of character 'F' ($46)
	.db 157 ; New tile for top of character 'G' ($47)
	.db 158 ; New tile for bottom of character 'G' ($47)
	.db 159 ; New tile for top of character 'H' ($48)
	.db 148 ; Existing tile for bottom of character 'H' ($48)
	.db 160 ; New tile for top of character 'I' ($49)
	.db 124 ; Existing tile for bottom of character 'I' ($49)
	.db 161 ; New tile for top of character 'J' ($4A)
	.db 162 ; New tile for bottom of character 'J' ($4A)
	.db 163 ; New tile for top of character 'K' ($4B)
	.db 164 ; New tile for bottom of character 'K' ($4B)
	.db 165 ; New tile for top of character 'L' ($4C)
	.db 155 ; Existing tile for bottom of character 'L' ($4C)
	.db 166 ; New tile for top of character 'M' ($4D)
	.db 148 ; Existing tile for bottom of character 'M' ($4D)
	.db 167 ; New tile for top of character 'N' ($4E)
	.db 168 ; New tile for bottom of character 'N' ($4E)
	.db 169 ; New tile for top of character 'O' ($4F)
	.db 133 ; Existing tile for bottom of character 'O' ($4F)
	.db 149 ; Existing tile for top of character 'P' ($50)
	.db 156 ; Existing tile for bottom of character 'P' ($50)
	.db 169 ; Existing tile for top of character 'Q' ($51)
	.db 106 ; Existing tile for bottom of character 'Q' ($51)
	.db 149 ; Existing tile for top of character 'R' ($52)
	.db 164 ; Existing tile for bottom of character 'R' ($52)
	.db 170 ; New tile for top of character 'S' ($53)
	.db 171 ; New tile for bottom of character 'S' ($53)
	.db 172 ; New tile for top of character 'T' ($54)
	.db 107 ; Existing tile for bottom of character 'T' ($54)
	.db 173 ; New tile for top of character 'U' ($55)
	.db 133 ; Existing tile for bottom of character 'U' ($55)
	.db 173 ; Existing tile for top of character 'V' ($56)
	.db 174 ; New tile for bottom of character 'V' ($56)
	.db 175 ; New tile for top of character 'W' ($57)
	.db 176 ; New tile for bottom of character 'W' ($57)
	.db 177 ; New tile for top of character 'X' ($58)
	.db 178 ; New tile for bottom of character 'X' ($58)
	.db 179 ; New tile for top of character 'Y' ($59)
	.db 107 ; Existing tile for bottom of character 'Y' ($59)
	.db 134 ; Existing tile for top of character 'Z' ($5A)
	.db 180 ; New tile for bottom of character 'Z' ($5A)
	.db 181 ; New tile for top of character '[' ($5B)
	.db 182 ; New tile for bottom of character '[' ($5B)
	.db 183 ; New tile for top of character '\' ($5C)
	.db 184 ; New tile for bottom of character '\' ($5C)
	.db 185 ; New tile for top of character ']' ($5D)
	.db 186 ; New tile for bottom of character ']' ($5D)
	.db 187 ; New tile for top of character '^' ($5E)
	.db 0 ; Existing tile for bottom of character '^' ($5E)
	.db 0 ; Existing tile for top of character '_' ($5F)
	.db 141 ; Existing tile for bottom of character '_' ($5F)
	.db 188 ; New tile for top of character '`' ($60)
	.db 189 ; New tile for bottom of character '`' ($60)
	.db 190 ; New tile for top of character 'a' ($61)
	.db 191 ; New tile for bottom of character 'a' ($61)
	.db 192 ; New tile for top of character 'b' ($62)
	.db 150 ; Existing tile for bottom of character 'b' ($62)
	.db 193 ; New tile for top of character 'c' ($63)
	.db 152 ; Existing tile for bottom of character 'c' ($63)
	.db 194 ; New tile for top of character 'd' ($64)
	.db 158 ; Existing tile for bottom of character 'd' ($64)
	.db 195 ; New tile for top of character 'e' ($65)
	.db 196 ; New tile for bottom of character 'e' ($65)
	.db 188 ; Existing tile for top of character 'f' ($66)
	.db 135 ; Existing tile for bottom of character 'f' ($66)
	.db 197 ; New tile for top of character 'g' ($67)
	.db 198 ; New tile for bottom of character 'g' ($67)
	.db 192 ; Existing tile for top of character 'h' ($68)
	.db 148 ; Existing tile for bottom of character 'h' ($68)
	.db 199 ; New tile for top of character 'i' ($69)
	.db 124 ; Existing tile for bottom of character 'i' ($69)
	.db 200 ; New tile for top of character 'j' ($6A)
	.db 162 ; Existing tile for bottom of character 'j' ($6A)
	.db 201 ; New tile for top of character 'k' ($6B)
	.db 202 ; New tile for bottom of character 'k' ($6B)
	.db 185 ; Existing tile for top of character 'l' ($6C)
	.db 124 ; Existing tile for bottom of character 'l' ($6C)
	.db 203 ; New tile for top of character 'm' ($6D)
	.db 204 ; New tile for bottom of character 'm' ($6D)
	.db 205 ; New tile for top of character 'n' ($6E)
	.db 148 ; Existing tile for bottom of character 'n' ($6E)
	.db 195 ; Existing tile for top of character 'o' ($6F)
	.db 133 ; Existing tile for bottom of character 'o' ($6F)
	.db 206 ; New tile for top of character 'p' ($70)
	.db 207 ; New tile for bottom of character 'p' ($70)
	.db 208 ; New tile for top of character 'q' ($71)
	.db 209 ; New tile for bottom of character 'q' ($71)
	.db 205 ; Existing tile for top of character 'r' ($72)
	.db 156 ; Existing tile for bottom of character 'r' ($72)
	.db 193 ; Existing tile for top of character 's' ($73)
	.db 210 ; New tile for bottom of character 's' ($73)
	.db 211 ; New tile for top of character 't' ($74)
	.db 212 ; New tile for bottom of character 't' ($74)
	.db 213 ; New tile for top of character 'u' ($75)
	.db 214 ; New tile for bottom of character 'u' ($75)
	.db 213 ; Existing tile for top of character 'v' ($76)
	.db 215 ; New tile for bottom of character 'v' ($76)
	.db 213 ; Existing tile for top of character 'w' ($77)
	.db 176 ; Existing tile for bottom of character 'w' ($77)
	.db 216 ; New tile for top of character 'x' ($78)
	.db 187 ; Existing tile for bottom of character 'x' ($78)
	.db 213 ; Existing tile for top of character 'y' ($79)
	.db 198 ; Existing tile for bottom of character 'y' ($79)
	.db 217 ; New tile for top of character 'z' ($7A)
	.db 126 ; Existing tile for bottom of character 'z' ($7A)
	.db 218 ; New tile for top of character '{' ($7B)
	.db 219 ; New tile for bottom of character '{' ($7B)
	.db 96 ; Existing tile for top of character '|' ($7C)
	.db 107 ; Existing tile for bottom of character '|' ($7C)
	.db 220 ; New tile for top of character '}' ($7D)
	.db 221 ; New tile for bottom of character '}' ($7D)
	.db 222 ; New tile for top of character '~' ($7E)
	.db 0 ; Existing tile for bottom of character '~' ($7E)
	.db 223 ; New tile for top of character '⌂' ($7F)
	.db 224 ; New tile for bottom of character '⌂' ($7F)

DoubleHeightOff = 225
DoubleHeightOn =  226

.endmodule