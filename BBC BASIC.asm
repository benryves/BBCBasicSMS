;------------------------------------------------------------------------------- 
;@doc:file
; 
; === BBC BASIC.asm ===
;
;   Includes the BBC BASIC binary and defines several useful constants.
;
;@doc:end
;------------------------------------------------------------------------------- 
.module Basic

.incsym "BBCBASIC.SYM", "BBCBASIC_"

/*	ADVAL 	EQU	4000H	*/ jp Host.ADVAL
/*	CLG   	EQU	4003H	*/ jp Host.CLG
/*	CLRSCN	EQU	4006H	*/ jp Host.CLRSCN
/*	COLOUR	EQU	4009H	*/ jp Host.COLOUR
/*	DRAW  	EQU	400CH	*/ jp Host.DRAW
/*	ENVEL 	EQU	400FH	*/ jp Host.ENVEL
/*	GCOL  	EQU	4012H	*/ jp Host.GCOL
/*	GETCSR	EQU	4015H	*/ jp Host.GETCSR
/*	GETEXT	EQU	4018H	*/ jp Host.GETEXT
/*	GETIME	EQU	401BH	*/ jp Host.GETIME
/*	GETIMS	EQU	401EH	*/ jp Host.GETIMS
/*	GETPTR	EQU	4021H	*/ jp Host.GETPTR
/*	LTRAP 	EQU	4024H	*/ jp Host.LTRAP
/*	MODE  	EQU	4027H	*/ jp Host.MODE
/*	MOVE  	EQU	402AH	*/ jp Host.MOVE
/*	OSBGET	EQU	402DH	*/ jp Host.OSBGET
/*	OSBPUT	EQU	4030H	*/ jp Host.OSBPUT
/*	OSCALL	EQU	4033H	*/ jp Host.OSCALL
/*	OSCLI 	EQU	4036H	*/ jp Host.OSCLI
/*	OSINIT	EQU	4039H	*/ jp Host.OSINIT
/*	OSKEY 	EQU	403CH	*/ jp Host.OSKEY
/*	OSLINE	EQU	403FH	*/ jp Host.OSLINE
/*	OSLOAD	EQU	4042H	*/ jp Host.OSLOAD
/*	OSOPEN	EQU	4045H	*/ jp Host.OSOPEN
/*	OSRDCH	EQU	4048H	*/ jp Host.OSRDCH
/*	OSSAVE	EQU	404BH	*/ jp Host.OSSAVE
/*	OSSHUT	EQU	404EH	*/ jp Host.OSSHUT
/*	OSSTAT	EQU	4051H	*/ jp Host.OSSTAT
/*	OSWRCH	EQU	4054H	*/ jp Host.OSWRCH
/*	PLOT  	EQU	4057H	*/ jp Host.PLOT
/*	POINT 	EQU	405AH	*/ jp Host.POINT
/*	PROMPT	EQU	405DH	*/ jp Host.PROMPT
/*	PUTCSR	EQU	4060H	*/ jp Host.PUTCSR
/*	PUTIME	EQU	4063H	*/ jp Host.PUTIME
/*	PUTIMS	EQU	4066H	*/ jp Host.PUTIMS
/*	PUTPTR	EQU	4069H	*/ jp Host.PUTPTR
/*	RESET 	EQU	406CH	*/ jp Host.RESET
/*	SOUND 	EQU	406FH	*/ jp Host.SOUND
/*	TRAP  	EQU	4072H	*/ jp Host.TRAP

; &4080
.org $4080
	jp BBCBASIC_EXTERR
; &4087
.org $4087
	jp Basic.BBCBASIC_FPP
; &FFC9  UKCMD     LD A,3           Unknown command
.org $40C9
	ld a,3
; &FFCB  OSFSC     JP FSC           Filing System Control
.org $40CB
	jp Host.OSFSC
; &FFCE  OSFIND    JP FIND          Open or close a file
.org $40CE
	jp Host.OSFIND
; &FFD1  OSGBPB    JP GBPB          Multiple byte file access
.org $40D1
	jp Host.OSGBPB
; &FFD4  OSBPUT    JP BPUT          Put a byte to a file
.org $40D4
	jp Host.OSBPUT
; &FFD7  OSBGET    JP BGET          Get a byte from a file
.org $40D7
	jp Host.OSBGET
; &FFDA  OSARGS    JP ARGS          Read or set file arguments
.org $40DA
	jp Host.OSARGS
; &FFDD  OSFILE    JP FILE          Load or save file, HL+0/1=>filename
.org $40DD
	jp Host.OSFILE
; &FFE0  OSRDCH    JP RDCH          Input a character
.org $40E0
	jp Host.OSRDCH
; &FFE3  OSASCI    CP 13            Print a character, with CR converted to LF,CR
.org $40E3
	cp 13
	jr nz,$40EE
; &FFE7  OSNEWL    LD A,10          Print a LF,CR sequence
.org $40E7
	ld a,10
	call Host.OSWRCH
	ld a,13
; &FFEE  OSWRCH    JP WRCH          Print a character
.org $40EE
	jp Host.OSWRCH
; &FFF1  OSWORD    JP WORD          Do an OSWORD call, HL=>control block
.org $40F1
	jp Host.OSWORD
; &FFF4  OSBYTE    JP BYTE          Do an OSBYTE call
.org $40F4
	jp Host.OSBYTE
; &FFF7  OSCLI     JP CLI           Interpret a command
.org $40F7
	jp Host.OSCLI

.if $ > $4100 \ .echoln "BBC BASIC is not located at $4100!" \ .endif
.org $4100
.incbin "BBCBASIC.COM"
.echoln strformat("BBC BASIC 3.00 - © R.T.Russell 1987 ({0} bytes).", $-$4100)
.endmodule