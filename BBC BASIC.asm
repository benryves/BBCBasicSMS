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

.if $ > $4100 \ .echoln "BBC BASIC is not located at $4100!" \ .endif
.org $4100
.incbin "BBCBASIC.COM"
.echoln strformat("BBC BASIC 3.00 - © R.T.Russell 1987 ({0} bytes).", $-$4100)
.endmodule