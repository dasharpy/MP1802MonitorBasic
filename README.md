MP1802MonitorBasic [a continuous work-in-progress] -
assembly monitor and minimilistic Basic style run-time interpreter for RCA 1802 cpu
using A18 1802 cross-assembler version 2.5
targeting to fit on 27128 EPROM
testing with EMMA 02 emulator



MPMonitor:

Assembled by Mark Phillips - 2024
; (audio cassette functions derived from 1978 Popular Electronics COSMAC Elf)

; MP1802Monitor is a resurrection of my original monitor developed around 1980 for RCA 1802 COSMAC Elf
; based micro (Popular Electronics 1976).  Recently, I rebuilt a minimalistic version of that micro
; and modified my original monitor program to go with it.  My original micro had a keyboard and video board,
; and ability to load and save programs via audio cassette.  The cassette functions are included in this
; listing, but not tested.  Timing of those functions need to be adjusted for cpu clock speed.

; Monitor commands are:
;	? - Display help menu
; E - Edit Memory - Specify start location, ESC or CR to exit, BS to overwrite previous char entered.
;	D - Display Memory - displays to 256 byte boundary and pauses, CR to continue, ESC to exit
;	X - Display Register status stored at entry to monitor
;	B - Move block of memory from one location to another
;	F - Fill block of memory with specified byte
;	G - Go to memory location and continue program execution.
;	U - Execute at memory location as via subroutine call and return via $D5
;	L - Load a binary file to memory via terminal application. Specify memory location to load to,
; 		followed by number of bytes to load, and waits for terminal program to send binary.
;	S - Send block of bytes (binary) to terminal application. Specify start and end address,
;		  wait for key press from terminal program to begin sending bytes. Used Tera Term
;		  record binary log function to collect bytes.

;	R - Load data from audio cassette (CLOAD - this hasn't been tested, uses EF2)
;	W - Write data to audio cassette (CSAVE - this hasn't been tested)
;	    audio cassette uses Kansas City Modulation: mark = 8 cycles @ 2400Hz, a space = 4 cycles @ 1200Hz

; For this iteration, just using serial IO to connect to PC terminal program (Tera Term VT)
; via a USB-TTL module which uses Q and EF3 for TX and RX.  The USB-TTL module uses high
; levels on TX/RX during inactive state.  As such, inverted-Q used for TX.  That way, my
; Q-led would be off during inactive communications.  The serial communications routines
; have fixed timing to match a 1.8432 MHz 1802 clock rate.  Serial communications are
; set to 2400 baud, 1 start bit, 8 data bits, 1 stop bit, no handshake control, 25msec/char transmit delay.

; Hardware memory configuration is Monitor in EPROM at $0000 and
; 32K RAM at $8000


 
MP1802Basic -
assembled by Mark Phillips - 1980 - 2024

; MP1802Basic is a run-time interpretive language styled after BASIC for the RCA 1802 microprocessor.  It was originally
; developed in early 1980's, and was recently resurrected in assembly form for A18 assembler.
; At that time, the program interfaced with a keyboard and TTL based video board connected to TV.
; In this current resurrection, I/O to the 1802 is handled via a serial terminal emulation application using
; a USB module connected to Q and EF3 to communicate.  The particular USB module I am using has
; the TXD and RXD connections transmitting as active low. 

; In general, MP1802Basic (MPB) has two over-arching sections.  One is the BASIC text editing section and
; the other is the runtime interpretive section.  The program starts in the text editing mode
; allowing for the input of a Basic program.  The editing section is a derivative of machine code taken
; from the 1802 Pilot application (R.W. Petty, Kilobaud Microcomputing, pg.78, July,1979).  
; Roughly using a modified version of original Pilot code from $0000 to $03D8 of published listing.

Single letter program text edit commands...most taken from 1802 Pilot:
;	[note: if nnn in following commands is omitted, then 1 is assumed]
;	B (Beginning) - move text pointer to first line in program. 
;	C (Clear) - clear all program text by moving $03 etx marker byte to beginning of text.
;	Dnnn (Down) - move text pointer down nnn lines from current position.
;   E (End) - move text pointer to last program line
;	I (Insert) - insert text after 'I' at current line pointer...usage exmpl: ILET A=10
;	Knnn (Kill) - deletes nnn lines at current text line
;	L (Load) - inputs text from cassette (uses CLOAD in monitor code, but timing not set)
;	M (Monitor) - exits text editor and returns to monitor entry point
;	R (Run) - begins program execution
;	S (Save) - saves text to cassette (uses CSAVE in monitor code, but timing not set)
;	Unnn (Up) -	move current text line pointer up nnn lines
;	Wnnn (Write) - write nnn lines of text to screen
;	Z (edit current line) - displays current text line and allows use of backspace to delete
;							chars and enter new ones [be careful on keyboard chars used, does
;							not respond properly to left/right arrows, etc.]

Program statement commands: [note: for many commands, just first 3 letters are needed...saved storage
;					space in earlier micro configurations]
;	LET - assign values to variables: LET A=5, A$="TEST", A4=123+4
;		(LET command is not required for variable assignment as is implied)
;	PRI(NT) - display variable value: PRI A, A$; A4 ... use comma seperator for
;			tabbed spacing, colon for single space
;	GOS(UB) - go to subroutine line number: GOS 100
;	RET(URN) - return from subroutine
;	GOT(O) - goto line number: GOT 100
;	REM(ARK) - remark statement: REM text
;	END - end of program
;	FOR - for/next statement: FOR I=1 TO 10...NEXT I
;	NEX(T) - ""
;	IF - if/then statement: IF A=5 THEN 100
;	ON - on given variable value goto equivalent indexed line number:
;		ON J GOTO 100,200,300,400,500...if J=3 then goes to line 300
;	INP(UT) - wait for keyboard input as number or string for respective variable:
;		INP A waits for number (123), INP A$ waits for text enclosed by quotes ("TEST")
;	CLS - clear screen and set cursor to upper left corner on VT100 terminal 
;	IPT - input byte from 1802 port INPUTPORT to variable: IPT A
;	OPT - output byte to 1802 port OUTPUTPORT: OPT &FF
;	SET - uses VT100 ESC commands to plot ascii character at specified screen position:
;		SET [x int val], [y int val], [ascii char as decimal], ie. SET 10, 15, 65 to plot 'A' at x=10, y=15
;		[Use CLS statement before plotting, and use a waiting loop with INKEY statement after plot generated]
;	REA(D) - read data sets to variable:  REA A(K)
;	DAT(A) -  numeric or string data values for read statement. Current data value
;			 is pointed to by DATAPOINTER parameter:  DAT 1,5,3,4,...
;	RES(TORE) - resets DATAPOINTER to beginning of program [TODO: consider using line number for reset]
;	DIM(ENSION) - dimension array (only numeric for now):  DIM A(10) .. DIM K(2,255)
;		[work-in-progress on another version attempting to include dimensioned string arrays...have to overcome bugs]
;	STO(P) - stops program execution by issuing error code $FF
;	CAL(L) - call machine code at given address: CALL (decimal high address),(decimal low address)
;	FIX - fix number of digits to right of decimal:  FIX 3
;	INKEY - waits for single char input from serial input and stores at KEYVAL param
;	POKE -  store byte(s) to machine address: POKE &A000, &7A, &12,... [takes decimal or hexadecimal values]
;	PEEK -  retrieve byte at specified address (decimal or hexadecimal) and assign to variable


