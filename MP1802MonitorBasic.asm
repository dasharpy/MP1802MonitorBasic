; Assembled by Mark Phillips - 2024
; (audio cassette functions derived from 1978 Popular Electronics COSMAC Elf)
; using A18 1802 cross-assembler version 2.5

; MP1802Monitor is a resurrection of my original monitor developed around 1980 for RCA 1802 COSMAC Elf
; based micro (Popular Electronics 1976).  Recently, I rebuilt a minimalistic version of that micro
; and modified my original monitor program to go with it.  My original micro had a keyboard and video board,
; and ability to load and save programs via audio cassette.  The cassette functions are included in this
; listing, but not tested.  Timing of those functions need to be adjusted for cpu clock speed.

; Monitor commands are:
;	? - Display help menu
; 	E - Edit Memory - Specify start location, ESC or CR to exit, BS to overwrite previous char entered.
;	D - Display Memory - displays to 256 byte boundary and pauses, CR to continue, ESC to exit
;	X - Display Register status stored at entry to monitor
;	B - Move block of memory from one location to another
;	F - Fill block of memory with specified byte
;	G - Go to memory location and continue program execution.
;	U - Execute at memory location as via subroutine call and return via $D5
;	L - Load a binary file to memory via terminal application. Specify memory location to load to,
;		followed by number of bytes to load, and waits for terminal program to send binary.
;	S - Send block of bytes (binary) to terminal application. Specify start and end address,
;		wait for key press from terminal program to begin sending bytes. Used Tera Term
;		record binary log function to collect bytes.

;	R - Load data from audio cassette (CLOAD - this hasn't been tested, uses EF2)
;	W - Write data to audio cassette (CSAVE - this hasn't been tested)
;	audio cassette uses Kansas City Modulation: mark = 8 cycles @ 2400Hz, a space = 4 cycles @ 1200Hz

; For this iteration, just using serial IO to connect to PC terminal program (Tera Term VT)
; via a USB-TTL module which uses Q and EF3 for TX and RX.  The USB-TTL module uses high
; levels on TX/RX during inactive state.  As such, inverted-Q used for TX.  That way, my
; Q-led would be off during inactive communications.  The serial communications routines
; have fixed timing to match a 1.8432 MHz 1802 clock rate.  Serial communications are
; set to 2400 baud, 1 start bit, 8 data bits, 1 stop bit, no handshake control, 25msec/char transmit delay.

; Hardware memory configuration is Monitor in EPROM at $0000 and
; 32K RAM at $8000

;-------------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------------
; Principal MP1802Monitor Routines:
;	SERIALSND - sends one byte to serial comm
;	SERIALRCV - receives one byte from serial comm
;	CHARO - sends string of ASCII bytes to serial terminal. string ends with $00
;	A2H - Converts a serial input ASCII value to hexadecimal
;	H2A - Converts byte to 2 char ASCII hexadecimal for output to terminal
;	VADJ - facilitates display of address for listing of memory bytes.
;	LDBFILE - send binary file from pc terminal program to 1802 memory:
;	SDBFILE - send block of bytes from mem to serial terminal app
;	BLKMOVE - move block of memory from one location to another
;	GOSUB - make a call to subroutine in memory using D5 to return
;	GOTO - goto a memory location
;	FILLMEM - fill memory section with specified byte
;	MEMDSPLY - display block of memory
;	MEMEDIT - edit memory location
;	REGSTAT - display register status stored at entry into MPMonitor
;	CSAVE - save to audio cassette (not recently tested, used in original KVOS code)
;	CLOAD - load from audio cassette (not recently tested, used in original KVOS code)

; following line used to remove assembly of cassette functions in bin file if not wanted.
; if excluding functions, then need to adjust command loop check branching
CASSETTEFUNCTIONS EQU 1 ;use with IF/ENDI psuedo ops:  0=do not compile  1=compile


		CPU 1802
R0		EQU	0
R1		EQU	1	;REGSTAT
R2		EQU	2	;CALL,RTRN,CHARO,BLKMOVE
R3		EQU	3	;CALL,RTRN
R4		EQU	4
R5		EQU	5
R6		EQU	6	;CALL
R7		EQU	7	;CHARO,H2A,A2H
R8		EQU	8	;GET_BEG_END_ADDR,BLKMOVE
R9		EQU	9	;VADJ,GET_BEG_END_ADDR,BLKMOVE
R10		EQU	10	;BLKMOVE
R11		EQU	11
R12		EQU	12
R13		EQU	13
R14		EQU	14	;H2A,A2H
R15		EQU	15	;CALL,RTRN,H2A,BLKMOVE

;-------------------------------------------------------------------------------------------------------------
; MP1802Monitor needs approx 36 bytes RAM for register storage/status
; at entry, and some additional storage for CALL/RETURN pointers.
; 32kx8 RAM @ $8000-$FFFF
MEMSTACK EQU $FFFF ;top of memory stack in RAM

;Monitor EPROM base address = $0000
BASE EQU 0000H
	ORG BASE

;---- Monitor Entry
	LDI	HIGH(MEMSTACK)  
	PHI	R2
	LDI	LOW(MEMSTACK)
	PLO	R2
; (X,P)>T;(X,P)>M(R(2));THEN P>X;R(2)-1
	MARK
	SEX	R2
; use shift left with carry to move DF  into byte $18
; result will be $30 w/DF-0, or $31 w/DF=1
; store result to mem stack
	LDI	$18
	SHLC
	STXD 
; store IE state: $30 w/IE=0 or $31 w/IE=1		
	LDI	$31	
	LSIE
	SMI	$01
	STXD
; storing all register values to memory stack
	GLO	R0
	STXD
	GHI	R0
	STXD
	GLO	R1
	STXD
	GHI	R1
	STXD
	GLO	R2
	STXD
	GHI	R2
	STXD
	GLO	R3
	STXD
	GHI	R3
	STXD
	GLO	R4
	STXD
	GHI	R4
	STXD
	GLO	R5
	STXD
	GHI	R5
	STXD
	GLO	R6
	STXD
	GHI	R6
	STXD
	GLO	R7
	STXD
	GHI	R7
	STXD
	GLO	R8
	STXD
	GHI	R8
	STXD
	GLO	R9
	STXD
	GHI	R9
	STXD
	GLO	R10
	STXD
	GHI	R10
	STXD
	GLO	R11
	STXD
	GHI	R11
	STXD
	GLO	R12
	STXD
	GHI	R12
	STXD
	GLO	R13
	STXD
	GHI	R13
	STXD
	GLO	R14
	STXD
	GHI	R14
	STXD
	GLO	R15
	STXD
	GHI	R15
	STXD
	;at this point, 32 bytes of mem stack used to store registers + 3 bytes for IE,DF,MARK
	LDI	HIGH(ENTRY3)
	PHI	R3
	LDI	LOW(ENTRY3)
	PLO	R3
	SEP	R3
ENTRY3:
	LDI HIGH(CALL1)
	PHI R4
	LDI LOW(CALL1)
	PLO R4
	LDI HIGH(RTRN1) ;new addition 4/7
	PHI R5
	LDI LOW(RTRN1);$E4
	PLO R5;
ENTRY4:
	CALL CHARO 	;command prompt
	BYTE '\r\n\n>MP?',$00
ENTRY5:
	CALL TERMRCV	;waiting for command character

;-------------------------------------------------------------------------------
;---- Display Help Menu -------------------------------------------------------- 
;-------------------------------------------------------------------------------
	GHI	R15 ; ??is this needed if D passed via RTRN
	XRI	'?' ;display this menu help 
	LBNZ GOTO 
	CALL CHARO
	BYTE '\r\n'
	BYTE 'E-MEM.EDIT\r\n'
	BYTE 'D-MEM.DISP.\r\n'
	BYTE 'X-REG.STAT.\r\n'
	BYTE 'B-BLOCK MOVE\r\n'
	BYTE 'F-FILL MEM\r\n'
	BYTE 'G-GOTO\r\n'
	BYTE 'U-GOSUB\r\n'
	BYTE 'L-LD BFILE\r\n'
	BYTE 'S-SND BYTES\r\n'
	BYTE 'R-CLOAD\r\n'
	BYTE 'W-CSAVE\r\n'
	BYTE 'P-MPBASIC'
	;BYTE 'I-SET DMA&INT\r\n' ;not implemented yet??
	BYTE $00
	LBR	ENTRY4

;-------------------------------------------------------------------------------
;---- GOTO - continue program execution at given address -----------------------
;-------------------------------------------------------------------------------
	;goto routine is entered with R3 as program pointer.
	;get target address and store in R8
	;make R9 the program pointer so that the
	;target address can be loaded to R3 and then
	;make R3 the program pointer again
GOTO:
	GHI	R15
	XRI	'G'
	LBNZ GOSUB
	CALL CHARO
	BYTE '\r\nGOTO $',$00
	CALL A2H
	PHI	R8
	CALL A2H
	PLO	R8
	CALL SERIALRCV
	XRI	$1B
	LBZ	ENTRY4
	LDI	HIGH(GOTO1)
	PHI	R9
	LDI	LOW(GOTO1)
	PLO	R9
	SEP	R9
GOTO1:
	GHI	R8
	PHI	R3
	GLO	R8
	PLO	R3
	SEP	R3

;-------------------------------------------------------------------------------
;---- GOSUB - call subroutine that returns via $D5 -----------------------------
;-------------------------------------------------------------------------------
GOSUB:
	GHI	R15
	XRI	'U'
	LBNZ BLKMOVE
	CALL CHARO
	BYTE '\r\nGOSUB $',$00
	CALL A2H
	PHI	R8
	CALL A2H
	PLO	R8
GOSUB1:
	CALL SERIALRCV
	XRI	$1B
	LBZ	ENTRY4
	LDI	HIGH(GOSUB2)
	PHI	R9
	LDI	LOW(GOSUB2)
	PLO	R9
	SEP	R9
GOSUB2:
	GHI	R8
	PHI	R3
	GLO	R8
	PLO	R3
	LDI	HIGH(ENTRY4)
	STXD
	LDI	HIGH(ENTRY4)
	STXD
	SEP	R3

;-------------------------------------------------------------------------------
;---- BLOCK MOVE ---------------------------------------------------------------
;-------------------------------------------------------------------------------
BLKMOVE:
	GHI	R15
	XRI	'B'
	LBNZ FILLMEM
	CALL CHARO
	BYTE '\r\nBLK MOVE\r\n\n',$00
	CALL GET_BEG_END_ADDR
	CALL CHARO
	BYTE '\r\nNEW ADDR$',$00
	CALL A2H
	PHI	R10
	CALL A2H
	PLO	R10
	CALL SERIALRCV
	XRI $1B ;ESC
	LBZ	ENTRY4
	GLO	R8
	STR	R2
	GLO	R10
	SM
	GHI	R8
	STR	R2
	GHI	R10
	SMB
	LBDF BLKMOVE2
BLKMOVE1:
	LDA	R8
	STR	R10
	INC	R10
	GHI	R8
	STR	R2
	GHI	R9
	XOR
	BNZ	BLKMOVE1
	GLO	R8
	STR	R2
	GLO	R9
	XOR
	BNZ	BLKMOVE1
	LDN	R8
	STR	R10
	BR	BLKMOVE4
BLKMOVE2:
	GLO	R8
	STR	R2
	GLO	R9
	SM
	STXD
	GHI	R8
	STR	R2
	GHI	R9
	SMB
	STR	R2
	INC	R2
	GLO	R10
	ADD
	PLO	R10
	DEC	R2
	GHI	R10
	ADC
	PHI	R10
	INC	R2
BLKMOVE3:
	LDN	R9
	DEC	R9
	STR	R10
	DEC	R10
	GHI	R9
	STR	R2
	GHI	R8
	XOR
	BNZ	BLKMOVE3
	GLO	R9
	STR	R2
	GLO	R8
	XOR
	BNZ	BLKMOVE3
	LDN	R9
	STR	R10
BLKMOVE4:
	LBR	ENTRY4
	
;-------------------------------------------------------------------------------
;---- FILL MEMORY --------------------------------------------------------------
;-------------------------------------------------------------------------------
FILLMEM:
	GHI	R15
	XRI	'F'
	LBNZ MEMDSPLY
	CALL CHARO
	BYTE '\r\nFILL MEM\r\n\n',$00
	CALL GET_BEG_END_ADDR
	CALL CHARO
	BYTE '\r\nHEX# ?',$00
	CALL A2H
	PLO	R10
	CALL SERIALRCV
	XRI	$1B
	LBZ	ENTRY4
	SKP
FILLMEM1:
	INC	R8
	GLO	R10
	STR	R8
	GLO	R8
	STR	R2
	GLO	R9
	XOR
	BNZ	FILLMEM1
	GHI	R8
	STR	R2
	GHI	R9
	XOR
	BNZ	FILLMEM1
	LBR	ENTRY4 ;LFABA
	
;-------------------------------------------------------------------------------
;---- MEMORY DISPLAY - display mem at specified address
;---- in table format with line address and 16 values per line
;---- keypress needed to display next line, ESC key to end displaying
;-------------------------------------------------------------------------------
MEMDSPLY:	
	GHI	R15
	XRI	'D'
	LBNZ MEMEDIT
	CALL CHARO
	BYTE '\r\nMDISP $',$00
	
	CALL A2H
	PHI	R9
	CALL A2H
	PLO	R9	;R9 contains start address of mem to display
	
MEMDSPLY1:
	CALL SERIALRCV 	;key press to start listing
	XRI $1B ;ESC to exit listing
	LBZ	ENTRY4
	CALL CHARO
	BYTE '\r\n',$00
MEMDSPLY2:
	CALL VADJ	;call routine to initialize horizontal table display of address and 16 values per line
	LDA	R9		;get mem value and advance R9 pointer
	CALL H2A	;send to H2A to display hex as 2char ascii 
	GLO	R9		;get R9.0	
	LBZ	MEMDSPLY1	;if $00, then wait for key press to display next line
	LBR	MEMDSPLY2	;if not, then display value with table adjust
	
;-------------------------------------------------------------------------------
;---- MEMORY EDIT - enter address where to begin editing byte by byte
;-------------------------------------------------------------------------------
MEMEDIT:
	GHI	R15
	XRI	'E'
	LBNZ REGSTAT 
	CALL CHARO
	BYTE '\r\nMEDIT? $',$00
	
	;Perform 2 successive calls to A2H to get High and Low edit address bytes stored to R9
	;2023 Note: although A2H can return CR($0D) as control char, I am not using it in the generation of mem edit address!
MEMED0:
	;get high byte of address
	CALL A2H
	GLO R15 ;retrieve possible control char, otherwise $00
	XRI $08 ;check if BS control char rcvd. If yes, then start address entry again
	LBZ MEMED0
	GLO R15
	XRI $1B ;check if ESC recvd. If yes, then exit mem edit routine
	LBZ ENTRY4
	GHI R15
	PHI	R9
	
	;get low byte of mem edit address
	CALL A2H
	GLO R15 ;retrieve possible control char, otherwise $00
	XRI $08 ;check if BS rcvd, then start low address entry again
	LBNZ MEMED2
	CALL CHARO ;move term screen cursor back 2 spaces
	BYTE $08, $08, $00
	LBR MEMED0
MEMED2:
	GLO R15
	XRI $1B ;check if ESC recvd, then exit mem edit routine
	LBZ ENTRY4
	GHI R15
	PLO	R9
	
	;Move terminal screen cursor to next line for byte entry
	CALL CHARO
	BYTE '\r\n',$00

	;Step 1 - display line address with VADJ
	;step 2 - get byte for storage at R9 using A2H
	;????step 3 - then wait for keypress to determine intent:
	;--- CR = enter value to R9 mem and advance.  
	;--- BS = reenter value
	;--- ESC = same as CR
MEMED4:	
	CALL VADJ

	;get byte to enter into current mem location at R9 pointer
	;check if byte is control char before storing to mem location
MEMED6:
	CALL A2H ;routine returns valid hex val in R15.0, and D (also R15.1) contains control byte, $00 control byte is valid hex chars
	
	;check if BS control rcvd, meaning want to delete previous mem byte entry...note: a BS received during second char entry in A2H is handled in that routine
	;this would have been returned for first char entry in A2H.
	;BS on 2nd char in A2H would have been handled by A2H and not returned.
	GLO R15
	XRI $08
	LBNZ MEMED8
	;BS received - adjust screen, decrement R9, and go back and get new byte
	
	CALL CHARO ;move screen cursor back 3 spaces (this includes 2 hex chars and one space char between entries)
	BYTE $08, $08, $08, $00
	DEC R9
	LBR MEMED6
	
MEMED8:
	GLO R15
	XRI $1B ;check if ESC recvd, then exit mem edit 
	LBZ ENTRY4
	GLO R15
	XRI $0D ;check if CR recvd, then exit mem edit
	LBZ ENTRY4
	
	;save mem edit byte to R9
	GHI R15
	STR	R9
	INC	R9
	LBR MEMED4


;-------------------------------------------------------------------------------
;---- Display register values stored at entry into monitor ---------------------
;-------------------------------------------------------------------------------
	; note: issue with using R2 mem stack pointer to retrieve stored
	; bytes and not overwriting them so that values could be displayed
	; again while still in the monitor.  CALLS and RETN from display
	; routines are overwriting stored register values.  Need to use
	; different register to access the stored register values.
	; Use R1 to retrieve reg values and not affect R2 stack pointer.	
REGSTAT:
	GHI	R15
	XRI	'X'
	LBNZ LDBFILE ;SETDI
	CALL CHARO
	BYTE '\r\n',$00
	;BYTE '\r\nREG STAT\r\n\n',$00
	; set R1 to point to MARk saved byte as starting point for displaying values
	; in lower to higher memory format
	LDI HIGH(MEMSTACK)
	PHI R1
	LDI LOW(MEMSTACK)
	SMI $22 ;subtract 35 bytes
	PLO R1
	; begin display of address, label, and value for stored registers
	; new addition - display address first for each stored register
	; address display can be used to modify register value later
	;---------------------------------------------------------------
	; R8.0 is counter to loop through display of registers
	LDI	$10 ; 16 registers saved
	PLO	R8
	; R9 points to char label for each register
	LDI	HIGH(REGSTAT3)
	PHI	R9
	LDI	LOW(REGSTAT3)
	PLO	R9
	;---------------------------------------------------------------
	; loop thru and display 16 general purpose registers
REGSTAT1:
	CALL LBLADDR
	; output label
	LDI	'R'
	CALL SERIALSND
	LDA	R9
	CALL SERIALSND
	LDI	':'
	CALL SERIALSND
	; get register value from mem stack via R1 and display
	LDA	R1
	CALL H2A
	LDA	R1
	CALL H2A
	CALL CHARO
	BYTE '\r\n',$00
	; decrement register count and loop again if not zero
	DEC	R8
	GLO	R8
	LBNZ REGSTAT1
	;---------------------------------------------------------------
	; display IE state
	CALL LBLADDR
	CALL CHARO
	BYTE 'IE:',$00
	LDA	R1 ;this val either $30 or $31
	CALL SERIALSND
	CALL CHARO
	BYTE '\r\n',$00
	;---------------------------------------------------------------
	; display DF state
	CALL LBLADDR
	CALL CHARO
	BYTE 'DF:',$00
	LDA	R1 ;this val either $30 or $31
	CALL SERIALSND
	CALL CHARO
	BYTE '\r\n',$00
	;---------------------------------------------------------------
	; display P byte as number which is lower 4 bits of stored byte
	CALL LBLADDR
	CALL CHARO
	BYTE 'P:',$00
	LDN	R1
	ORI	$30 ; OR-ing with $30 to make ascii number of 4 LSBits
	CALL SERIALSND
	CALL CHARO
	BYTE '\r\n',$00
	;---------------------------------------------------------------
	; display X byte as number which is upper 4 bits of store byte
	CALL LBLADDR
	CALL CHARO
	BYTE 'X:',$00
	LDA R1
	SHR
	SHR
	SHR
	SHR
	ORI	$30 ; OR-ing with $30 to make ascii number of 4 MSBits
	CALL SERIALSND
	CALL CHARO
	BYTE '\r\n',$00
	;--------------------------------------------------------------
	; return to menu
	LBR	ENTRY4
REGSTAT3:
	BYTE 'FEDCBA9876543210'
	
;--------------------------------------------------------------------------------
;---- Load Binary File from Terminal App ----------------------------------------
;--------------------------------------------------------------------------------
	;---- Using to send a 'bin' file from pc to 1802 memory.
	;---- Works with TeraTerm Send File function (check box binary)
	;---- to send binary file to another device.
	;---- Enter target address for
	;---- where binary data to be loaded to, and the number
	;---- of bytes to read (hex format), and then wait for TeraTerm
	;---- to initiate sending of data.
LDBFILE:
	GHI R15
	XRI	'L'
	LBNZ SDBFILE
	CALL CHARO
	BYTE '\r\nLOAD BIN FILE\r\n'
	BYTE 'LOAD TO $',$00
	
	CALL A2H
	PHI	R9
	CALL A2H
	PLO	R9 ;R9 = load to address
	
	CALL CHARO
	BYTE '\r\nNUM BYTES $',$00
	CALL A2H
	PHI	R8
	CALL A2H
	PLO	R8 ;R8 = number of bytes to read
	
	;waiting for a byte to be transmitted from term 
	CALL WAITKEYLBL
LDBFILE1:
	CALL SERIALRCV ;using char transmission delay 25msec in Tera Term
	STR R9
	INC R9
	
	STR R2 ;store to mem stack for led display
	OUT 4 ; M(R(X))->BUS, R(X)+1
	DEC R2

	;check if specified bytes have been received
	DEC R8
	GHI R8
	LBNZ LDBFILE1
	GLO R8
	LBNZ LDBFILE1
	LBR ENTRY4
	
	
;--------------------------------------------------------------------------------
;--- Send Memory Bytes to Terminal App
;--------------------------------------------------------------------------------
SDBFILE:
	GHI	R15
	XRI	'S' ;$57
	;If CASSETTEFUNCTIONS ;if not assembling cassette functions, then use different branch
	;LBNZ CLOAD
	;ELSE
	;LBNZ ENTRY4 ;end of command check loop, no valid command char received
	;ENDI
	LBNZ GOBASIC
	CALL CHARO
	BYTE '\r\nSEND BIN\r\n\n',$00
	CALL GET_BEG_END_ADDR ;beginning address in R8, ending address in R9

	;wait for keypress to begin sending data
	CALL WAITKEYLBL
	;check for esc key to exit routine
	CALL SERIALRCV
	XRI $1B ;ESC key
	LBZ	ENTRY4

	;send bytes beginning at R8 address
SDBFILE1:
	LDA R8
	CALL SERIALSND
	;check if R8 address equals R9 address
	GHI	R8
	STR	R2
	GHI	R9
	XOR
	LBNZ SDBFILE1
	GLO	R8
	STR	R2
	GLO	R9
	XOR
	LBNZ SDBFILE1

	;R8 equals R9, send last byte at R8
	LDN R8
	CALL SERIALSND
	;done - display command prompt
	LBR ENTRY4

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; on P, goto start address for MPBasic
GOBASIC:
	GHI	R15
	XRI	'P'
	If CASSETTEFUNCTIONS ;if not assembling cassette functions, then use different branch
	LBNZ CLOAD
	ELSE
	LBNZ ENTRY4 ;end of command check loop, no valid command char received
	ENDI
	LBR L0003 ;goto start of mpbasic



;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; display R1 as address for register retrieval
LBLADDR:
	LDI	'$'
	CALL SERIALSND
	GHI R1
	CALL H2A
	GLO R1
	CALL H2A
	LDI ' '
	CALL SERIALSND
	RETN


;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;---- GET_BEG_END_ADDR
;---- get a beginning mem addr to R8 and ending mem addr to R9
GET_BEG_END_ADDR:
	CALL CHARO
	BYTE 'BEG ADDR$',$00
	CALL A2H
	PHI	R8
	CALL A2H
	PLO	R8
	CALL CHARO
	BYTE '\r\nEND ADDR$',$00
	CALL A2H
	PHI	R9
	CALL A2H
	PLO	R9
	RETN
	
	
	
;-------------------------------------------------------------------------------
;---- wait for keypress to begin sending data ----------------------------------
WAITKEYLBL:
	CALL CHARO
	BYTE '\r\nWAIT4KEYPRESS',$00
	RETN


;---------------------------------------------------
;---- CALL Subroutine 'D4 $xxxx' - D passed via RF.1 and D?
;---- 1)R3 was pgm pointer when D4 executed, and D4 points to CALL1
;---- 2)CALL addr pointed to by R3 retrieved and set to R6
;---- 3)R3 now holds return address and is stored to MEM stack (pointed to by R2)
;---- 4)R6 val transferred to R3 (CALL addr), and program execution continues from there.
;---- sufficient MEM stack (R2) space is needed to store return address for each CALL
CALL0:
	GHI	R15
	SEP	R3
CALL1:
	PHI	R15
	SEX	R2
	LDA	R3
	PHI	R6
	LDA	R3
	PLO	R6
	GHI	R3
	STXD
	GLO	R3
	STXD
	GHI	R6
	PHI	R3
	GLO	R6
	PLO	R3
	LBR	CALL0

;---------------------------------------------------
;---- RETURN 'D5' - D passed via RF.1 and D? 
RTRN0:
	GHI	R15
	SEP	R3
RTRN1:
	PHI	R15
	SEX	R2
	IRX
	LDXA
	PLO	R3
	LDX
	PHI	R3
	LBR	RTRN0


;-----------------------------------------
;---- TERMRCV -routine to receive 1 char from
;---- serial terminal and echo back to terminal
TERMRCV:
	CALL SERIALRCV
	CALL SERIALSND
	RETN



;----------------------------------------
;---- CHARO - send character string out to terminal (null terminated)
CHARO:
	IRX
	LDXA
	PLO	R7
	LDX
	PHI	R7
CHARO1:
	LDA	R7
	BZ	CHARO2
	CALL SERIALSND
	BR	CHARO1
CHARO2:
	GHI	R7
	STXD
	GLO	R7
	STXD
	RETN


;----------------------------------------
;--- CVR - command to clear video terminal screen
CVR:
	CALL CHARO
	BYTE $1B,'[2J',$03 ;esc sequence to clear screen for VT100 term
	CALL CHARO
	BYTE $1B,'[0;0H',$03 ;esc sequence to set cursor to 0,0 for VT100 term
	RETN


;-------------------------------------------------
;---- Hex value to 2char ASCII (H-A)
;---- D val passed via CALL in RF.1
H2A:
	LDI 00H
	PLO R14 ;iteration flag
	GHI R15
	PHI	R7
	SHR
	SHR
	SHR
	SHR
HA1:
	PLO	R15
	SDI	$09
	BNF	HA2
	GLO	R15
	ADI	$30
	BR HA3
HA2:
	XRI	$FF
	ADI	$41
HA3:
	CALL SERIALSND
	GLO R14
	LSZ
	RETN
	NOP
	GHI	R7 ; from 1st iteration
	ANI	$0F
	INC R14 ;increment iteration counter
	BR	HA1
	
;-----------------------------------------------	
;ASCII to Hex Value (A-H) - receive 2 chars from term and form one hex byte for return
;SERIALRCV & SERIALSND use R15,R13,R12
;this routine using R15.1 for temp storage of entered char, R14 for iteration counter, R7 temp storage
;using BS to backup and change char, ESC or CR to terminate editing

;2024 mod: R15.1 used to pass data byte, R15.0 used to pass control character
;this is correction for possibly valid hex bytes of $08,$0D,$1B being formed
;and returned to MEMEDIT routine, but being interpreted as control chars.

A2H:
	LDI $00
	PLO R15 ;clear control char byte
	PLO R14 ;iteration flag marking whether getting first or second char of Hex byte
A2H1:
	CALL SERIALRCV	;get byte, should be $30-$39, $41-$46 for valid hex chars
	XRI $08 ;test received char for backspace BS
	LBNZ A2H4 ;if not BS, then go to next check for ESC char
	
	;BS received, check if iteration on 1st or 2nd received char.
	;if BS is first, send it back to calling program for it to handle.
	;if BS is second char, send BS back to terminal to handle
	GLO R14 ;if R14.0 is 0, then 1st char, else 2nd char 
	LBNZ A2H3 ;

A2H2:
	;return BS control byte to calling pgm via R15.0 for it to handle 
	GHI R15
	PLO R15
	RETN

A2H3:
	;iter not zero. receiving 2nd char, therefore, send BS to terminal to overwrite 1st
	GHI R15
	CALL SERIALSND
	LBR A2H ; loop back and get 1st char of hex byte again

A2H4:
	;check for ESC, if so, exit this routine passing back ESC in D to calling pgm for it to handle
	GHI R15
	XRI $1B
	LBZ A2H2 ;received char was ESC, therefore, return ESC to calling program via R15.0 regardless whether 1st or 2nd char
	
	;2023 - new check for CR which, if 1st char, is sent back to calling pgm as control char.
	GHI R15
	XRI $0D
	BNZ A2H5
	;CR received. check if 1st char
	GLO R14
	LBZ A2H2 ;send CR back to calling pgm via R15.0 as control char

A2H5:
	;char not BS, ESC, or CR. Then test for valid hex char $30-$39 or $41-$46
	GHI R15
	SDI $2F ;subract D from I
	NOP
	NOP
	BGE A2HBELL ;char not valid hex key, rcv again (I >= D)
	GHI R15
	SDI $46
	BL A2HBELL ;char not valid hex key, rcv again (I > D)
	GHI R15
	SDI $3A
	BL A2H6 ;valid hex number key, proceed
	GHI R15
	SDI $40
	BL A2HBELL ;char between 39 and 41, rcv again (I < D)
	
	;previous A2H code concentrates on handling control characters received from terminal
	;following section forms an 8-bit byte from the two hex characters received from terminal
	;procedure uses R7 to facilitate the conversion process
A2H6:
	GHI R15
	CALL SERIALSND ;echo D to terminal
	SHL		;shifting to get bit to DF to determine if number or alphabetic
	SHL
	PHI R7	;temp save
	GLO R14
	BNZ A2H7;branch on second iteration (R14.0 = 1)
	GHI R7	;1st iteration	A-F shl-shl would have df=1, 0-9 would have df=0
	LSNF	;skip if 0-9 shift
	ADI	$24	;add $24 if A-F shift
	SHL		;$35 shl-shl = $D4 shl-shl = $50
	SHL		;$41 shl-shl = $04 + $24 = $28 shl-shl = $A0
	PLO	R7	;store 1st iteration result to R7.0
	INC R14 ;increment iteration flag to $01
	LBR A2H1	;branch to get 2nd char for hex value
A2H7:
	GHI R7	;2nd iteration stage
	BNF	A2H8;A-F shlshl would have df=1, 0-9 would have df=0
	ADI	$24			
	LSKP
A2H8: ;same as AH3 in 1st version MPMonitor
	ADI	$40
	SHR		;$41 shlshl + $24 = $28 shrshr = $0A
	SHR		;$35 shlshl = $D4 + $40 = $14 shrshr = $05
	STR	R2	;store 2nd iter result to mem of stack pointer for following add operation
	GLO	R7		
	ADD		;add 1st and 2nd iter results ->D
	RETN
A2HBELL:
	;sound vt100 bell on char entry error and get byte again
	LDI $07
	CALL SERIALSND
	LBR A2H1


;-----------------------------------------------------------
;---- VADJ - display horizontal lines of address and 16 hex values
;---- 0000:vv vv vv vv ->
;---- 0010:vv vv vv vv ->
;---- 0020:vv vv vv vv ->
;---- etc.

VADJ:
	GLO	R9			;R9 points to byte to display
	BZ VADJ1		;if R9.0 = $00, then display line address
	ANI	$0F 		;if R9.0 = $0F, then sent CR/LF to term
	BNZ	VADJ2		;send space to term between bytes
	CALL CHARO
	BYTE '\r\n',$00
VADJ1: ;if R9.0 E.Q. 00 then display address at beginning of line
	GHI	R9
	CALL H2A
	GLO	R9
	CALL H2A
	LDI	':'
	LSKP
VADJ2: ;if R9.0 N.E. 00, then place space between bytes
	LDI	' '
	LBR	SERIALSND ; since this isn't a call, retn in SerialSnd returns this routine


;----------------------------------------
;---- SET DMA & INT	
; 4/19/21 - commented out since this function didn't seem to have
; a viable purpose. also note that I never did develop valid code
; for this menu item!
;SETDI:
;	GHI R15
;	XRI	'I'
;	LBNZ LDBFILE ;ENTRY5 ;LFAC3
;SETDI1:
;	CALL CHARO
;	BYTE '\r\n','S','E','T',' ','D','M','A'
;	BYTE ' ','I','N','T',$0D,$0A
;	BYTE 'R','0',' ','=',' ',$00
;	CALL A2H
;	PHI	R0
;	CALL A2H
;	PLO	R0
;	CALL CHARO
;	BYTE '\r\n','R','1',' ','=',' ',$00
;	CALL A2H
;	PHI	R1
;	CALL A2H
;	PLO	R1
;	CALL SERIALRCV
;	;LDN	R13
;	XRI $1B	;$FB
;	LBZ	ENTRY4 ;LFABA
;	;CALL CVR
;	BR	SETDI1
	
	
;----- TERMINAL SERIAL COMMUNCATIONS	
	;Mark Phillips - March 2021 
	;1802 development/test routine for serial char sending from 1802 to terminal program on external device such as pc/android
	;using USB2TTL module as interface: TX <-> EF3, RX <-> Q
	;1.8432 mhz 1802 clock : 2802 machine cycle (mc) of 8 clk pulses = 4.3403e-6 secs
	;2400 baud rate (1 start bit, 8 data bits, 1 stop bit, no parity)
	;sampling target is mid point of serial bit
	;serial bit width = 4.1667e-4 secs = 96 mc : mid point at 2.0833e-4 secs = 48 mc
	;data bits received in order of LSB to MSB
	;1802 EF states are active 1 (low level) and inactive 0 (high level), so watch those branch instructions 
	
	;Serial send/receive one char for 1802-Pilot
	;at send entry, I/O char to send in D and D is stored in RF.1
	;receive returns I/O char in D and RF.1
	;routine returns with D5
	;modification: used inverted Q to serial com since we want high TX line state when board reset and Q is off

		ORG ($ AND $FFF0)+$10 ;put code on boundary to avoid short branching errors which can affect timing
		
SERIALSND:
		REQ			;set Q to low state (hi TX line level)
		PLO R15		;move D to RF.0 as working byte
	;--------------------------------------------------------
		LDI	08H		;init bit counter 
		PLO	12
	;--------------------------------------------------------
		SEQ			;start bit, set Q hi (low TX) for 96 mc
		LDI	0DH		
SND1	PLO	13		
SND2	DEC	13		
		GLO	13
		BNZ	SND2
		NOP
		NOP
	;--------------------------------------------------------	
SND2.5	GLO 15		;get working byte
		SHR			;shift right LSB to carry flag
		PLO 15		;store working byte
	;--------------------------------------------------------	
		BDF SND3		;branch if DF=1
		SEQ			;DF=0, so set Q hi (low TX)
		BR SND3.5
SND3	REQ			;DF=1, so set Q lo (high TX)
	;--------------------------------------------------------	
SND3.5	LDI	0CH		;hold Q state for 96mc (include processing and bit counter)
		PLO	13		
SND4	DEC	13		
		GLO	13
		BNZ	SND4
		NOP
		NOP
	;--------------------------------------------------------	
		DEC 12		;DECrement bit counter
		GLO	12
		LBNZ	SND2.5	;move to stop bit if 8 data bits received
	;--------------------------------------------------------	
		SEQ			;set Q hi (low TX) for stop bit
		LDI	0EH		
		PLO	13		
SND6	DEC	13		
		GLO	13
		BNZ	SND6
		NOP
		NOP
		REQ			;set Q low (high TX) at end of stop bit
		GHI 15 ;byte to send should have been in R15.1 on routine entry, recalling on exit
		RETN

	;--------------------------------------------------------	
	;--------------------------------------------------------	
	;--------------------------------------------------------	
	;--------------------------------------------------------	

	ORG ($ AND $FFF0)+$10 ;put code on boundary to avoid short branching errors which can affect timing
		
SERIALRCV	REQ			;set TX to high state (Q low) 
	;--------------------------------------------------------	
RCV2	LDI	$00		;init received byte register RF.1
		PHI 15
		LDI	$08		;init bit counter 
		PLO	12
	;--------------------------------------------------------	
RCV3	BN3	RCV3	;wait for EF3 to go low at begin of start bit
	;--------------------------------------------------------	
		LDI $1C		;144 mc (96mc+48mc) delay to middle of first data bit(skipping over start bit)
RCV5	PLO	13		
RCV6	DEC	13		
		GLO	13
		BNZ	RCV6
		NOP
		NOP
	;--------------------------------------------------------	
		BN3	RCV7
		LDI	$00		;low receive bit, setup for 0 to carry flag
		BR	RCV8
RCV7	LDI	$01		;high baud bit, setup for 1 to carry flag
RCV8	SHR			;right shift to DF
		GHI 15		;get work byte to D
		SHRC		;right rotate DF into D 
		PHI 15		;save work byte
	;--------------------------------------------------------	
		DEC	12		;DEC bit counter
		GLO	12
		BZ	RCV9	;move to stop bit if 8 data bits received
	;--------------------------------------------------------	
		LDI	$0A		;since processing bit took time, shorten delay to next bit
		;BR	RCV5	;about 70 mc delay
		LBR	RCV5	;about 70 mc delay
	;--------------------------------------------------------	
RCV9	LDI	$0A		;delay 70 mc to mid of stop bit
		PLO	13		
RCV10	DEC	13		
		GLO	13
		BNZ	RCV10
		NOP
		NOP
	;--------------------------------------------------------	
RCV11	B3 RCV11	;loop on stop bit with ef3 low		
	;--------------------------------------------------------
		GHI 15		;move RF.1 to D for return
		RETN
	


;-----------------------------------------------------
	IF CASSETTEFUNCTIONS

	ORG ($ AND $FFF0)+$10 ;put code on boundary to avoid short branching errors which can affect timing
		
;-------------------------------------
;-------------------------------------

;---- Tape cassette read and write routines use Kansas City Modulation
;----   a mark is 8 cycles at 2400Hz, a space is 4 cycles at 1200Hz

;-------------------------------------
;-------------------------------------
;	IF CLOADFLAG
;---- CLOAD - cassette load routine
;---- routine used EF2 to receive signal from tape
;---- uses Q as flag
;---- registers used: R8,R9,R7,R10,R11

CLOAD:
	GHI	R15
	XRI	'R'
	LBNZ CSAVE ;branch on thru Monitor command check loop if not CLOAD
	CALL CHARO
	BYTE '\r\nCLOAD\r\n\n',$00
	CALL GET_BEG_END_ADDR ;beginning address in R8, ending address in R9
	
	;wait for keypress to begin cassette load
	;check for esc key to exit routine
	CALL SERIALRCV
	XRI $1B ;ESC key
	LBZ	ENTRY4
	
CASSLD1: ;beginning of cassette audio load
	SEX	R8 ;R8 is X
	LDI	$80 ;put one in R7
	PLO	R7 
	LDI	$08 ;shift count in R10
	PLO	R10
	SEQ ;Q on
	LDI	$00 ;clear byte to be loaded via R8
	STR	R8
CASSLD2:
	;loop on marks in tape header or between bytes
	B2	CASSLD2
CASSLD3:
	BN2	CASSLD3
	LDI	$00
CASSLD4:
	ADI	$01
	B2	CASSLD4
	ADI	$DA
	BNF	CASSLD2

	;if space, start 5ms delay
	LDI	$60 ;delay duration time
	PLO	R11
	REQ
CASSLD5:
	;do delay using duration in R11
	GLO	R11
CASSLD6:
	SMI	$01
	NOP
	BNZ	CASSLD6

CASSLD7:
	;determine if mark or space at sampling time
	B2	CASSLD7
CASSLD8:
	BN2	CASSLD8
	LDI	$00
CASSLD9:
	ADI	$01
	B2	CASSLD9
	
	;branch if mark
	ADI	$DA
	BNF	CASSLD10 
	
	;if space, turn Q off, shift right output byte, set delay for 2.5ms
	REQ
	LDX
	SHR
	STR	R8
	LDI	$28
	PLO	R11
	BR	CASSLD11

CASSLD10:
	;if mark, turn Q on, shift right output byte and add 1, set delay for 2.9ms
	SEQ
	LDX
	SHR
	STR	R8
	GLO	R7
	ADD
	STR	R8
	LDI	$52
	PLO	R11
	
CASSLD11:
	;loop until all 8 bits in byte
	GLO	R10
	SMI	$01
	PLO	R10
	BNZ	CASSLD5
	
	;continue until all tape loaded
	OUT	4 ;display byte just loaded??
	SEX	R2
	GHI	R8
	STR	R2
	GHI	R9
	XOR
	BNZ	CASSLD12
	GLO	R8
	STR	R2
	GLO	R9
	XOR
	LBZ	ENTRY4
	LBR	CASSLD1
CASSLD12:
	NOP
	NOP
	NOP
	LBR	CASSLD1
	
;-----------------------------------------------------

	ORG ($ AND $FFF0)+$10 ;put code on boundary to avoid short branching errors which can affect timing
		
;----------------------------------
;--- CSAVE - cassette save routine
CSAVE:
	GHI	R15
	XRI	'W'
	LBNZ ENTRY4 ;end of command check loop, no valid command char received
	CALL CHARO
	BYTE '\r\nCSAVE\r\n\n',$00
	CALL GET_BEG_END_ADDR ;beginning address in R8, ending address in R9

	;wait for keypress to begin cassette write
	CALL WAITKEYLBL
	CALL SERIALRCV
	;check for ESC to exit routine
	XRI $1B
	LBZ	ENTRY4

CASSSV1:  ;start of cassette audio write
	SEX R2
	REQ
	LDI	$00
	PLO	R11 ;clear R11
	PLO	R10	;clear R10 which is end-of-byte flag
	
	;generate a mark, 8 cycles at 2400Hz
CASSSV2:
	LDI	$08 
CASSSV3:
	PLO	R12 ;cycle count
	LDI	$01 ;$0F ;delay for frequency...needed $01 for cpu clock of 1.83mhz
	PLO	R13	;
	BR	CASSSV4

CASSSV4:
	;if Q on, turn it off and vice versa
	BQ	CASSSV5
	SEQ
	BR	CASSSV6
CASSSV5:
	REQ
	BR	CASSSV6

	;variable delay to balance half cycles
CASSSV6:
	GLO	R13
CASSSV7:
	SMI	$01
	BNZ	CASSSV7
	
	;repeat if cycle count not zero
	GLO	R12
	SMI	$01
	PLO	R12
	BZ	CASSSV10
	
	;fixed delay
	LDI	$07
CASSSV8:
	SMI	$01
	BNZ	CASSSV8
	BR	CASSSV9
CASSSV9:
	BR	CASSSV4

CASSSV10:
	;end of mark test
	GLO	R11
	BNZ	CASSSV12
	
	BN4 CASSSV2 ;wait for input button press
	INC	R11
CASSSV11:
	;SEX R8
	;start by getting byte
	;LDX
	LDN R8
	PLO	R7
	;set shift count to R15
	LDI	$09
	PLO	R15
	;going to space
	BR	CASSSV13

CASSSV12:
	;branch if end of byte
	GLO	R10
	LBNZ	CASSSV15

	;if not, reduce shift count and branch if end of byte
	GLO	R15
	SMI	$01
	PLO	R15
	LBZ	CASSSV14
	
	;goto mark or space according to bit of byte
	GLO	R7
	SHRC
	PLO	R7
	BDF	CASSSV2
	
CASSSV13:
	;generate a space 4 cycles at 1200Hz
	LDI	$04
	PLO	R12
	LDI	$08 ;$28 controls space frequency
	PLO	R13
	LBR	CASSSV4
	
	;at end of byte, set eob switch and start double mark
CASSSV14:
	LDI	$01
	PLO	R10
	LDI	$10
	LBR	CASSSV3
	
	;when end of byte, display byte and if end of core, go to mark
	;if not, get next byte and return
	;2024 mods: changed output sequence from previous code as wanted last byte sent to be M(R(9))
CASSSV15:
	LDI	$00
	PLO	R10
	LDN R8
	STR R2
	OUT	4 ;X=2, OUT M(R(X)), R(X)+1
	DEC R2
	;SEX R2
	GHI	R8
	STR	R2
	GHI	R9
	XOR
	LBNZ CASSSV16
	GLO	R8
	STR	R2
	GLO	R9
	XOR
	LBZ ENTRY4 
	;LBR CASSSV11
CASSSV16:
	NOP ;don't know if NOP here for timing???
	NOP
	NOP
	INC R8
	LBR	CASSSV11

;=============
	ENDI ;end of IF CASSETTEFUNCTIONS psuedo op






;################################################################################
;################################################################################
;################################################################################
;################################################################################
;################################################################################
;################################################################################

; MP1802Basic by Mark Phillips - development period: 1980 - 2024

; MP1802Basic is a run-time interpretive language styled after BASIC for the RCA 1802 microprocessor.  It was originally
; developed in early 1980's, and was recently resurrected in assembly form for A18 assembler.
; At that time, the program interfaced with a keyboard and TTL based video board connected to TV.
; In this current resurrection, I/O to the 1802 is handled via a serial terminal emulation application using
; a USB module connected to Q and EF3 to communicate.  The particular USB module I am using has
; the TXD and RXD connections transmitting as active low. 

; In general, MP1802Basic (MPB) has two overarching sections.  One is the BASIC text editing section and
; the other is the runtime interpretive section.  The program starts in the text editing mode
; allowing for the input of a Basic program.  The editing section is a derivative of binary code taken
; from the 1802 Pilot application (R.W. Petty, Kilobaud Microcomputing, pg.78, July,1979).  
; Roughly using a modified version of original Pilot code from $0000 to $03D8 of published listing.
; See EDIT_CMD_TBL for possible editing commands. The edit command 'R' begins interpretive execution of Basic program.00

; A line of Basic style code is terminated with either CR or :
; Line numbers are only used as line labels for GOTO or GOSUB commands and can be in any order.
; If non-space alphabetic char is encountered it is evaluated as to whether it is a variable or beginning of BASIC command
; BASIC commands just need first 3 chars of command.
; Numeric variable names are restricted to two chars - first is alpha char and optional second char is number - such as B, A1, or K9.
; Dimensioned numeric variables are single alpha char - A(10), L(20)
; String variables are single alpha char followed by '$' char - A$

; The BASIC text is stored beginning at PGM_START memory location.  It is preceded by $02 byte
; and is terminated by $03 byte (this format was based on 1802 Pilot).

; EXECUTE function is called to initiate the running interpretation of program code.
;	  It initializes several counters such as FOR/NEXT sub-levels,
;	  and CALL Subroutine statement sub-levels.  Routine also monitors for low on 1802 EF4 pin
;	  connected to input button, which terminates Basic Pgm execution.

; Interpreter handles floating point calculations.  There is no provision for integer
; arithmetic.

; A fundamental component of this interpreter is a section called the 
; arithmetic operating system (AOS).  An arithmetic stack (ASTCK) is used to hold intermediate values
; during evaluation of an arithmetic expression. During evaluation, as operands are encountered,
; they are assigned; an hierarchy value byte (i.e. 01,02,03,04) depending on priority.
; The operand execution address and associated hierarchy values are stored in a separate
; memory stack (BASICMEMSTACK) from ASTCK for comparison to successive operands encountered.
; Operand execution sequence is determined based on hierarchy values.
 
; Function sequence for handling numbers encountered in program arithmetic expression:
;
;	 1)GETNUM routine extracts a number from program text and transfers to scratch memory
;	   where ASCII chars are stripped to just the right 4 bits to form decimal number that
;	   is left justified in scratch memory and limited to 8 significant digits.
;	   [2024 mod - added code to handle hex value which is specified by preceding '&' char]
;
;	 2)NORM function normalizes number in scratch memory to eliminate any preceding zero digits in a
;      non-zero decimal number and exponent is adjusted accordingly. [??have to give this some more
;	   thought, but does this function need to be performed? If not, this could speed things up.]
; 
;	 3)PACK function compresses 8-digit decimal in scratch memory into 4 bytes with two decimal
;	   values per byte in preparation for storage in the arithmetic stack.
;
;	 4)COPY_TO_ARITHSTACK function copies the packed number to next slot in the arithmetic stack
;	   for usage in arithmetic operations.
;
;	 5)Converse of some above functions are UNPACK and FSTCK (copy from arithmetic stack)
;
;	 ASTCK stands for arithmetic stack.  Holds numbers encountered in arithmetic expression
;		evaluation. During evaluation, as operands are encountered, they are assigned
;		an hierarchy value byte (i.e. 01,02,03,04) depending on priority. The operand execution
;		address and associated hierarchy values are stored in separate memory stack from ASTCK
;		for comparison to successive operands encountered.  Operand execution sequence is 
;		determined based on hierarchy values.
;

;----------------------------------------------------------------
;----------------------------------------------------------------
; Variable values are stored in memory after program text and is terminated
; by $04 byte.  Variables use following structures:
;
; Types of variables:
;	Scalar Numeric - A, A5
;	Scalar String  - A$, A5$
;	Array Numeric  - A(10), A(2,255), A(I), A(J,K)

; Variable memory format:
;	Scalar Numeric - Exmpl A  = 41 00 xx xx xx xx xx xx (1st byte:41-5A, 2nd byte:00,30-39), 3rd byte sign, 4th byte exponent, remaining bytes hold number as 4 bits per number sequential as byte high, then byte low, and repeated for 4 bytes
;					 >>	for A=12345678, sign=$00, exponent=$08, remaining 4 bytes: $12,$34,$56,$78 (number is limited to 4 bytes, but exponent can get larger depending on number entered.
;	                 Exmpl B5 = 42 35 xx xx xx xx xx xx (SHL of 1st byte during pgm execution results in DF=0 as test for numeric scalar)

;	Scalar String  - Exmpl A$ = 82 3F F. P. xxxxxxxxxxx 00 (1st byte:82-B4, 2nd byte:00,30-39)
;	                 Exmpl B5$= 84 45 F. P. xxxxxxxxxxx 00 (1st byte is SHL of ascii char, sets byte up for SHL to DF during execution as test)

;	Array Number   - Exmpl A()= 00 41 F. P. Col. xxxxxxxxxxx (2nd byte 41-5A)



;----------------------------------------------------------------


BASICBASE	EQU ($ AND $FF00)+$100
BASICMEMBASE EQU $8000

BASICBASEHIGH	EQU HIGH(BASICBASE) ;not being used anywhere meaningful
		ORG	BASICBASE

		CPU 1802
		
R0		EQU	0
R1		EQU	1
R2		EQU	2
R3		EQU	3
R4		EQU	4
R5		EQU	5
R6		EQU	6
R7		EQU	7
R8		EQU	8
R9		EQU	9
R10		EQU	10
R11		EQU	11
R12		EQU	12
R13		EQU	13
R14		EQU	14
R15		EQU	15

OUTPUTPORT EQU 6 ;2024 addition for OPT command to output byte to 1802 IO port
INPUTPORT EQU 6 ;2024 addition for IPT command to input byte from 1802 IO port

;following register assignments were generally taken from 1802 Pilot
;R0 = scratch pad register
;R1 = scratch pad register
;R2 = memory stack pointer
;R3 = program pointer
;R4 = call program counter
;R5 = return program counter
;R6 = holds return address of in-line argument
;R7 = save/restore register
;R8 = scratch pad register
;R9 = general purpose counter
;RA = BASIC text pointer
;RB = line pointer
;RC = MPBasic: used as arithmetic stack pointer
;RD = MPBasic: scratch register, assists arith stack maneuvers
;RE = MPBasic: scratch register
;RF.1 = used to facilitate passing D val in calls and returns

;MP1802Basic memory assignments (relative to BASICMEMBASE):
;	$0000-$005F > Arithmetic operations stack and scratch memory - INIT_ARITH_STACK_POINTER sets $xx5F
;	$0060-$00FF > FOR/NEXT nesting stack
;	      $0100 > Error code
;	      $0101 > 
;	      $0102 > Subroutine count
;	      $0103 > End of program text pointer address (high byte), also used to point to start of program variable table
;	      $0104 > End of program text pointer address (low byte)
;	      $0105 > Variable pointer
;	      $0106 > "
;	      $0107 > FOR/NEXT nesting stack pointer
;	      $0108 > "
;	      $0109 > End of variable storage address (high byte) [added 2024 to assist in variable storage deletions]
;	      $010A > End of variable storage address (low byte
;	      $010B > Current DATA statement pointer (high byte)
;	      $010C > Current DATA statement pointer (low byte)
;	      $010D > 
;	      $010E > FIX number of decimal numbers (check PILOT for how it handled)
;	      $010F > KEY value ???
;	      $0110 > Text pointer (main)
;	      $0111 > 
;	$0112-$013E > Text pointers (subcount 1 to 24)
;	      $0160 > Beginning of input buffer
;         $01FF > End of machine stack, R2 is initialized to this location
;	$0141-$0144 > Random number seed

;	$0200 > Beginning of program text

BASICMEMSTACK EQU BASICMEMBASE+$01FF ; top of memory stack used for text editing functions

SCRATCHMEMB EQU BASICMEMBASE+$0000 	;keep low address range on 0x00 boundary due to GETNUM requirements
ASTCKMEM EQU BASICMEMBASE+$005F ;top of arithmetic stack

FORNEXTMEMB EQU BASICMEMBASE+$0060 ;mem stack for storing nested for/next program statements
FORNEXTMEME EQU BASICMEMBASE+$00FF

ERROR_CODE EQU BASICMEMBASE+$0100
SUBCOUNT EQU BASICMEMBASE+$0102
ENDOFPGMTEXTPOINTER EQU BASICMEMBASE+$0103 ;may be better to call this STARTOFVARIABLESPOINTER
VARPOINTER EQU BASICMEMBASE+$0105

FORNEXTSTACKPOINTER EQU BASICMEMBASE+$0107 ;will be pointing to range 1360-13FF
ENDOFVARSTORAGE EQU BASICMEMBASE+$0109 ;2024 rev.
DATAPOINTER EQU BASICMEMBASE+$010B ;pointer to next value for reading from DATA statement - 2 bytes hi/lo 
FIXNUMBER EQU BASICMEMBASE+$010E ;set number of chars to right of decimal (FIX statement)
KEYVAL EQU BASICMEMBASE+$010F ;holds char input from INKEY statement
TXTPNTRMAIN EQU BASICMEMBASE+$0110

LINE_BUFB EQU BASICMEMBASE+$0160
LINE_BUFE EQU BASICMEMBASE+$01FF

RNDSEEDMEM EQU BASICMEMBASE+$0141

PGM_START EQU BASICMEMBASE+$0200
PGM_LIMIT EQU PGM_START+$2000
RAM_LIMIT EQU HIGH (BASICMEMBASE + $7F00) ;32k x 8 RAM = $7FFF - Leave room for memory stack area

;----------------------------------------------------------------
;using functions from MP1802Monitor
TERM_SERIAL_SND EQU SERIALSND ; send one char to serial terminal (function in Monitor)
TERM_SERIAL_RCV EQU SERIALRCV ; receive one char from serial terminal (function in Monitor)
MONITOR_CLOAD EQU CLOAD ; - cassette load (not tested in monitor)
MONITOR_CSAVE EQU CSAVE ; - cassette save (not tested in monitor)
MONITOR_CVR EQU CVR ; - clear video terminal screen
;MONITOR_PLOTXY EQU $0000 ;coarse serial plotting version incorporated in basic code below

;----------------------------------------------------------------
;Error Codes:
;	 1 > division by zero
;	 2 > unidentified command
;	 3 > memory full
;	 4 > exponential overflow
;	 5 > cannot find variable
;	 6 > bad expression
;	 7 > illegal arithmetic comparator
;	 8 > undefined return statement
;	 9 > bad variable syntax
;	10 > not enough data input or error
;	11 > missing equals sign
;	12 > undefined GOTO or GOSUB
;	13 > bad statement syntax
;	14 > illegal array
;	15 > illegal NEXT variable
;	16 > not enough data present
;	17 > string not found
;	18 > re-dimensioning array not allowed
;	19 > exceeded FOR/NEXT nesting limit
;	20 > unexpected character in statement
;	21 > square root of negative number

;--------------------------------------------------------------
;scratch memory low address assignments for arithmetic operations involving two numbers - NUMA and NUMB
;(low byte addresses within scratch memory beginning at SCRATCHMEMB)

;Addition - NUMA - 01 02 03 04 05 06 07 08 09
;           NUMB - 0A 0B 0C 0D 0E 0F 10 11 12 

;Mult&Div - NUMA - 01 02 03 04 05 06 07 08 09
;           NUMB - 0A 0B 0C 0D 0E 0F 10 11 12
;		  Result - 13 14 15 16 17 18 19 1A 1B
;Negative Number - 1C 1D 1E 1F 20 21 22 23 24


;--------------------------------------------------------------
;single letter text edit commands...most taken from 1802 Pilot:
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

;--------------------------------------------------------------
;Statement commands: [note: for many commands, just first 3 letters are needed...saved storage
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


;--------------------------------------------------------------
;--------------------------------------------------------------
;--------------------------------------------------------------
;entry to MPBasic begins here with setting up CALL and RETN registers
;along with memory stack and other program initializations.

L0003:
	LDI HIGH(CALL)
	PHI R4
	LDI HIGH(RTN)
	PHI	R5
	LDI HIGH(ENTRY1)
	PHI R3
	LDI	LOW(ENTRY1)
	PLO	R3
	SEP	R3 ;R3 is now program pointer
ENTRY1:
	LDI	HIGH(BASICMEMSTACK)
	PHI	R2
	LDI	LOW(BASICMEMSTACK)
	PLO	R2
	LDI	LOW(CALL)
	PLO	R4
	LDI	LOW(RTN)
	PLO	R5
	
	CALL ZEROMEM ;Zero out mem used for various program pointers/values
	CALL INITTXT ;set R10 to start of BASIC text and place 02 STX
	             ;note that INITTXT is not placing $03 (ETX )after $02 since I don't want BASIC text to be cleared at entry.
				 ;need to issue 'C' edit command to clear text
	CALL EDITPROMPT ;send text edit mode prompt to terminal
	LBR	GET_EDIT_CMD ;begin input line buffer routine waiting for edit command input
;
;-------------------------
	; D4 CALL subroutine - D stored to R15.1 on entry, and D restored on exit 
	SEP	R3
CALL:
	PHI	R15
	SEX	R2
	GHI	R6
	STXD
	GLO	R6
	STXD
	GHI	R7
	STXD
	GLO	R7
	STXD
	GHI	R3
	PHI	R6
	GLO	R3
	PLO	R6
	LDA	R6
	PHI	R3
	LDA	R6
	PLO	R3
	GHI	R15
	BR CALL-1

;-------------------------
	; D5 RETN subroutine - D stored to R15.1 on entry, and D restored on exit  
	SEP	R3
RTN:
	PHI	R15
	SEX	R2
	INC	R2
	GHI	R6
	PHI	R3
	GLO	R6
	PLO	R3
	LDXA
	PLO	R7
	LDXA
	PHI	R7
	LDXA
	PLO	R6
	LDX
	PHI	R6
	GHI	R15
	BR RTN-1
	
;--------------------------
	; Zero out mem area for various program pointers/counters
ZEROMEM:	
	LDI	HIGH(RNDSEEDMEM)
	PHI	R7
	LDI	LOW(RNDSEEDMEM)
	PLO	R7
ZEROMEM1:
	LDI $00
	DEC	R7
	STR	R7
	GLO	R7
	BNZ	ZEROMEM1
	RETN

;--------------------------------------------------------------		
	; Clear (C) command - clears BASIC program text 
C_CMD:
	CALL INITTXT
	LDI	$03 ;$03 is end-of-text marker
	STR	R10
	RETN

;-------------------------------------------------
	; Insert start-of-text ($02) at PGM_START address
	; R10 pointing to beginning of Basic text program
INITTXT:
	LDI HIGH(PGM_START)
	PHI	R10
	LDI	LOW(PGM_START)
	PLO	R10
	LDI	$02
	STR	R10
	INC	R10
	RETN

;-------------------------------------------------
TERMSND:
	;MPMonitor SerialSnd routine uses R15.0,R12.0,R13.0
	;save those register values to stack since they may be
	;used by the calling program
	PLO R15 ;D is byte to send, store to R15.0
	GLO R12
	STXD
	GLO R13
	STXD
	GLO R15 ;retrieve send-byte
	CALL TERM_SERIAL_SND ;Serial Term Send
	IRX
	LDXA
	PLO R13
	LDX
	PLO R12
	RETN

;-------------------------------------------------
BTERMRCV: ;(added preceding 'B' since TERMRCV label also in Monitor)
	CALL TERM_SERIAL_RCV ;Serial Term Receive
	RETN

;---------------------------------------------
	;output in-line chars following call to this routine to terminal until $00 encountered
	;Note that the CALL routine leaves R6 pointing to next byte following CALL address
	LDA	R6
	CALL TERMSND
SNDCHAR:
	LDN	R6
	BNZ	SNDCHAR-4
	INC	R6
	RETN
	
;-------------------------------------------------
LINEADVANCE:
	CALL SNDCHAR
	BYTE '\r\n',$00
	RETN

;-------------------------------------------------
EDITPROMPT:
	CALL SNDCHAR
	BYTE '\r\nEDIT\r\n',$00
	RETN
	
;-------------------------------------------------
;	stores call in-line byte to error code storage slot
ERRCODE:
	LDI HIGH(ERROR_CODE)
	PHI	R7
	LDI	LOW(ERROR_CODE)
	PLO	R7
	LDA	R6
	STR	R7
	RETN

;-------------------------------------------------
L00A9:
	CALL INITLINEBUF ;set start of line buffer
	SKP
L00AD:
	INC	R11
	LDN	R11
	XRI	$20 ;skip space
	BZ L00AD
	RETN

;-------------------------------------------------
INITLINEBUF:
	LDI	HIGH(LINE_BUFB)
	PHI	R11
	LDI	LOW(LINE_BUFB)
	PLO	R11
	RETN

;-------------------------------------------------
L00BB:
	; get error code at +$0100 and store in R9.0
	; actually, depending on D pass in, this can retrieve value of any pgm parameter into R9.0 
	PLO	R7
	LDI	HIGH(ERROR_CODE) 
	PHI	R7
	LDN	R7
	PLO	R9
	RETN

;-------------------------------------------------
	; following code from 1802 PILOT seems to want to display error code
	; may not show if code - 0
	; code seems overly complex, need to review
L00C2:
	LDI	$00
	PLO	R8
	GLO	R9
	PHI	R9
	LDI	$64
	CALL L00D4
	LDI	$0A
	CALL L00D4
	GHI	R9
	BR	L00E2
L00D4:
	PLO	R9
L00D5:
	GHI	R9
	PHI	R7
	CALL L00E8
	GLO	R9
	BNZ	L00E0
	GLO	R8
	BZ	L00E7
L00E0:
	INC	R8
	GLO	R9
L00E2:
	ORI	$30
	CALL TERMSND
L00E7:
	RETN

;-------------------------------------------------
L00E8:
	GLO	R9
	STR	R2
	LBZ	L00F8
	LDI	$00
	PLO	R9
	GHI	R7
	SKP	
L00F1:
	INC	R9
	SM
	LBDF	L00F1
	ADD
	PHI	R9
	RETN
L00F8:
	CALL ERRCODE
	BYTE $01
	RETN

;----------------------------------------------------------
;	BYTE $0D,$3A,$F9 ;THIS CODE DOESNT FIT
;L0100: ;following code called in Pilot, but maybe not here in BASIC
;	LDI	BASICBASEHIGH+$14
;	PHI	R7
;	LDA	R6
;	PLO	R7
;	GHI	R15
;	STR	R7
;	RETN

;----------------------------------------------------------
; Edit mode routine to input and echo one line of chars
LINEBUFFINPUT:
	CALL INITLINEBUF ;INITLINEBUF ;set start of line buffer to $0860 in R11
	LDI	$00
	PLO	R7
L010E:
	CALL BTERMRCV ;get one char from term	XON=$11	XOFF=$13
	STR	R11 ;store char to M(R11)
	XRI	$08	;check for backspace
	BZ	L0142;handle backspace
	XRI	$10 ; CANCEL (control X) = $18 XRI $08 = $10
	BZ	L0150
	XRI	$15 ; CR $0D XRI $08 = $05 XRI $10 = $15
	BZ	L0159
	XRI	$04 ; HT
	BZ	L012E
	INC	R7
	GLO	R7
	XRI	$3E ;max length line buffer
	BZ	L0159
	LDA	R11 ;get char just stored to M(R11) and echo to term
	CALL TERMSND ;echoing received char back to term?
	BR	L010E ;get next terminal char
L012E:
	;handle HT
	LDI	' '
	STR	R11	;store space in line buffer
	INC	R7
	INC	R11
	CALL TERMSND
	GLO	R7
	XRI	$3E ;max length line buffer
	BZ L0159
	GLO	R7
	ANI	$03
	BZ L010E
	BR L012E
L0142:
	GLO	R7 ;handle backspace keypress
	BZ L010E
	DEC	R7
	DEC	R11
	CALL SNDCHAR
	BYTE $08,$20,$08,$00
	BR L010E
L0150:
	CALL SNDCHAR ;handle CANCEL, delete entire line from buffer
	BYTE $0D,$15,$00,$00
	LBR LINEBUFFINPUT ;reset to beginning of line buffer
L0159:
	LDI	$0D ;store CR at end of line
	STR	R11
	CALL LINEADVANCE
	RETN

;-----------------------------------------------
;L0160
CMDLOOKUP:
	;this segment handles edit mode command table lookup to match input edit char
	STR	R2
CMDLOOKUP1:
	LDN	R7 			;get cmd char from table, cmd tbl ends with $00
	BZ	CMDLOOKUP3 	;end of table, issue error ;L017A	
	LDA	R7			;get cmd char from table
	XOR				;compare with mem
	BZ	CMDLOOKUP2	;cmd match, get address  ;L016E
	INC	R7			;increment to next table cmd
	INC	R7
	BR	CMDLOOKUP1 	;loop thru tble ;L0161
CMDLOOKUP2: ;access point from CMDSRCH statement match
	;this segment handles program pointer routing to edit-cmd & basic-statement matches from CMDSRCH
	;sets R8 to be program pointer temporarily while R3 is loaded with command address to continue program execution
	LDI	HIGH(CMDLOOKUP25);$01
	PHI	R8
	LDI	LOW(CMDLOOKUP25);$75
	PLO R8
	SEP	R8
CMDLOOKUP25:
	LDA	R7
	PHI	R3
	LDA	R7
	PLO	R3
	SEP	R3	;cmd address loaded to R3 and execution from R3
CMDLOOKUP3	;L017A:
	CALL ERRCODE 
	BYTE $02
	RETN

	BYTE $FF ; ISSUE WITH ASSEMBLER???

;-------------------------------------------------------------
;Wait for edit command input from terminal
;L0180:
GET_EDIT_CMD:
	LDI $00
	CALL L00BB ;get error code and put in R9.0
	BZ L0196
	CALL L00C2 ;??? display error code if not zero
	LDI "?"
	CALL TERMSND
	CALL LINEADVANCE
	CALL ERRCODE
	BYTE $00
L0196:
	LDI	">"
	CALL TERMSND ;send > prompt to term
	CALL LINEBUFFINPUT	;get one line of chars in line buffer 
	CALL L00A9 ;find first non-space char in line buffer as command pointed to by R11
	LDI	HIGH(EDIT_CMD_TBL) ;command letter table to R7
	PHI	R7
	LDI	LOW(EDIT_CMD_TBL)
	PLO	R7
	LDA	R11
	CALL CMDLOOKUP
L01AB:
	BR	GET_EDIT_CMD

	
;----------------------------------------------------
;---Command letters followed by 2-byte ADDress -----
EDIT_CMD_TBL:
	BYTE "B",HIGH(B_CMD),LOW(B_CMD) 
	BYTE "C",HIGH(C_CMD),LOW(C_CMD) 
	BYTE "D",HIGH(D_CMD),LOW(D_CMD) 
	BYTE "E",HIGH(E_CMD),LOW(E_CMD) 
	BYTE "I",HIGH(I_CMD),LOW(I_CMD) 
	BYTE "K",HIGH(K_CMD),LOW(K_CMD) 
	BYTE "L",HIGH(L_CMD),LOW(L_CMD) 
	BYTE "M",$00,$00 ;entry to new monitor
	BYTE "R",HIGH(EXECUTE),LOW(EXECUTE) ;$07E8
	BYTE "S",HIGH(S_CMD),LOW(S_CMD) 
	BYTE "U",HIGH(U_CMD),LOW(U_CMD) 
	BYTE "W",HIGH(W_CMD),LOW(W_CMD)
	BYTE "Z",HIGH(Z_CMD),LOW(Z_CMD) ;2024 addition to allow editing of current text line
	BYTE $00

;----------------------------------------------
; check if R10 pointing to upper case alpha char
; return 0x00 if yes, 0x01 if not
;L01D8:
CHK_UPALPHA:
	LDN	R10
	SMI	$41
	BNF	CHK_UPALPHA_1
	LDN	R10
	SMI	$5B
	BDF	CHK_UPALPHA_1
	LDI	$00
	RETN
CHK_UPALPHA_1:
	LDI	$01
	RETN

;---------------------------------------------	
	; 2024 MP addition of Z Command to more easily edit current text line.
Z_CMD:
	CALL SNDCHAR
	BYTE 'EDIT:\r\n',$00
	;R10 should be pointing to current text line of program
	;copy R10 to R8
	GHI R10
	PHI R8
	GLO R10
	PLO R8
	;set R11 to beginning of line buffer area
	CALL INITLINEBUF
	;R7.0 counts number of chars, init to zero
	;LDI $00
	;PLO R7
Z_CMD1:
	;copy current pgm text line to line buffer for editing
	;display chars to terminal as they are transfered
	LDN R8
	XRI $03
	LBZ Z_CMD2
	XRI $0E
	LBZ Z_CMD2
	LDA R8
	STR R11
	INC R11
	CALL TERMSND
	;INC R7
	LBR Z_CMD1
Z_CMD2:
	;call routine to facilitate modifying line buffer
	;need to emulate to see how it exits from a RETN
	;ESC to exit edit without replacing???
	CALL L010E ;original line buffer editing code for entering text
	;KILL current line
	;Note: KILL initially looks for possible value after 'K' in line buffer for numb lines to delete.
	;	Need to bypass that so as not to mess up current chars in line buffer.
	;	R9.0 needs to be initialize for number of lines to delete, which is one!
	LDI $01
	PLO R9
	CALL K_CMD_Z
	;INSERT line buffer
	;enter I_CMD with R11 pointing to beginning of line buffer
	;and R10 pointing to text position
	CALL INITLINEBUF
	CALL I_CMD
	
	RETN
;---------------------------------------------	
; B (Begin) Command
B_CMD:
	CALL INITTXT
L0203: ;following displays current line of text
	GHI	R10
	PHI	R8
	GLO	R10
	PLO	R8
L0207:
	LDN	R8
	XRI	$03
	LBZ	L0216
	XRI	$0E
	LBZ	L0216
	LDA	R8
	CALL TERMSND
	LBR L0207
L0216:
	CALL LINEADVANCE
	RETN
	
;------------------------------
;E(End) Command
;NOTE: seems like a lot of $03 compares?
E_CMD:		
	CALL L0220
	BYTE $03,$03
	RETN
L0220:
	LDA	R6
	PHI	R7
	LDA	R6
	PLO	R7
	SEX	R10
L0225:
	LDI	$03
	XOR
	BZ	L0235
	GLO	R7
	XOR
	BZ	L0235
	GHI	R7
	XOR
	BZ	L0237
	INC	R10
	BR	L0225
L0235:
	LDI	$01
L0237:
	RETN
		
;-----------------------------------------
; I (Insert) command
;L0238
I_CMD:
	SEX	R10
	GHI	R10	; R10 is text pointer, R7 is save/restore reg
	PHI	R7
	GLO	R10
	PLO	R7
L023D:
	LDA	R10
	XRI	$03 ; find end of pgm text
	BNZ	L023D
	GHI	R10
	XRI	HIGH(PGM_LIMIT); upper limit of RAM available to PILOT/BASIC PGM
	BZ	L025E ;display mem limit error
	
	;store byte at start of current line and temporarily replace with $02
	LDN	R7
	STR	R2
	LDI	$02 ;start of text byte
	STR	R7

	;shift pgm text one byte to right from end to temp $02
L024C:
	DEC	R10
	LDXA
	STXD
	XRI	$02
	BNZ	L024C
	
	;copy byte from line buf to pgm text, then repeat shift right process
	INC	R7
	LDN	R2
	STR	R7
	LDA	R11
	STR	R10
	INC	R10
	XRI	$0D ;terminate transfer when $0D reached at end of line buffer
	BNZ	L023D
	RETN
L025E:
	DEC	R10
	LDI	$03
	STR	R10
L0262:
	CALL ERRCODE
	BYTE $03 ;memory full error
	RETN

;---------------------------------------------
;NOTE: This needs more work to interface with MP1802Monitor
;Code that is here was based on circuit configuration from
;the 1980's

;CASSETTE SAVE - uses CSAVE Kansas City Format audio routine 
;in MP1802Monitor.  Saves Basic program to cassette.
;Enter with R10 pointing to beginning of program text
;R10 is copied to R8 to point to beginning of text and
;R9 is set to point to end of text.
;CSAVE outputs bytes from R8 to R9 using R2 mem stack.
;L0267:
S_CMD:
	;copy R10 (beginning of basic program) to R8 and R9
	GHI	R10
	PHI	R9
	PHI	R8
	GLO	R10
	PLO	R9
	PLO	R8
L026D:
	;find end-of-text ($03) with R9
	LDA	R9
	XRI	$03
	BNZ	L026D
	
	;following address assignments need to be changed
	;based on current configuration and mem locations

	;assign $3FFF to R2 - ?? monitor mem stack
	LDI	$3F
	PHI	R2
	LDI	$FF
	PLO	R2
	;assign $F8D2 to R4 - ?? monitor call routine
	;assign $F8E4 to R5 - ?? monitor return routine
	LDI	$F8
	PHI	R4
	PHI	R5
	LDI	$D2
	PLO	R4
	LDI	$E4
	PLO	R5
	;brancb to $F860 which is CSAVE function in monitor eprom
	LBR	MONITOR_CSAVE

;-------------------------------------
;NOTE: this needs more work to interface with MP1802Monitor

;address assignments in following code needs to be
;modified to reflect current memory addresses

;CASSETTE LOAD - uses CLOAD Kansas City Format audio in monitor
;	enter with R10 pointing to beginning of program text
;	copy R10 to R8
;	assign $EF to R9.1
;	assign $EFFF to R2 - ?? monitor temp mem location 
L_CMD:
	GHI	R10
	PHI	R8
	GLO	R10
	PLO	R8
	LDI	$EF
	PHI	R9
	PHI	R2
	LDI	$FF
	PLO	R2
	LDI	$F8
	PHI	R4
	PHI	R5
	LDI	$D2
	PLO	R4
	LDI	$E4
	PLO	R5
	LBR	MONITOR_CLOAD ;LF800
	
;-------------------------------------
; W (Write) Command
W_CMD:
	CALL L031F
	BZ	L02B5
	GHI	R10
	PHI	R8
	GLO	R10
	PLO	R8
L02A9:
	CALL L0207
	LDA	R8
	XRI	$03
	BZ	L02B5
	DEC	R9
	GLO	R9
	BNZ	L02A9
L02B5:
	RETN
	
	
;----------------------------------------------------------------------------------------------	
; U (Up) Command
U_CMD:
	CALL L031F
	BZ	L02CC
L02BB:
	DEC	R10
L02BC:
	LDN	R10
	XRI	$02
	BZ	L02CB
L02C1:
	DEC	R10
	LDN	R10
	XRI	$0D
	BNZ	L02BC
	DEC	R9
	GLO	R9
	BNZ	L02BB
L02CB:
	INC	R10
L02CC:
	CALL L0203 
	RETN
	
;--------------------------
; D (Down) Command
D_CMD:	
	CALL L031F 
	BZ L02D8
	CALL L02DC 
L02D8:
	CALL L0203 
	RETN

;----------------------------------------------
L02DC:
	LDA	R10
	XRI	$03
	LBZ	L02EA
	XRI	$0E
	BNZ	L02DC
	DEC	R9
	GLO	R9
	LBNZ	L02DC
	SKP	
L02EA:
	DEC	R10
	RETN

;-----------------------------------------------
; routine facilitates converting ascii number up to 255 to a binary number.
; enter with ascii number char in D and preceding binary number in R9.0
; return 1 if ascii not number
; convert ascii to binary and form singular binary with R9.0
; returns 0 if successful

;2024 rev - mods to allow for conversion of number to 65k to two bytes
;instead of passing in num char to R9.1, using R8.1. 
;using R9.1 and R9.0 to hold binary value
L0300:
	PHI R9
	SMI $30
	LBNF L031C ;return if char < $30
	GHI	R9
	SMI	$3A
	LBDF L031C ;return if char >= $3A
	GHI	R9
	ANI	$0F
	PHI	R9 ;decimal char
	GLO	R9 ;binary interim val
	SHL
	SHL
	STR	R2
	GLO	R9
	ADD
	SHL
	STR	R2
	GHI	R9
	ADD
	PLO	R9
	LDI	$00 ;valid number char detected
	RETN
L031C:
	LDI	$01 ;end of number detected
	RETN

;-------------------------------------------------------
L031F:
	CALL INITLINEBUF

L0322:
	LDI	$00
	PLO	R9
	PLO	R7
	SKP	
L0327:
	INC	R7
L0328:
	LDA	R11 ;get statement char
	CALL L0300
	LBZ L0327
	GLO	R7
	LBNZ L0337
	GHI	R9
	XRI	$0D
	LBNZ L0328
	INC	R9
L0337:
	GLO	R9
	RETN
	
	
;--------------------------------------------
; K (Kill) Command
K_CMD:
	CALL L031F ;this may be getting a value after 'K' for number lines to delete
	BZ	L034C
K_CMD_Z: ;2024 - added this entry point to facilitate Z_CMD
	GHI	R10
	PHI	R7
	GLO	R10
	PLO	R7
	CALL L02DC
	CALL L0350
	GHI	R7
	PHI	R10
	GLO	R7
	PLO	R10
L034C:
	CALL L0203
	RETN
L0350:
	LDA	R10
	STR	R7
	INC	R7
	XRI	$03
	BNZ	L0350
	RETN

;--------------------------------------------------
;---- this is end of 1802Pilot code used as template for
;---- text entry/editing commands in MP1802Basic.
;--------------------------------------------------


;--------------------------------------------------
;GETNUM routine
; 2024 mod - added code for hex value. Using R1.1 as flag for whether dealing with decimal or hex number
; Get number from program statement and store in scratch memory location as decimal
; Number is then normalized, packed, and copied to the arithmetic stack

;R13 points to ASTCK

GETNUM:
	CALL GETNUM1 ;_DEC_HEX
	;normalize decimal value in scratch
	CALL NORM
L0439:
	;pack decimal in scratch mem
	CALL PACK
	;copy decimal number (packed 4 bytes) from scratch mem to arithmetic stack
	LBR	COPY_TO_ARITHSTACK

;--------------------------------
;R10 - source text pointer
;R12 - scratch mem pointer
;R7.1 - holds sign
;R7.0 - holds exponent
;R15.1 - flag

;Scratch Mem: this segment from $0000 to $0009
; 00 01 02 03 04 05 06 07 08 09
;       X  X  X  X  X  X  X  X  = 8 digits of number

;R8.0 = exp on numA    R8.1 = sign of numA
;R9.0 = exp on numB    R9.1 = sign of numB

;2024 - separated following code out of GETNUM so could be used with DEC2BIN function
GETNUM1:
	LDI	HIGH(SCRATCHMEMB)
	PHI	R12
	LDI	LOW(SCRATCHMEMB)+$09
	PLO	R12
	SEX	R12
GETNUM2:
	;zero out the scratch mem 01-09 and exp/sign R8
	LDI $00
	STXD
	GLO	R12
	BNZ	GETNUM2
	PHI	R8 ;set to positive number (bit 7 of R8.1=0)
	PLO	R8 ;exponent set to zero
	PLO	R15 ;R15 used as decimal point encounter flag for setting up exponent
	
	LDN	R10
	;2024-check for hexadecimal indicator char '&' at M(R10)
	XRI $26; '&'
	LBZ GETHEX
	;check for negative sign at M(R10) 
	XRI	$0B ;'-' 
	BNZ	GETDEC
	;negative number signified by bit 7 of R8.1 = 1
	LDI	$80
	;PHI R7
	PHI	R8
	INC	R10
GETDEC:
	;set R12 to $0002 in scratch memory
	LDI	$02
	PLO	R12
	LSKP	
GETDEC2:
	INC	R15
	INC	R10
GETDEC3:
	;get pgm text char and check for decimal point or exponent sign
	LDN	R10
	XRI	$2E ;'.' 
	LBZ	GETDEC2
	;check for exponent specifier
	XRI	$6B ;'E'
	LBZ	GETDEC5
	;check if number
	LDN	R10
	SMI	$30
	LBNF GETDEC7 ;branch if < ascii 30
	SMI	$0A 
	LBDF GETDEC7 ;branch if > ascii 39
	GLO	R15 ;if R15.0 not zero, then decimal encountered and no longer increment exponent R8.0
	LBNZ GETDEC4
	INC	R8 ;counts number of chars to left of decimal point in number
GETDEC4:
	LDA	R10
	ANI	$0F ;strip $30 off ascii number char for decimal value
	STR	R12 ;store to decimal to R12 scratch pointer and increment pointer
	INC	R12
	LBR	GETDEC3 ;loop to get next pgm char
	;??? 2024 - no check on limiting number to 8 chars in scratch area??
GETDEC5:
	;this segment handles positive or negative exponent at end of number
	LDI $00 ;orig GHI R4
	PLO	R15
	INC	R10
	LDN	R10
	XRI	'-'
	LSNZ
	INC	R15 ;??flag for neg exponent
	INC	R10
	CALL NUMCON ;?convert exponent value (0-255) to binary in R9.0
	DEC R10
	;GLO	R7
	GLO	R8
	STR	R2
	GLO	R15
	LBZ GETDEC6
	GLO	R9
	SDI	$00
	PLO	R9
GETDEC6:
	GLO	R9
	ADD
	PLO R8
	;SKP	
GETDEC7:
	;if R10 char not a number...end of number?
;	;transfer sign and exponent from R7 to R8
;	GLO	R7
;	PLO	R8
;	GHI	R7
;	PHI	R8
L0433:
	LDI	$02
	PLO	R12 ;R12 at $0002 - beginning of decimal number in scratch mem
	RETN

;--------------------------------
GETHEX: ;2DEC:
;	;dealing with hex number. set R1.1 flag
;	LDI $01
;	PHI R1
	
	;hex num can be 1 to 4 chars [0-FFFF];  limit of 65K
	;read char and store in R9 register. keep shifting down register if more chars encountered
	;valid chars: 30-39, 41-46
	
	;using R9 to store full hex 2-byte value
	LDI $00
	PHI R9
	PLO R9
	INC R10
	
GETHEX5:
;POKE5:	
	LDA R10
	;temp store hex char
	PHI R1
	;check for valid hex char
	CALL CHK_HEX_CHAR
	BNZ GETHEX6 ;POKE6
	;call returned D=0 and encountered non-hex char, so treat as end of hex value
	;now convert R9 hex value to decimal using BIN2DEC routine
	;copy R9 to right most 2 bytes of binary assignment in scratch mem using R12
	DEC R10 ;point R10 back to ending char for proper handling by calling LET function
	LDI	HIGH(SCRATCHMEMB)
	PHI	R12
	LDI	LOW(SCRATCHMEMB)+$0A
	PLO R12
	
	LDI $00 ;zero out left 2 bytes of 4-byte binary
	STR R12
	INC R12
	STR R12
	INC R12
	GHI R9
	STR R12
	INC R12
	GLO R9
	STR R12
	;R12 now pointing to LSB of 4-byte binary for entry to BIN2DEC
	CALL BIN2DEC
	;BIN2DEC returns decimal that is right justified in scratch mem.
	;since next call is going to be NORM, and since NORM adjusts exponent
	;assuming decimal was left justified in scratch memory, need to set
	;R8 = $08 to correct for NORM adjustment.
	LDI $08
	PLO R8
	;set sign of decimal to positive
	LDI $00
	PHI R8
	RETN

;-------------------------------------------------------------------
GETHEX6:
	GHI R1 ;retrieve hex char to D
	CALL HEXCHAR2BIN
	;save D to temp mem
	STR R2
	;rotate R8 left 4 bits for D insertion using R7 counter
	LDI $04
	PLO R7
;POKE_HEXVAL1:	
GETHEX7:
	GLO R9
	SHL
	PLO R9
	GHI R9
	SHLC
	PHI R9
	DEC R7
	GLO R7
	LBNZ GETHEX7 ;POKE_HEXVAL1
	;insert saved 4 bit val to R8.0
	GLO R9
	ADD
	PLO R9
	;check for potential next hex char
	LBR GETHEX5 ;POKE5

;-------------------------------------------------------------------
CHK_HEX_CHAR:
	;pass in ascii char via D (and RF.1) to check if valid hex char $30-$39 or $41-$46
	;returns D=0 if not valid, D=1 if valid
	;
	;chk if char less than '0'
	SDI $2F
	BGE CHK_HEX_CHAR_NOTVALID
	;chk if char greater than 'F'
	GHI R15
	SDI $46
	BL CHK_HEX_CHAR_NOTVALID
	;is char '0' to '9'
	GHI R15
	SDI $3A
	BL CHK_HEX_CHAR_VALID
	;is char between $3A - $40
	GHI R15
	SDI $40
	BL CHK_HEX_CHAR_NOTVALID
CHK_HEX_CHAR_VALID:
	LDI $01
	LSKP
CHK_HEX_CHAR_NOTVALID:
	LDI $00
	RETN
	
;--------------------------------
	;convert ascii number in statement to binary (range 0-255)
	;returns binary val in R9.0
NUMCON:
	GHI	R10
	PHI	R11
	GLO	R10
	PLO	R11
	CALL L0322
	GHI	R11
	PHI	R10
	GLO	R11
	PLO	R10
	RETN
	
;-------------------------------
	;Normalize the number so that most significant digit
	;is non-zero, adjusting exponent accordingly
	;note: since shift-left iteration counter set to 8, a zero decimal will remain zero after 8 iterations
	;R8.1=sign
	;R8.0=exponent
	;R12->$0002 - decimal number in scratch mem
NORM:
	GLO	R12
	PLO	R11
	ADI	$07
	STR	R2 ;$09 stored to mem stack for XOR test for location
	LDI	$08
	PLO	R7 ;shift left counter set to 8
NORM1: ;L0453:
	LDN	R12 ;get digit from scratch
	BNZ	NORM3 ;L046A ;if left-most digit is non-zero, then it is normalized
NORM2: ;L0456:
	;left digit is zero, shift 8 digits left one position
	INC	R12
	LDN	R12
	DEC	R12
	STR	R12
	INC	R12
	GLO	R12
	XOR	;test if R12 at $09
	BNZ	NORM2 ;L0456 ;if not $09, get next digit
	STR	R12 ;store 00 at end of number
	;[can't use DEC R8 since it will affect R8.1 if exponent goes negative]
	GLO	R8
	SMI	$01
	PLO	R8 ;subtract one from exponent
	GLO	R11
	PLO	R12 ;set R12 to $0002
	DEC	R7 ;decrement shift left counter
	GLO	R7
	BNZ	NORM1 ;L0453
NORM3: ;L046A:
	RETN

;--------------------------------------------------------
PACK:
	;compress 8 stripped ascii digits into 4 bytes
	;R12-->$0002 from NORM (normalized stripped ascii 8 digit number)
	GHI	R12
	PHI	R11
	GLO	R12
	PLO	R11
	SEX	R12
	LDI	$04
	PLO	R7 ;iteration counter = 4
L0474:
	;iterate through this 4 times
	;get digit, SHL four times and OR with next digit, store to M(R11)
	LDA	R12
	SHL
	SHL
	SHL
	SHL
	OR
	STR	R11
	INC	R11
	INC	R12
	DEC	R7
	GLO	R7
	LBNZ	L0474
	GLO	R12
	SMI	$05
	PLO	R12
	RETN
;
;--------------------------------------------------------
UNPACK:
	GHI	R12
	PHI	R11
	GLO	R12
	ADI	$04
	PLO	R11
	SEX	R11
	LDI LOW(SCRATCHMEMB)+$04
	PLO	R7
L0490:
	LDN	R12
	ANI	$0F
	STXD
	LDN	R12
	SHR
	SHR
	SHR
	SHR
	STXD
	DEC	R12
	DEC	R7
	GLO	R7
	BNZ	L0490
	INC	R12
	RETN
;
;--------------------------------------------------------
; Retrieve numbers (6 bytes) From the arithmetic stack
; R8.1=NUMA sign   R8.0=NUMA exponent
; R9.1=NUMB sign   R9.0=NUMB exponent
; R13 --> arithmetic stack ASTCK (initialized in INIT_ARITH_STACK_POINTER)
; R12 target address specified by calling code

COPY_ARITHSTACK_TO_NUMB:
	INC	R13
	LDA	R13
	PHI	R9
	LDA	R13
	PLO	R9
	BR	FSTCK0 
;this address called from INT
COPY_ARITHSTACK_TO_NUMA:
	INC	R13
	LDA	R13
	PHI	R8
	LDA	R13
	PLO	R8
FSTCK0:
	LDA	R13
	STR	R12
	INC	R12
	LDA	R13
	STR	R12
	INC	R12
	LDA	R13
	STR	R12
	INC	R12
	LDN	R13
	STR	R12
	RETN

;--------------------------------------------------------
;L04B9
COPY_TO_ARITHSTACK:
	; Store numbers (6 bytes) To arithmetic stack for potential arithmetic operations
	; R13 --> arithmetic stack ASTCK (initialized in INIT_ARITH_STACK_POINTER
	; R12 --> right most byte of 4-bytes to be stored to ASTCK
	; R8.1=sign   R8.0=exponent
	SEX	R13
	LDN	R12
	STXD
	DEC	R12
	LDN	R12
	STXD
	DEC	R12
	LDN	R12
	STXD
	DEC	R12
	LDN	R12
	STXD
	GLO	R8
	STXD
	GHI	R8
	STXD
	RETN

;--------------------------------------------------------
ZERO_CHECK:
	;check if 8-digit decimal number is zero
	;enter with R12 pointing to left-most decimal number
	;returns D = 0 if decimal is zero
	LDI	$08
	PLO	R7
ZERO_CHECK1:
	LDA	R12
	BNZ	L04D4
	DEC	R7
	GLO	R7
	BNZ	ZERO_CHECK1
L04D4:
	RETN

;--------------------------------------------------------
NEGNUM:
	SEX	R12
	LDI	$09
	PLO	R7
	SHR
L04DA:
	LDI	$09
	SMB
	STXD
	DEC	R7
	GLO	R7
	BNZ	L04DA
	RETN
;
;--------------------------------------------------------
;shift 8 number chars one char to right, do 8-digit shift for R7.0 times
;enter with RC.0 pointing to digit 7 of 8
;L04E3:
EIGHT_DIGIT_SHIFT_RIGHT:
	SEX	R12
L04E4:
	LDI	$08 ;iteration counter
	PLO	R15
L04E7:
	LDA	R12
	STXD
	DEC	R12
	DEC	R15 ;dec counter and check for zero
	GLO	R15
	BNZ	L04E7 ;iterate if not zero
	GLO	R12
	ADI	$08 ;resets R12.0 back to start position of shift-right
	PLO	R12
	DEC	R7
	GLO	R7
	LBNZ L04E4
	RETN
;
;--------------------------------------------------------
; Fetches 2 numbers from arithmetic stack (ASTCK) and unpacks
;L04F7
FETCH_ARITHSTACK_TO_NUMB_NUMA:
	LDI	HIGH(SCRATCHMEMB);$13
	PHI	R12
	LDI	$0A
	PLO	R12
	LDI	$00
	STR	R12
	INC	R12
	CALL COPY_ARITHSTACK_TO_NUMB
	CALL UNPACK
	LDI	$01
	PLO	R12
	LDI	$00
	STR	R12
	INC	R12
	CALL COPY_ARITHSTACK_TO_NUMA
	LBR UNPACK
	
;--------------------------------------------------------
;L0514:
ADD:
	CALL FETCH_ARITHSTACK_TO_NUMB_NUMA
	CALL ZERO_CHECK
	LBZ L05B6
	LDI	$0B
	PLO	R12
	CALL ZERO_CHECK
	LBZ L05C0
	GLO	R8
	STR	R2
	GLO	R9
	SD
	LBZ L054B ;(XA=XB)
	SHL
	LBNF L053F ;(XA>XB)
	SHRC
	SDI	$00 ;(XB>XA)
	PLO	R7
	SMI	$09
	LBDF	L05B6
	LDI	$08
	PLO	R12
	CALL EIGHT_DIGIT_SHIFT_RIGHT
	GLO	R9
	PLO	R8
	BR	L054B
L053F:
	SHRC
	PLO	R7
	SMI	$09
	LBDF	L05C0
	LDI	$11
	PLO	R12
	CALL EIGHT_DIGIT_SHIFT_RIGHT
L054B:
	GHI	R8
	BZ	L0554
	LDI	$09
	PLO R12
	CALL NEGNUM
L0554:
	GHI	R9
	BZ	L055D
	LDI	$12
	PLO	R12
	CALL NEGNUM
L055D:
	GHI	R8
	STR	R2
	GHI	R9
	ADD
L0561:
	LDI	$09
	PLO	R12
	LDI	$12
	PLO	R11
	SEX	R12
L0568:
	LDN	R11
	ADC
	STR	R12
	SMI	$0A
	BNF	L0570
	STR	R12
L0570:
	DEC	R12
	DEC	R11
	GLO	R12
	LBNZ	L0568
	INC	R12
	LDN	R12
	BNF	L058C
	BNZ	L058C
	LDI	$12
	PLO	R11
	SEX	R11
	LDI	$00
	STXD
	STXD
	STXD
	STXD
	STXD
	STXD
	STXD
	STXD
	STXD
	LBR	L0561
L058C:
	SMI	$02
	BNF	L059A
	LDI $09
	PLO R12
	CALL NEGNUM
	INC	R12
	LDI	$80
	LSKP
L059A:
	LDI	$00
L059C:
	PHI	R8
	LDA	R12
	BZ	L05C5
	LDI	$01
	PLO	R7
	PLO	R9
	LDI	$08
	PLO	R12
	CALL EIGHT_DIGIT_SHIFT_RIGHT
	CALL EXPADD
	LDI	$02
	PLO	R12
L05B0:
	CALL PACK
L05B3:
	LBR	COPY_TO_ARITHSTACK
L05B6:
	GLO	R13
	SMI	$06
	PLO	R12
	GHI	R9
	PHI	R8
	GLO	R9
	PLO	R8
	BR L05B3
L05C0:
	GLO	R13
	SMI	$06
	PLO	R13
	RETN
L05C5:
	CALL NORM
	BR	L05B0

;--------------------------------------------------
SUB:
	INC	R13
	LDN	R13
	XRI	$80
	STR	R13
	DEC	R13
	LBR	ADD

;-------------------------------------------------
EXPSUB:
	GLO	R9
	SDI	$00
	PLO	R9
EXPADD:
	GLO	R9
	STR	R2
	GLO	R8
	ADD
	PLO	R8
	SHL
	GLO	R8
	BDF	L05EB
	SMI	$66
	BNF	L05E8
	CALL ERRCODE
	BYTE $04
L05E8:
	LDI	$00
	RETN
L05EB:
	SMI	$9D
	LBDF	L05E8
	LDI	$01
	RETN

;-------------------------------------------------
ZNUMB:
	SEX	R13
	LDI	$00
	STXD
	STXD
	STXD
	STXD
	STXD
	STXD
	RETN

;------------------------------------------------	
MULT:
	CALL FETCH_ARITHSTACK_TO_NUMB_NUMA
	LDI	$1B
	PLO	R12
	SEX	R12
L0603:
	LDI	$00
	STXD
	GLO	R12
	XRI	$12
 	LBNZ L0603
	CALL EXPADD
L060E:
	LBNZ ZNUMB
	GHI	R8
	STR	R2
	GHI	R9
	ADD
	PHI	R8
	GHI	R13
	STXD
	GLO	R13
	STXD
	GHI	R12
	PHI	R13
	LDI	$0B
	PLO	R13
	LDI	$09 ;iteration count
	PLO	R9
L0622:
	LDA	R13
	PLO	R7 ;NUMB
L0624:
	GLO	R7
	LBZ	L0641
	LDI	$1C
	PLO	R12
	LDI	$0A
	PLO	R11
	SHL
	SEX	R12
L062F:
	LDN	R11
	ADC
	STR	R12
	SMI	$0A
	BNF	L0637
	STR	R12
L0637:
	DEC	R12
	DEC	R11
	GLO	R12
	XRI	$12
	LBNZ L062F
	DEC	R7
	LBR	L0624
L0641:
	LDI	$01
	PLO	R7
	LDI	$09
	PLO	R12
	CALL SHIFTR1
	DEC	R9
	GLO	R9
	LBNZ L0622
	LDI $13
	PLO	R12
	LDA	R12
	LBZ L0662
L0654:
	LDI	$01
	PLO	R7
	LDI	$1B
	PLO	R12
	CALL SHIFTR1
	LDI	$14
	PLO	R12
	LBR	L0666
L0662:
	GLO	R8
	SMI	$01
	PLO	R8
L0666:
	INC	R2
	LDA	R2
	PLO	R13
	LDN	R2
	PHI	R13
	LBR	L05B0

;---------------------------------------------
;L066E	
DIV:
	CALL FETCH_ARITHSTACK_TO_NUMB_NUMA
	LDI	$0B
	PLO	R12
	CALL ZERO_CHECK
	BNZ L067E
	CALL ERRCODE
	BYTE $01
	RETN
L067E:
	CALL EXPSUB
	LBNZ	L060E
	GHI	R8
	STR	R2
	GHI	R9
	ADD
	PHI	R8
	GHI	R13
	STXD
	GLO	R13
	STXD
	GHI	R12
	PHI	R13
	LDI	$13
	PLO	R13
	LDI	$24
	PLO	R11
	LDI	$12
	PLO	R12
	SEX	R11
L0698:
	LDN	R12
	SDI	$09
	STXD
	DEC	R12
	GLO	R12
	XRI	$09
	BNZ	L0698
	LDI	$09
	PLO	R9
L06A5:
	LDI	$00
	PLO	R7
L06A8:
	LDI	$24
	PLO	R11
	LDI	$09
	PLO	R12
	SHR
	SEX	R12
L06B0:
	LDN	R11
	ADC
	STR	R12
	SMI	$0A
	BNF	L06B8
	STR	R12
L06B8:
	DEC	R12
	DEC	R11
	GLO	R12
	BNZ	L06B0
	BNF	L06C2 ;negative result
	INC	R7
	BR	L06A8
L06C2:
	LDI	$09
	PLO	R12
	LDI	$12
	PLO	R11
L06C8:
	LDN	R11
	ADC
	STR	R12
	SMI	$0A
	BNF	L06D0
	STR	R12
L06D0:
	DEC	R12
	DEC	R11
	GLO	R12
	BNZ	L06C8
	GLO	R7
	STR	R13
	INC	R13
L06D8:
	INC	R12
	INC	R12
	LDN	R12
	DEC	R12
	STR	R12
	GLO	R12
	XRI	$09
	BNZ	L06D8
	DEC	R9
	GLO	R9
	BNZ	L06A5
	LDI	$13
	PLO	R12
	LDA	R12
	LBZ	L0666
	GLO	R8
	ADI	$01
	PLO	R8
	LBR	L0654

;----------------------------------------------
SHIFTR1:
	LDA	R12
	STR	R12
	DEC	R12
	DEC	R12
	LBR	EIGHT_DIGIT_SHIFT_RIGHT
	
;----------------------------------------------
OUT2:
	LDI	$08
	PLO	R12
	SKP	
L0703:
	DEC	R12
OUT2A:
	LDN	R12
	LBZ L0703
	INC	R12
	LDI	$3A
	STR	R12
	RETN

;----------------------------------------------------
OUTPUT:
	LDI	$01
L070E:
	PLO	R12
	CALL ZERO_CHECK
	LBNZ L071C
	CALL SNDCHAR
	BYTE '0. ',$00
	RETN
L071C:
	LDI HIGH(FIXNUMBER)
	PHI	R7
	LDI	LOW(FIXNUMBER)
	PLO	R7
	GHI	R8
	LBZ	L0729
	LDI	'-' ;$2D
	CALL TERMSND
L0729:
	GLO	R8
	SHL
	LBDF L0765 ;negative exponent
	SMI	$12
	LBDF L078E; written code is L078E, eprom is L070E ;exponential output
	GLO	R8
	ADI	$01 ;0002 since R8 was 0001
	STR	R2
	LDN	R7
	ADD
	PLO	R9
	LDI	$09
	PLO	R12 ;R12 points to end of number at $0009
	;---------------------------------------------------
	;my test showing $0001> 05 FE 3A 00 00 00 00 00 00  LET A=5 PRINT A
	;---------------------------------------------------
L073B:
	;shift R12 bytes ($0002-$0008) to the right by one position
	GLO	R12
	XOR ;check for R12 at $1302 since R8 was 0001
	BZ	L0745
	DEC	R12
	LDA	R12
	STR	R12
	DEC	R12
	BR	L073B
L0745:
	LDI	$FE ;when sending to term later, FE+30=2E ('.')
	STR	R12 ;R12 at $0002
	LDI	$09
	PLO	R12 ;R12 @ $0009
	CALL OUT2A ;this inserted 3A after FE
	LDI	$01
	PLO	R12 ;R12 @ $0001
L0751:
	LDN	R12
	XRI	$3A
	LBZ	L0760
	LDA	R12
	ADI	$30
	CALL TERMSND
	DEC	R9 ;decrementing from 0A
L075D:
	GLO	R9
	LBNZ	L0751
L0760:
	LDI	$20
	LBR	TERMSND
L0765:
	SHRC
	SDI	$00
	PLO	R8
	CALL OUT2
	DEC R12
	GLO	R12
	STR	R2
	GLO	R8
	ADD
	SMI	$09
	BDF	L0792 ;exponential output
	LDN	R7
	PLO	R9
	LDI	$2E
	CALL TERMSND
L077C:
	GLO	R9
	LBZ	L0760
	LDI	$30
	CALL TERMSND
	DEC	R9
	DEC	R8
	GLO	R8
	BNZ	L077C
	LDI	$01
	PLO	R12
	LBR L075D
L078E:
	DEC	R8
	LDI	$00
	LSKP
L0792:
	INC	R8
	LDI HIGH(BASICMEMSTACK)
L0794:
	PHI	R8
	LDN	R7
	PLO	R9
	CALL OUT2
	LDI	$01
	PLO	R12
	LDA	R12
	ADI	$30
	CALL TERMSND
	LDI	'.' ;$2E
	CALL TERMSND
L07A8:
	GLO	R9
	BZ	L07B9
	LDN	R12
	XRI	$3A
	BZ	L07B9
	LDA	R12
	ADI	$30
	CALL TERMSND
	DEC	R9
	BR L07A8
L07B9:
	LDI	'E' ;$45
	CALL TERMSND
	GHI	R8
	BZ	L07C6
	LDI	'-' ;$2D
	CALL TERMSND
L07C6:
	GLO	R8
	PLO	R9
	CALL L00C2
	LBR L0760

;-----------------------------------
; set R8 to point to text pointer stored in mem for current subroutine
; cant find where subroutine count ($1402) is initialized at execution???
;L07CD
SUBROUT1:
	;get subcount val at $0102, add val to $10, and put result in R8.0
	LDI HIGH(SUBCOUNT)
	PHI	R8
	LDI LOW(SUBCOUNT)
	PLO	R8
	LDN	R8
	ADI $10
	PLO	R8
	;R8-->mem text pointer for current subroutine level
	RETN

;-----------------------------------
; sets main text pointer R10 to text pointer for current subcount
SUBROUT2:
	CALL SUBROUT1
	LDA	R8
	PHI	R10
	LDN	R8
	PLO	R10
	RETN

;----------------------------------
; main text pointer R10 is stored to subcount text pointer
SUBROUT3:
	CALL SUBROUT1
SUBROUT3.1:
	GHI	R10
	STR	R8
	INC	R8
	GLO	R10
	STR	R8
	RETN

;-------------------------------------
; enter with R2 as memory stack pointer
; R10 is main BASIC program pointer
EXECUTE:
	CALL INITTXT ;L005F  Set R10 to start of text, place $02 at start
	CALL ZEROMEM ;L004A  Zero out memory for program parameters +0100-+0140; uses R7
	
	;init for/next stack
	LDI HIGH(FORNEXTSTACKPOINTER)
	PHI	R8
	LDI LOW(FORNEXTSTACKPOINTER)
	PLO	R8
	;+$0060 is beginning of for/next mem stack
	LDI	HIGH(FORNEXTMEMB)
	STR	R8
	INC R8
	LDI	LOW(FORNEXTMEMB)
	STR	R8
	
	LDI	LOW(FIXNUMBER) ;$0E
	PLO	R8
	LDI	$08
	STR	R8 ;initialize FIX number $08
	
;NOT USING FOR NOW
;	;2024 mod - init DATA statement pointer to beginning of pgm text
;	;then don't need to check for value in READ routine
;	LDI LOW(DATAPOINTER)
;	PLO R8
;	GHI R10
;	STR R8
;	INC R8
;	GLO R10
;	STR R8
	
	; R10 pointing to beginning of program text
	CALL SUBROUT3 ;L07DF 
	; SUBROUT3 stored R10 to current subroutine level pointer R8	
EXECUTE1:
	LDA	R10
	XRI	$03 ;check for end of program text
	BNZ	EXECUTE1
	; variables encountered during program text interpretation are stored after the program text.
	; set $04 byte after end of program text as marker for end of variable storage
	LDI	$04
	STR	R10
	LDI LOW(ENDOFPGMTEXTPOINTER)
	PLO R8
	CALL SUBROUT3.1	;store R10 to M(ENDOFPGMTEXTPOINTER).  At this point, storing pointer to start of variables (one byte beyond end of program text)
	CALL SUBROUT2 ;set R10 to program at current subroutine level
EXECUTE2:
	; since uncontrolled serial comms can't register key press during pgm execution,
	; checking for input button (EF4) low to exit pgm execution
	BN4 EXECUTE3
	LBR EDITPROMPT
	
	;original code checked for key press at this point during program execution.
	;this was when keyboard was interfaced with micro and key press was stored in KEYVAL.
	;using current bit-bang serial communications makes it difficult to emulate that function.

EXECUTE3:
; loop on pgm text until ascii char >= 'A'
	;-- new addition May 18, 2021 - check if current R10 byte is end-of-text
	LDN R10
	XRI $03
	LBZ EDITPROMPT
	;-----------------------------------------------------------------
	;load and advance until first pgm char >= 'A'
	;note that this skips over any ascii numeric chars for a line label
	LDA	R10
	SMI	$41
	LBNF EXECUTE3
	;(R10 pointing one past char)
	;find matching statement to R10 text and execute
	CALL CMDSRCH ; L0839
	;check for zero-error-code, if zero, branch to EXECUTE2
	LDI HIGH(ERROR_CODE) ;$14
	PHI R8
	LDI LOW(ERROR_CODE) ;$00
	PLO	R8
	LDN	R8
	LBZ	EXECUTE2
	;if non-zero-error-code then appears to exit
	LDI	$01
	PLO	R9
	CALL L02BC ;U_CMD segment - appears to be trying to display line that produced error
	LBR	EDITPROMPT ;L008A

;----------------------------------------
; BASIC COMMAND SEARCH - find command that matchs current basic pgm pointer
CMDSRCH:
; set R7 to point to statement table
	LDI	HIGH(COMMAND_TABLE)
	PHI	R7
	LDI	LOW(COMMAND_TABLE)
	PLO	R7
	SEX	R7 ;R7 --> COMMAND Table
	LDN	R10
	SMI	$41 ;check if 2nd text char is alpha
	DEC	R10 ;R10 pointing to 1st text char
	BDF	CMDSRCH2 ; branch if 2nd pgm char is alpha >= 'A'
	;if not alpha, assume dealing with variable name, such as 'A1', will perform implied LET statement
	LDI	LOW(COMMAND_TABLE+4) ;LET command address pointer
	PLO	R7
CMDSRCH1:
	LBR	CMDLOOKUP2 ;R3(pgm pointer)will be set to statement address
CMDSRCH2:
	;copy R10 to R9
	GHI	R10
	PHI	R9
	GLO	R10
	PLO	R9
CMDSRCH3:
	LDN	R7
	LBNZ CMDSRCH4 
	CALL ERRCODE ;if $00 at end of table encountered, then unidentified command
	BYTE $02
	RETN
CMDSRCH4:
	;compare pgm text to table command
	;if chars match, keep looping until $00 encountered in table
	LDA	R10
	XOR
	LBNZ CMDSRCH6 ;not match, get next table command
	INC	R7
	LDN	R7
	LBNZ CMDSRCH4
CMDSRCH5: ;pgm command matches table command
	;skip any additional alpha chars in basic text command
	LDA	R10
	SMI	$41
	BDF	CMDSRCH5
	DEC	R10
	INC	R7
	;R7-->matched command address
	LBR	CMDSRCH1
CMDSRCH6:
	;set R7 to next command in table and reset R10 to start of program command
	LDA	R7
	BNZ	CMDSRCH6
	INC	R7
	INC	R7
	GHI	R9
	PHI	R10
	GLO	R9
	PLO	R10
	LBR	CMDSRCH3
	
;----------------------------------------------
; END COMMAND
END:
	CALL EDITPROMPT
	INC	R2
	INC	R2
	INC	R2
	LDI	HIGH(L01AB) ;$01
	PHI	R6
	LDI	LOW(L01AB) ;$AB
	PLO	R6
	RETN


;---------------------------------------------
L0881:
BGOTO:
	CALL LINE_SEARCH
	LDI	$00
	CALL L00BB
	LBZ	SUBROUT3 ;L07DF
	RETN

;----------------------------------------------
;L088D:
LINE_SEARCH:
	LDA	R10
	XRI	$20
	BZ	LINE_SEARCH ;L088D ;skip spaces
	DEC	R10
	GHI	R10
	PHI	R8
	PHI	R7
	GLO	R10
	PLO	R8
	PLO	R7 ;R8,R7 point to label to search for
	CALL INITTXT ;point R10 to beginning of pgm text
	SEX	R10
	SKP
L089E:
	INC	R10
L089F:
	LDN	R10
	XRI	$03 ;check for end of pgn text
	BNZ	L08A9
	CALL ERRCODE
	BYTE $0C
	RETN
L08A9:
	LDN	R10
	SMI	$30
	BNF	L089E ;not valid char for label?
	SMI	$0A
	BNF	L08B9
L08B2:
	LDA	R10
	XRI	$0D
	BNZ	L08B2 ;skip to end of pgm text line
	BR	L089F ;search for matching label on next line
L08B9:
	LDA	R8
	XOR
	INC	R10
	BZ	L08C4
L08BE:
	GHI	R7
	PHI	R8
	GLO	R7
	PLO	R8
	BR	L08B2
L08C4:
	LDN	R8
	SMI	$30
	BNF	L08CD
	SMI	$0A
	BNF	L08B9
L08CD:
	LDN	R10
	SMI	$30
	BNF	L08D6
	SMI	$0A
	BNF	L08BE
L08D6:
	DEC	R10
	LDN	R10
	XRI	$0D ;CR
	LBZ	L08E0
	XRI	$0F ;$02
	LBNZ	L08D6
L08E0:
	INC	R10
	RETN
	
;------------------------------------------
;L08E2
REM:
	LDA	R10
	XRI	$0D ;CR
	LBZ	REM1
	XRI	$37 ;':'
	LBNZ	REM
REM1:
	LBR	SUBROUT3

;------------------------------------------
;L08EE
STOP:
	CALL ERRCODE
	BYTE $FF
	RETN

;------------------------------------------
;L08F3
BGOSUB:
	GHI	R10
	PHI	R11
	GLO	R10
	PLO	R11
	CALL REM
	GHI	R11
	PHI	R10
	GLO	R11
	PLO	R10
	;GHI R2
	LDI HIGH(SUBCOUNT)
	PHI	R7
	LDI	LOW(SUBCOUNT) ;$02
	PLO	R7
	LDN	R7
	SHR
	ADI	$01
	SHL
	STR	R7
	CALL LINE_SEARCH
	LDI	$00
	PLO	R7
	LDN	R7
	LBNZ SUBROUT2 ;L07D7
	LBR	SUBROUT3 ;L07DF

;------------------------------------------
;0916
RETURN:
	;GHI R2
	LDI HIGH(SUBCOUNT)
	PHI	R8
	LDI	LOW(SUBCOUNT) ;$02
	PLO	R8
	LDN	R8
	LBNZ L0923
	CALL ERRCODE
	BYTE $08
	RETN
L0923:
	SHR
	SMI	$01
	SHL
	STR	R8
	LBR	SUBROUT2 ;L07D7

;------------------------------------------
;L092B
RESTORE:
	;GHI R2
	LDI HIGH(DATAPOINTER)
	PHI	R7
	LDI	LOW(DATAPOINTER) ;$0B
	PLO	R7
	LDI	$00
	STR	R7
L0933:
	LBR	REM ;L08E2

;------------------------------------------
;L0936
CLS:
	CALL SNDCHAR
	BYTE $1B,'[2J',$00 ;esc sequence to clear screen for VT100 term
	CALL SNDCHAR
	BYTE $1B,'[0;0H',$00 ;esc sequence to set cursor to 0,0 for VT100 term
	RETN

;------------------------------------------
; following code not used from my original notes
;L093C:
;	INC	R10
;	GHI	R10
;	XRI	$1F ;RAM Limit
;	BZ	L0947
;	LDI	$00
;	STR	R10
;	BR	L093C
;L0947:
;	CALL SUBROUT2
;	LBR	EXECUTE2

;----------------------------------------- 
;L094D
VARIABLE_SEARCH:
	; Variable Search - see if variable exists in stored variables at end of program text
	; routine returns flag values via D:
	; 00=variable not used
	; 01=variable used
	; 02=illegal array

	;STEP 1 - get variable being evaluated from program statement and store id in R9
	;one or two char variables possible
	;examples: numeric - A,C5 ; string - B$, D6$ ; numeric array - K(
	LDA	R10
	PHI	R9	;char 1 of var
	LDA	R10
	PLO	R9	;char 2 of var
	
	XRI	$28 ;check char 2 = '(' and branch if array
	LBZ VSEARCH_ARRAY ;L09C9
	
	XRI	$0C ;check char 2 = '$' [exmpl A$]
	LBZ	L09B1
	
	LDA	R10 ;checking for potential char 3 of string variable [exmpl A1$]
	XRI	$24 ;check char 3 = '$'
	BZ L09C1
	
	DEC	R10 ;point to char 2
	GLO	R9	;char 2
	SMI	$30
	BNF	L0968 ;char 2 less than number
	SMI	$0A
	BNF	VSEARCH_R9COMPARE ;L096C ;char 2 is number
	
L0968:
	;char 2 was not number
	DEC	R10 ;point to char 1
L0969:
	LDI	$00
L096B:
	PLO	R9 ;R9.0=00 since only single char numeric variable


;L096C:
VSEARCH_R9COMPARE:
	;STEP 2 - compare variable in R9 with any variables already stored after program text
	;variables are stored after end of basic program text
	;set R12 to point to start of variable storage
	LDI HIGH(ENDOFPGMTEXTPOINTER)
	PHI	R7
	LDI	LOW(ENDOFPGMTEXTPOINTER)	
	PLO	R7
	LDA	R7
	PHI	R12
	LDN	R7
	PLO	R12
	SEX	R12

L0976:
	LDN	R12
	BZ L0995 ;if zero, then array
	
	;check for string variable
	SHL
	BDF	L09A6
	
	;check if at end of variable storage which is terminated by $04
	XRI	$08 ;$04 SHL = $08
	BZ L098B ;at end of variables, return D=0, variable not used
	
	;compare numeric variable in R9 with stored variable pointed to by R12
	GHI	R9
	XOR
	INC	R12
	LSNZ
	GLO	R9
	XOR
	BNZ	L098C ;not a match

	;stored var matches R9 var
L0988:
	INC	R12
L0989:
	LDI	$01 ;1 indicates variable has been used before
L098B:
	RETN ;returns: D=1 & R12 points to variable val, or D=0 & R12 points to new variable slot

	;setup R12 to point to next stored var
L098C:
	INC	R12
	INC	R12
	INC	R12
	INC	R12
	INC	R12
	INC	R12
	INC	R12
	BR L0976
	
L0995:
	INC	R12
	GLO	R9
	XOR
	BZ L0988


	;add the F. P. bytes (data length) in stored string and array
	;variables to R12 to point to beginning of next variable?
L099A:
	INC	R12
	GLO	R12
	ADD
	PLO	R7
	GHI	R12
	INC	R12
	ADC
	PHI	R12
	GLO	R7
	PLO	R12
	BR L0976
	
	;looking for string variable match
L09A6:
	GHI	R9
	XOR
	INC	R12
	BNZ	L099A
	GLO	R9
	XOR
	BNZ	L099A
	BR	L0988
	
	;setting up R9 with string variable being evaluated
L09B1:
	;shifting R9.1 left one bit as code for string variable
	;exmpl: A = 0x41 SHL = 0x82
	;then search variables at end of program to find match to 0x82
	GHI	R9
	SHL
	PHI	R9
	CALL L0969
L09B7:
	BNZ	L09BC
	LDI	$00 ;D=0x00, string variable not used
	RETN
L09BC:
	INC	R12
	INC	R12
	LDI	$04 ;D=0x04,string variable used
	RETN
	
	;3 char string var
L09C1:
	GHI	R9
	SHL
	PHI	R9 ;form 1st char into bit shifted left format [ie. $41 (A) shifted to $82 signifying string]
	;R9.0 should contain 2nd char numeric char
	CALL VSEARCH_R9COMPARE ;L096C ;L096B
	BR L09B7
	
;----------------------------------------
	;array variable 
	;2024-attempted to speed up this routine with revising NUM2 and how it is used,
	;but failed to improve at this point.

VSEARCH_ARRAY:
	;set R9.1 = 00 and R9.0 = ID where ID is array variable ascii code
	GHI	R9
	PLO	R9
	LDI	$00
	PHI	R9
	;compare R9 to existing variables in storage.
	CALL VSEARCH_R9COMPARE ;returns 01 if variable used
	BNZ	VSEARCH_ARRAY2

VSEARCH_ARRAY1:
	LDI	$02 ;D=2 - illegal array
	RETN

VSEARCH_ARRAY2:
	;existing array found
	;skip over 2 bytes forming FP. (forward pointer=array length with low byte first)
	INC	R12
	INC	R12

	;get dimensions of full array
	;TODO: check if variable index being requested is within array dimensions. Not doing that for now
	LDA	R12
	PHI	R1 ;COL dimension
	LDA R12
	PHI R7 ;ROW dimension
	
	;R12 is now pointing to beginning of numeric array packed values (6 bytes per value).
	
	;get COL index of desired array value from program statement
	CALL ARRAY_INDEX ;COL returned in R9.0, call returns 00 if success
	LBNZ VSEARCH_ARRAY1 ;error from ARRAY_INDEX

	;now find appropriate location in array for given variable indices
	;NUM2 increments through the array based on variable indices
	;routine calls NUM2 once or twice depending on number of varable indices
	;enter from above with variable COL index value in R9.0, and array dimension COL in R7.1
VSEARCH_ARRAY3:
	CALL NUM2
	;check for 2nd index ROW value if comma in statement
	LDA	R10
	XRI	',' ;$2C 
	LBNZ L0989 ;return variable used flag??
	CALL ARRAY_INDEX ;variable index ROW in R9.0
	LBNZ VSEARCH_ARRAY1 ;error from ARRAY_INDEX
	LDI	$01
	PHI	R7
	LBR	VSEARCH_ARRAY3

;----------------------------------------
;L09F3:
ARRAY_INDEX:
	;getting array index value from statement and return via R9.0
	;status of operation is also returned via D: success=0
	;also be used by PLOT statement to get coordinate
	;2024 Note: there appears to be no check of variable index value to check if it exceeds 255
	;enter with R10 pointing to statement char after variable id
	LDN	R10
	SMI	$41
	BDF	ARRAY_INDEX1 ;L09FF ;branch if char is >='A' as a possible variable
	CALL NUMCON ;convert number in range of 0-255 and return via R9.0 as index value
	DEC R10
	LDI	$00 ;?flag to indicate success
	RETN
;L09FF:
ARRAY_INDEX1:
	;search for variable which contains index value
	GHI	R12
	PHI	R7
	GLO	R12
	PLO	R7
	CALL VARIABLE_SEARCH
	LBZ	VSEARCH_ARRAY1 ;L09D3 ;this returns 02 at subroutine calling point
	;copy packed num to scratch mem 
	CALL V2TEMP
	;setup scratch memory for unpacking variable
	;2024 Note: V2TEMP returns with R11 pointing to $8004...can that somehow be used instead of following R12 assignment?
	LDI HIGH(SCRATCHMEMB)
	PHI	R12
	LDI	$04
	PLO	R12
	CALL UNPACK
	SEX	R11
	GLO	R8 ;V2TEMP call put exponent in R8.0
	LBNZ ARRAY_INDEX3 ;L11E7
ARRAY_INDEX2: ;L11E2:
	LDI $00 ;GHI R4
	PLO	R9
	LBR	ARRAY_INDEX6 ;L0A2A
ARRAY_INDEX3: ;L11E7:
	SHL
	LBDF ARRAY_INDEX2 ;L11E2
	GLO	R8
	ADI	$01
	PLO	R11
ARRAY_INDEX4: ;L0A1D:
	STXD
	GLO	R11
	LBZ ARRAY_INDEX5 ;L0A26
	LDN	R11
	ADI	$30
	LBR ARRAY_INDEX4 ;L0A1D
ARRAY_INDEX5: ;L0A26:
	INC	R11
	CALL L0322
ARRAY_INDEX6: ;L0A2A:
	;LDN R1 ;??2024 - doesn't do anything
	GHI	R7
	PHI	R12
	GLO	R7
	PLO	R12
	LDI	$00
	RETN

;---------------------------------------------------
V2TEMP:
	;get a existing packed variable and copy to scratch memory
	;set R11 to point to scratch mem
	LDI	HIGH(SCRATCHMEMB)
	PHI	R11
V2TEMP1: ;L0A35:
	LDI	$01
	PLO	R11
	;get packed num sign and exponent bytes in R8
	LDA	R12
	PHI	R8
	LDA	R12
	PLO	R8
	;copy 4-byte packed numbers (R12) to scratch mem (R11)
	LDA	R12
	STR	R11
	INC	R11
	LDA	R12
	STR	R11
	INC	R11
	LDA	R12
	STR	R11
	INC	R11
	LDN	R12
	STR	R11
L0A47:
	RETN

;-------------------------------------------
NUM2:
	;reserve numeric storage for dimensioned array
	;R7.1 = 1st dimension value
	;R9.0 = 2nd dimension value ... will equal 1 if only one dimension array
	DEC	R9

NUM2_1:
	GLO	R9
	BNZ NUM2_2
	RETN
	
NUM2_2:
	GHI	R7 ;size of 1st array dimension
	PLO	R7

NUM2_3:
	;reserve 6 byte value for each dimension value
	GLO	R7
	LBZ	NUM2
	INC	R12
	INC	R12
	INC	R12
	INC	R12
	INC	R12
	INC	R12
	DEC	R7
	BR NUM2_3

;-------------------------------------------
NUM_ARRAY_CREATE:
	;This routine used by DIM statement to create zeroed out numerical array of COL:ROW dimensions
	
	;enter with R12 pointing to start of memory to be reserved
	;for dimensioned array of size COL:ROW.
	;R9.0 = ROW and R7.0 = COL
	
	;reserve 6 bytes for each packed number and zero them out -
	;sign byte, exponent byte, and 4-byte packed number

	GLO R7
	PHI R7

NUM_ARRAY_CREATE_1:
	GLO	R9
	BNZ NUM_ARRAY_CREATE_2
	RETN
	
NUM_ARRAY_CREATE_2:
	GLO	R7 ;COL
	LBZ	NUM_ARRAY_CREATE_3
	LDI $00
	STR R12
	INC	R12
	STR R12
	INC	R12
	STR R12
	INC	R12
	STR R12
	INC	R12
	STR R12
	INC	R12
	STR R12
	INC	R12
	DEC	R7
	BR NUM_ARRAY_CREATE_2

NUM_ARRAY_CREATE_3:
	DEC	R9 ;ROW
	GHI R7
	PLO R7
	LBR NUM_ARRAY_CREATE_1


;-------------------------------------------

UPDATE_END_VARIABLE_STORAGE:
	;stores address of end of variable storage (EVS).
	;pass in EVS address via RC
	
;	;routine uses R7, so save and restore R7 in case being used in calling code
;	GLO R7
;	STXD
;	GHI R7
;	STR R2
	
	LDI HIGH (ENDOFVARSTORAGE)
;	;PHI R7
	PHI R15
	LDI LOW (ENDOFVARSTORAGE)
;	;PLO R7
	PLO R15
;	;GHI RF
	GHI R12
;	STR R7
;	INC R7
	STR R15
	INC R15
;	;GLO RF
	GLO R12
;	STR R7
	STR R15
	
;	;restore R7
;	LDXA
;	PHI R7
;	LDX
;	PLO R7
	RETN

;-------------------------------------------
;DIMENSION numeric variable statement

;current code only allows for 1 or 2 dimensions numeric variables with single ascii char identifier
;such as: A(10), K(2,255)

;2024: added call to ARRAY_INDEX routine to get numeric or variable dimensions
;arrays are not zero base, starts with A(1) or A(1,1)

;cannot re-dimension an existing array...issue error code 18 if attempted.

;2024: exmpl storage structure of arrayed number: A()= 00 41 FP COL ROW xxxxxxxxxxx
;2024: added ROW to structure
;	where: ?? 1st byte = 00 to indicate array, and 2nd byte 41-5A as variable ID
;	and: FP is forward pointer (2 bytes - low byte first) to end of array
;		 COL is column or 1st dimension
;		 ROW is row or 2nd dimension

DIM:
	LDA	R10
	;check for alpha char and if it is, branch to setup for variable search
	SMI	$41
	BDF DIM2
	;check for  end of line CR
	XRI	$CC ;$0D
DIM1:
	LBZ	SUBROUT3
	;check for end-of-statement colon
	XRI	$F6 ;':'
	BZ DIM1
	BR DIM ;get next char in DIM statement

DIM2:
	;encountered alpha char specifying array name, save to R9 to compare to any existing array variable of same name
	LDI	$00
	PHI	R9
	DEC	R10
	LDA	R10
	PLO	R9 ;R9.0 = variable ID for variable search
	INC	R10
	CALL VSEARCH_R9COMPARE ;L096C ;call code segment in VARIABLE_SEARCH
	BZ	DIM3

	CALL ERRCODE
	BYTE $12 ;error 18...attempting to re-dimension existing numeric array
	RETN

DIM3:
	;R12 pointing to next available variable storage slot as set by variable search call
	STR	R12 ;store 00
	INC	R12
	GLO	R9
	STR	R12 ;store variable ID
	INC	R12
	
	;get 1st (COL) dimension of array
	;NUMCON converts text number (0-255) to binary which is returned via R9.0
	;CALL NUMCON
	CALL ARRAY_INDEX ;2024 - should return index in R9.0
	GLO R9
	PLO R7 ;COL
;	PHI	R7
	;R7.0 now holds size of COL dimension value
	
	;checking if a 2nd dimension value exists
;	DEC	R10
	LDA	R10
	XRI	',' ;$2C
;DIM4:
	BZ DIM5

	;if no comma, then no 2nd index.  Store 01 to R9.0 for one dimensional array
	LDI	$01
	PLO	R9
	BR DIM6

DIM5:
	;get 2nd dimension of array since ',' encountered in statement.  limit is 2 dimensions for array
;;	CALL NUMCON
	CALL ARRAY_INDEX ;2024 - should return 2nd index in R9.0


DIM6:
;	GLO R9
;	PHI	R9
;	;R9.1 now holds size of 2nd dimension value, or it holds $01 ??
	;R9.0 now holds ROW or 2nd dimension value, or it holds $01 if one dimension array
	
	;high bytes or R7 and R9 used to store dimensions since high bytes will be copied to low bytes of same
	;registers to be used as loop counters in NUM2 routine
	
	;copy R12 to R11.  R11 pointing to bytes to store F.P. when calculated
	GHI	R12
	PHI	R11
	GLO	R12
	PLO	R11
	INC	R12
	INC R12
	
	GLO R7
	STR R12 ;store array COL dimension
	INC	R12
	GLO R9
	STR R12 ;store array ROW dimension
 	INC	R12
	;R12 now pointing to 4 bytes after variable ID stored 
	
	;call routine to size the variable storage based on R9.0 and R7.1 values
	;this is sizing only for numeric values (6 bytes per packed number)
	;what about string variables????
	CALL NUM_ARRAY_CREATE
	GHI	R12
	SMI	RAM_LIMIT ;$1F
	LBDF L0262
	LDI	$04 ;store marker for new end of variable storage
	STR	R12
	CALL UPDATE_END_VARIABLE_STORAGE ;saves R12 address

	;calculate length of array and store as forward pointer (F.P. low byte first)
	GLO	R11
	STR	R2
	GLO	R12
	SM
	STR	R11
	GHI	R11
	STR	R2
	INC	R11
	GHI	R12
	SMB
	STR	R11
	
	;store 2nd array dimension
	;INC	R11
	;GHI	R9
	;STR	R11
	LBR	DIM ;L0A5A



;------------------------------------
;L0AB5:
PRINT:
	LDA	R10
	XRI	$0D ;CR - on CR, advance term screen one line and exit print command
	BNZ	L0AC0
L0ABA:
	CALL LINEADVANCE
L0ABD:
	LBR SUBROUT3
L0AC0:
	XRI	$37 ;':' - on colon, advance term screen one line and exit print command
	BZ	L0ABA
	XRI	$18 ;'"' 
	LBNZ L0AD6
L0AC8:
	;encountered first quote, so output text to term until next quote
	LDN	R10
	XRI	$22 ;'"' - on second quote, branch to start of print command
	LBNZ L0AD0
	INC	R10
	LBR PRINT
L0AD0:
	LDA	R10
	CALL TERMSND ;sending chars between quotes to term
	LBR L0AC8
L0AD6:
	;outside of quotes
	XRI	$02 ;' ' - on space, get next char in print command
	LBZ PRINT
	XRI	$0C ;',' - on comma, send horizontal tab to terminal
	LBNZ L0B01
	;LDI	$20 ;...think this commented out code had something to do with original video board
	;STR	R14 
	;GLO	R14
	;ANI	$F0
	;ADI	$10
	;PLO	R14
	;GHI	R14
	;ADCI $00
	;PHI	R14
	;LDI	$20
	LDI $09 ;horizontal tab
	CALL TERMSND
L0AF1:
	;???need to see if following can be more efficiently handled
	LDA	R10
	XRI	$20 ;' ' - skip spaces outside quotes
	BZ	L0AF1 
	XRI	$2D ;CR - on CR, exit print command
L0AF8:
	LBZ	L0ABD
	XRI	$37 ;':' - on colon, exit print command
	LBZ	L0ABD
	LBR	L0B17
L0B01:
	XRI	$17 ;';' - on semi colon, test next char in print statement
	LBZ	L0AF1
	XRI	$18 ;'#' - not sure of original intent...possible moving cursor to specific position in screen line
	LBNZ L0B1B
	;commented out code possibly for original video board.
	;revised code to use VT100 escape sequence to position cursor to specific char position in line
	;LDI	$20
	;STR	R14
	;CALL NUMCON
	;GLO R14
	;ANI	$C0
	;STR	R2
	;GLO	R9
	;ADD
	;PLO	R14
	CALL NUMCON ;convert decimal number (0-255) following '#' to byte (R9.0) for use in vt100 escape sequence
	;send beginning of horizontal positioning ESC command ... ESC[colH
	CALL SNDCHAR
	BYTE $1B,'[',$00
	GLO R9 ;sent horizontal position byte to term
	CALL TERMSND ;using TERMSND since is saves R12 and R13 in case they are being used
	CALL SNDCHAR
	BYTE 'H',$00
L0B17:
	DEC	R10
L0B18:
	LBR	PRINT ;L0AB5

L0B1B:
	XRI	$06 ;'%' - send byte to terminal (convert  0-255 decimal to byte)
	LBNZ L0B29
	CALL NUMCON
	GLO R9
	DEC	R10
	CALL TERMSND
	LBR L0B18

L0B29:
	;encountered char that is nonspace or not control char...assume variable encountered
	DEC	R10
	CALL VARIABLE_SEARCH
	XRI	$01
	LBZ	L0B3A
L0B31:
	XRI	$05 ;{string}
	LBZ	L0B4C
L0B35:
	CALL ERRCODE
	BYTE $05
	RETN
L0B3A:
	CALL V2TEMP
	LDI HIGH(SCRATCHMEMB)
	PHI	R12
	LDI LOW(SCRATCHMEMB)+$04
	PLO	R12
	CALL UNPACK
	CALL OUTPUT
L0B49:
	LBR	PRINT
L0B4C:
	LDA	R12
	BZ L0B49
	CALL TERMSND
	BR L0B4C
	
;------------------------------------------------------	
; Variable setup - determine if variable used or not used
; and set pointers
VARIABLE_SETUP:
	CALL VARIABLE_SEARCH
	BNZ	VARIABLE_EXISTS ;L0B7C ;branch if variable already exists
L0B59:
	;variable not used yet
	GHI	R9 ;R9.1 contains variable id - exmpl 41 for numberic A, or 82 for string A
	STR	R12 ;R12 points to 1st byte of new variable slot
	INC	R12
	SHL ;sets 1 to DF if R9.1 is string id
	GLO	R9
	STR	R12
	INC	R12 ;R12 pointing to 3rd byte of variable slot
L0B60:
	LDI HIGH(VARPOINTER)
	PHI	R7
	LDI	LOW(VARPOINTER)
	PLO	R7
	;storing address of var slot 3rd byte to R7 
	GHI	R12
	STR	R7
	INC	R7
	GLO	R12
	STR	R7
	BDF	L0B7B ;branch if variable is string
	;scalar numeric...insert $04 at end of var slot
	INC	R12
	INC	R12
	INC	R12
	INC	R12
	INC	R12
	INC	R12
	LDI	$04
	STR	R12
	CALL UPDATE_END_VARIABLE_STORAGE ;save R12 address
	;check if placing variables beyond RAM limit
	GHI	R12
	SMI	RAM_LIMIT
	LBDF L0262 ;terminate if exceeded
L0B7B:
	RETN ;return with VARPOINTER (R7) setup for value entry

;-------------------------------------------------------------------------
	;branch here if it was determined that variable already exists in storage - can be numeric scalar, array, or string
;L0B7C:
VARIABLE_EXISTS:
	XRI	$02 ;illegal array
	BNZ	L0B85
	CALL ERRCODE
	BYTE $0E
	RETN
L0B85:
	XRI	$03 ;scalar
	LBNZ L0B8E
L0B89:
	LDI	$01
	SHR
	LBR L0B60


	;2024 rewrite:
	;handling situation where existing string variable is being reassigned to a new value.
	;since length of string value can be changing, method is to delete existing string variable
	;by writing over the string variable with all other tailing variables residing in storage.
	;the string variable being replaced is then appended to end of variable storage.
L0B8E:
	;calculate end of string address using string length bytes stored in variable structure
	SEX	R12
	DEC	R12
	DEC	R12
	GLO	R12
	ADD
	PLO	R11
	GHI	R12
	INC	R12
	ADC
	PHI	R11
	;R11 now points to beginning of tailing variable storage at end of string variable being erased
	DEC	R12
	DEC	R12
	DEC	R12
	;R12 now points to start of string variable to be erased

	;2024 rev. - since end of variable storage $04 narker could exist
	;as variable exponent, couldn't use that to find end of storage.
	;handled by adding ENDVARSTORAGE pointing to reserved memory
	;where address of end of var storage is maintained.
	LDI LOW (ENDOFVARSTORAGE)
	PLO R7
	LDI HIGH (ENDOFVARSTORAGE)
	PHI R7
	SEX R7
	;R7 pointing to hi byte of EVS address
	
	;begin loop copying trailing variables over string thereby erasing it
DELETE_STRING_VAR:
	LDA	R11
	STR	R12
	INC R12
	;compare R11 to M(R7) to see if at end of storage
	GHI R11
	SM
	BNF DELETE_STRING_VAR ;branch if R11.1 LT Hi EVS
	INC R7 ;lo EVS address
	GLO R11
	SM
	LBDF DELETE_STRING_VAR1
	DEC R7 ;hi EVS address
	LBR DELETE_STRING_VAR
DELETE_STRING_VAR1:
	SEX R2
	LBR L0B59 ;branch to append string variable to end of storage

;------------------------------------------------
;L0BA6:
	;begin search in pgm text for variable (numeric or string)
	;followed by assingment char '=',
	;followed by arithmetic or char assigment statement
	;R10 points to current char in pgm text line
LET:
	 ;skip spaces
	LDA	R10
	XRI	$20
	BZ LET
	
	;first non-space is assumed to be variable
	DEC	R10
	CALL VARIABLE_SETUP
	
	;return if error in variable setup
	LDI HIGH(ERROR_CODE)
	PHI	R7
	LDI	LOW(ERROR_CODE)
	PLO	R7
	LDN	R7
	LBNZ L0BC4
	
	;R10 points to char after var, skip spaces, looking for '='
L0BB7:
	LDA	R10
	XRI	$20 ;' '
	LBZ	L0BB7
	XRI	$1D ;'='
	LBZ	L0BC5
	XRI $14 ;')'
	LBZ	L0BB7

	;unexpected char encountered, set error code
	CALL ERRCODE
	BYTE $0B
L0BC4:
	RETN
	
	;found '='
L0BC5:
	GHI	R9 ;R9.1 should have been set by VARIABLE_SETUP
	SHL
	LBDF STRING ;string var if DF=1
	
	;code falls through to AOS if number and not string
	;---------------------------------------
	;AOS - arithmetic operating system - routine to solve mathematical expressions.
	;Starts on leftmost char of expression and systematically checks
	;char groups as to whether they are alpha functions, numeric, or operator 
	;characters.

	;Lookup tables are used to determine operator chars and point program
	;pointer to appropriate execution routine for that operator or function. Tables also
	;contain a hierarchy value for that particular operator/function.

	;R13: arithmetic stack pointer
	;R2 : general purpose memory stack
	;R10: text pointer for BASIC statement being evaluated.
	;R7.0: hierarchy/priority byte - determines which operand occurs before others
	;      01 lowest, 04 highest (01,02,03,04)
	;R8 : used to point to operand lookup table

;L0BCA
AOS:
	;R10 points to first char after = sign during statement
	;initialize R13 to point to top of arithmetic stack
	CALL INIT_ARITHSTACK_POINTER
AOS1:
	;hierarchy byte zeroed out and also stored to mem stack
	LDI $00
	PLO R7
	STXD
	SKP

L0BD1:
	INC	R10
L0BD2:
	LDN	R10
	XRI	$20 ;skip spaces
	LBZ	L0BD1
	;2024 mod for hex number indicator char '&'
	XRI $06 ;'&'
	LBZ L0BE0
	;determine if non-space is number or non-number
	LDN	R10
	SMI	$30
	LBNF L0BED ;branch if non-number less than ascii 0
	SMI	$0A
	BDF	L0BE5 ;branch if non-number greater than ascii 9
	
L0BE0:
	;R10 is pointing to beginning of number
	;get the number and store to arithmetic stack (temp value at NUMA)
	CALL GETNUM
	LBR	L0BD2 ;branch and eval next R10 char

L0BE5:
	;entered here with non-number on high side
	SMI	$07 ;check for alpha char?
	LBDF L0C8B ;alpha char branch - use operand table 2 for arith function match
	LBR	L0C83 ;non-alpha char so use OPERAND_TABLE1
	
L0BED:
	;R10 char is non-number low
	XRI	$FE ;'.'
	BZ	L0BE0 ;decimal number, go get
	XRI	$03 ;'-'
	LBNZ L0C1A
	
	;assess whether '-' is operator or num sign
	DEC	R10
	LDA	R10
L0BF8:
	XRI	$28 ;'('
	LBNZ L0C2F
	;encountered '(-'
	INC	R10
	LDN	R10 ;checking '(-#'
	DEC	R10
	SMI	$41
	LBNF L0BE0 ;go get neg number
	
	;# was alpha, so it is a variable
	INC	R10
	CALL VAR1
	GHI R2 ;HIGH(BASICMEMSTACK) ???check the GHI R2
	PHI	R15
	LDI	$00
L0C0D:
	PLO	R15
	LDN	R15
	LBNZ L0C42 ;checking for error code
	INC	R13
	LDN	R13
	XRI	$80
	STR	R13
	DEC	R13
	LBR	L0BD2
	
L0C1A:
	;check for left paren
	XRI	$05 ;'('
	LBNZ L0C25
	GLO	R7
	ADI	$10
	PLO	R7
L0C22:
	LBR	L0BD1
	
L0C25:
	;check for right paren 
	XRI	$01 ;')'
	LBNZ L0C2F
	GLO	R7
	SMI	$10
	PLO	R7
	LBR	L0C22
	
L0C2F:
	;table lookup for operand
	LDI	HIGH(OPERAND_TABLE)
	PHI	R8
	LDI	LOW(OPERAND_TABLE)
	PLO	R8

L0C35:
	LDI	$00
	SHL
	;copy R10 operand to be tested to R9.1
	LDA	R10
	PHI	R9

L0C3A:
	SEX	R8 ;R8 is X

L0C3B:
	;check if at end of table, if so issue error
	LDN	R8
	BNZ	L0C43
	CALL ERRCODE
	BYTE $06
L0C42:
	RETN
	
L0C43:
	;entry to table comparison, R8 table origin specified at various locations
	GHI	R9
	XOR
	INC	R8
	BNF	L0C4D
	BNZ	L0C4C ;not a match, increment to next table operand

	GLO	R9 ;R9.0 comes into play if using OPERAND_TABLE2
	XOR
L0C4C:
	INC	R8 ;if OPERAND_TABLE2, need to increment xtra byte
	
L0C4D:
	;OPERAND_TABLE and OPERAND_TABLE1 test
	BZ L0C54
	INC	R8
	INC	R8
	INC	R8
	BR L0C3B
L0C54:
	;OPERAND_TABLE and OPERAND_TABLE1 operand match
	;OPERAND_TABLE2 match also branches to here.
	;copy operand address to R11
	LDA	R8
	PHI	R11
	LDA	R8
	PLO	R11
	
	GLO	R7
	ADD  ;R8 is still X and pointing to hierarchy byte in table at matching operand
	PHI	R7 ;add matching hierarchy flag to hierarchy
	
	;compare hierarchy to previous R2 stored hierarchy
	SEX	R2 ;R8 was X, now R2 is X
	INC	R2
L0C5D:
	GHI	R7
	SD
	LBNF L0C74

L0C61:
	;current operand hierarchy less than or equal to previous,
	;so perform previous operand function and store current operand
	INC	R2
	LDA	R2
	PHI	R9
	LDN	R2
	PLO	R9
	GLO	R11
	STXD
	GHI	R11
	STXD
	CALL PERF
	INC	R2
	LDA	R2
	PHI	R11
	LDA	R2
	PLO	R11
	LBR L0C5D

L0C74:
	;current operand hierarchy greater than previous,
	;so store operand perform address followed by hierarchy to R2
	DEC	R2
	GLO	R11
	STXD
	GHI	R11
	STXD
	GHI	R7
	STXD
	XRI	$01
	LBNZ L0BD2
	INC	R2
	LBR L0C61
	
L0C83:
	;point R8 to OPERAND_TABLE1
	LDI	HIGH(OPERAND_TABLE1) ;$0C
	PHI	R8
	LDI	LOW(OPERAND_TABLE1) ;$E7
	PLO	R8
	LBR L0C35

L0C8B:
	;3/23/24 - this is where to modify old code to use new 3 char match
	;in operand table 2
	
	;enter with R10 pointing to first alpha char...could be arith func or variable
	;check next char in text to see if alpha. if not then assume it is variable
	INC	R10
	LDN	R10
	DEC	R10
	SMI	$41
	LBNF	L0CA7
	
	;next char was alpha so start going through operand table 2 for potential match
	LDI	HIGH(OPERAND_TABLE2) ;$0C
	PHI	R8
	LDI	LOW(OPERAND_TABLE2) ;$F8
	PLO	R8

	;set R9 as X and compare command table to text for match of chars
	SEX R9
	
OPERTBL2_0:
	;copy R10 to R9 as temporary pointer to text position
	GHI R10
	PHI R9
	GLO R10
	PLO R9
	
	;check for 00 byte at end of opertbl2
	LDN R8
	BNZ OPERTBL2_1
	CALL ERRCODE
	BYTE $06
	RETN

OPERTBL2_1:
	LDA R8
	LBZ OPERTBL2_3 ;check for 00 byte at end of operand in opertbl2 
	XOR
	INC R9
	BZ OPERTBL2_1 ;if char match, test next char
	
	;char not match, then reset text pointer and move to next operand in table
OPERTBL2_2:
	;find 00 at end of tbl operand
	LDA R8
	BNZ OPERTBL2_2
	;found 00 byte, skip next 3 bytes in tbl to get to next test operand
	INC R8
	INC R8
	INC R8
	LBR OPERTBL2_0

	;text operand and tbl operand match, load execution address and test hierarchy
OPERTBL2_3:
	;copy R9 to R10
	GHI R9
	PHI R10
	GLO R9
	PLO R10
	;advance R10 through any remaining alpha chars
L0C9C:
	LDA	R10
	SMI	$41
	BDF	L0C9C
	DEC	R10
	;copy operand address to R11 (?L0C54)
	SEX R8
	LBR L0C54


	;############################################################################
	;old operand table 2 code started here
	;INC	R10
	;LDN	R10
	;DEC	R10
	;SMI	$41
	;BNF	L0CA7
	;LDI	HIGH(OPERAND_TABLE2) ;$0C
	;PHI	R8
	;LDI	LOW(OPERAND_TABLE2) ;$F8
	;PLO	R8
	;table uses two chars for operand
	;copy 1st char to R9.1, 2nd char to R9.0
	;LDA	R10
	;PHI	R9
	;LDA	R10
	;PLO	R9
;L0C9C:
	;advance R10 through any remaining alpha chars
	;LDA	R10
	;SMI	$41
	;BDF	L0C9C
	;LDI	$01
	;SHR
	;DEC	R10
	;BR L0C3A
	
	;############################################################################

	;keep this for new code
L0CA7:
	;encountered assumed variable
	CALL VAR1
	LDI HIGH(ERROR_CODE) ;GHI R2
	PHI	R9
	LDI LOW(ERROR_CODE) ;$00
	PLO	R9
	LDN	R9
	LBZ	L0BD2
	RETN

;----------------------------------------------
;allows transfer of R9 to R3 and continue program execution from there
;R9 was set with operand address in L0C61 code segment
;L0CB4
PERF:
	;GHI R3
	LDI HIGH(PERF1)
	PHI	R8
	;LDI $BA
	LDI LOW(PERF1)
	PLO	R8
	SEP	R8
PERF1:
	GHI	R9
	PHI	R3
	GLO	R9
	PLO	R3
	SEP	R3

;------------------------------------------------
;get variable value and transfer to arithmetic stack
;L0CBF
VAR1:
	CALL VARIABLE_SEARCH
	LBZ	L0B35 ;Error
L0CC5:
	LDA	R12
	PHI	R8
	LDA	R12
	PLO	R8
	INC	R12
	INC	R12
	INC	R12
	LBR	COPY_TO_ARITHSTACK

;-----------------------------------------------
;L0CCF
OPERAND_TABLE:
	; operand-to-match,high execute addr,low execute addr, hierarchy byte
	BYTE '\r',HIGH(TERMINATE),LOW(TERMINATE),$01
	BYTE '+',HIGH(ADD),LOW(ADD),$02 
	BYTE '-',HIGH(SUB),LOW(SUB),$02 
	BYTE '*',HIGH(MULT),LOW(MULT),$03 
	BYTE '/',HIGH(DIV),LOW(DIV),$03 
	BYTE '^',$00,$00,$04 ;not implemented
OPERAND_TABLE1:
	BYTE ':',HIGH(TERMINATE),LOW(TERMINATE),$01 
	BYTE '=',HIGH(TERMINATE1),LOW(TERMINATE1),$01 
	BYTE '>',HIGH(TERMINATE1),LOW(TERMINATE1),$01 
	BYTE '<',HIGH(TERMINATE1),LOW(TERMINATE1),$01
	BYTE $00
OPERAND_TABLE2:
	;3/8/24 modify table to match chars until 00 byte to identify these operands
	BYTE 'TO',$00,HIGH(TERMINATE),LOW(TERMINATE),$01 ;TO?
	BYTE 'THE',$00,HIGH(TERMINATE1),LOW(TERMINATE1),$01 ;THEN 
	BYTE 'STE',$00,HIGH(TERMINATE),LOW(TERMINATE),$01 ;STEP? STOP?
	BYTE 'INT',$00,HIGH(INT),LOW(INT),$04
	BYTE 'RND',$00,HIGH(RND),LOW(RND),$04
	BYTE 'SGN',$00,HIGH(SGN),LOW(SGN),$04
	BYTE 'ABS',$00,HIGH(ABS),LOW(ABS),$04
	BYTE 'KEY',$00,HIGH(KEY),LOW(KEY),$01 ;retrieves serial term keypress from INKEY function
	BYTE 'SQR',$00,HIGH(SQR),LOW(SQR),$04
	BYTE 'PI',$00,HIGH(PI),LOW(PI),$05
	BYTE 'SIN',$00,HIGH(SIN),LOW(SIN),$04
	BYTE 'COS',$00,HIGH(COS),LOW(COS),$04
	BYTE 'TAN',$00,HIGH(TAN),LOW(TAN),$04
	
	BYTE 'PEE',$00,HIGH(PEEK),LOW(PEEK),$05
	
	;following operands have not been implemented yet
	BYTE 'LN',$00,$00,$00,$04
	BYTE 'LOG',$00,$00,$00,$04
	BYTE 'EXP',$00,$00,$00,$04
	;BYTE $00,$00,$00,$00,$00
	BYTE $00

;---------------------------------------------------------
; sets R13 to point to bottom of arithmetic stack
;L0D4E
INIT_ARITHSTACK_POINTER:
	LDI	HIGH(ASTCKMEM) ;$13
	PHI	R13
	LDI	LOW(ASTCKMEM) ;$5F
	PLO	R13
	RETN

;--------------------------------------------------------
;L0D55
COPY_ARITHSTACK_TO_VARIABLE:
	LDI	HIGH(VARPOINTER) ;GHI R2
	PHI	R7
	LDI	LOW(VARPOINTER) ;$05
	PLO	R7
	LDA	R7
	PHI	R12
	LDN	R7
	PLO	R12
	INC	R13
L0D5F:
	LDA	R13
	STR	R12
	INC	R12
	LDA	R13
	STR	R12
	INC	R12
	LBR	FSTCK0 ;L04AD

;-------------------------------------------------------
;Performs termination of arithmetic operands
;such as storing arithmetic stack value to variable
;When this routine encounters a D5 return from SUBROUT3,
;R3 program execution continues inline from the original
;CALL to AOS. R6 is retrieved from mem stack and points to
;to code after call to AOS.

;L0D68
TERMINATE:
	CALL COPY_ARITHSTACK_TO_VARIABLE
TERMINATE1:
	INC	R2
	INC	R2
	INC	R2
	LDXA
	PLO	R6
	LDXA
	PHI	R6
	;LBR L0E0E ;MOVED CODE AFTER STATEMENT TABLE TO HERE
	;L0E0E:	
	INC	R2
	INC	R2
	LBR	SUBROUT3 ;L07DF

;---------------------------------
; table of basic statement commands
COMMAND_TABLE:
	BYTE 'LET',$00,HIGH(LET),LOW(LET)
	BYTE 'PRI',$00,HIGH(PRINT),LOW(PRINT)
	BYTE 'GOS',$00,HIGH(BGOSUB),LOW(BGOSUB)
	BYTE 'RET',$00,HIGH(RETURN),LOW(RETURN)
	BYTE 'NEX',$00,HIGH(NEXT),LOW(NEXT)
	BYTE 'IF',$00,HIGH(IF_THEN),LOW(IF_THEN)
	BYTE 'FOR',$00,HIGH(FOR),LOW(FOR)
	BYTE 'GOT',$00,HIGH(BGOTO),LOW(BGOTO)
	BYTE 'SET',$00,HIGH(SET),LOW(SET)
	BYTE 'IPT',$00,HIGH(IPT),LOW(IPT)
	BYTE 'REA',$00,HIGH(READ),LOW(READ)
	BYTE 'DAT',$00,HIGH(REM),LOW(REM) ;using same address as REM
	BYTE 'DIM',$00,HIGH(DIM),LOW(DIM)
	BYTE 'INP',$00,HIGH(INPUT),LOW(INPUT)
	BYTE 'END',$00,HIGH(END),LOW(END) ;$08,$74
	BYTE 'REM',$00,HIGH(REM),LOW(REM) ;$08,$E2
	BYTE 'RES',$00,HIGH(RESTORE),LOW(RESTORE)
	BYTE 'STO',$00,HIGH(STOP),LOW(STOP) ;$08,$EE
	BYTE 'CLS',$00,HIGH(CLS),LOW(CLS)
	BYTE 'OPT',$00,HIGH(OPT),LOW(OPT)
	BYTE 'TST',$00,$00,$00 ;NOT IMPLEMENTED
	BYTE 'CAL',$00,HIGH(BCALL1),LOW(BCALL1)
	BYTE 'FIX',$00,HIGH(FIX),LOW(FIX)
	BYTE 'ON',$00,HIGH(ON_GOTO),LOW(ON_GOTO)
	BYTE 'INKEY',$00,HIGH(INKEY),LOW(INKEY) ;added 5/50/21
	BYTE 'POKE',$00, HIGH(POKE),LOW(POKE) ;added 7/23/24
	BYTE 'PEEK',$00, HIGH(PEEK),LOW(PEEK) ;added 8/28/24
	BYTE $00


;---------------------------------------------------
;copies chars between "" to string variable
S2VPNT:
	;GHI R2
	LDI HIGH(VARPOINTER)
	PHI	R7
	LDI	LOW(VARPOINTER) ;$05
	PLO	R7
	LDA	R7
	PHI	R8
	PHI	R11
	LDN	R7
	PLO	R8
	PLO	R11
	;R8 & R11 now point to 3rd byte of var slot (3rd=F., 4th=P.)
	INC	R11
	INC	R11 ;R11 points to 5th byte of var slot
L0E20:
	LDA	R10 ;pointing to program text
	XRI	$20 ;' '
	LBZ	L0E20 ;skip leading spaces
	XRI	$02 ;'"'
	LBNZ L0E4B
L0E29:
	;encountered first double quote, transfer chars to var until second double quote
	LDN	R10
	XRI	$22 ;'"'
	LBZ	L0E33
	LDA	R10
	STR	R11
	INC	R11
	LBR L0E29
L0E33:
	STR	R11 ;put 00 (result from XRI) at end of string
	INC	R11
L0E35:
;	LDI	$04
;	STR	R11 ;put $04 at end of variable
	
	GHI	R11
	SMI	RAM_LIMIT ;BASICBASEHIGH+$1F ; RAM LIMIT
	LBDF L0262 ;branch if exceeding RAM
	
	;07/03/24 REV
	LDI	$04
	STR	R11 ;put $04 at end of variable
	GHI R11
	PHI R12
	GLO R11
	PLO R12
	CALL UPDATE_END_VARIABLE_STORAGE
	
	GLO	R8
	STR	R2
	GLO	R11
	SM ;D = R11.0 minus R8.0
	;calc length of string and store to F.P. bytes of var
	STR	R8; store D at var F. 3rd slot
	GHI	R8
	STR	R2
	INC	R8
	GHI	R11
	SMB ;D = R11.1 minus R8.1
	STR	R8 ;store D at var P. 4th slot
	INC	R10
	RETN
L0E4B:
	DEC	R10
	CALL VARIABLE_SEARCH
	LBZ	L0B35 ;ERROR
L0E52:
	LDA	R12
	STR	R11
	INC	R11
	LBNZ L0E52
	DEC	R10
	LBR	L0E35
	
;--------------------------------------------------	
;L0E5A:
STRING:
	CALL S2VPNT
	LBR REM ;using REM to find end of statement (CR)

;-------------------------------------------------
; When reading R10 chars:
;    skips past space, comma
;    return on CR, colon, or any other char not targeted
;    send to display terminal all chars between double quotes
;L0E60:
SKIPPER:
	INC	R10
	LDN	R10 ;entry point into this routine??
	XRI	$20 ;' '
	BZ SKIPPER ;skip space
	XRI	$2D ;CR
	LSZ ;return on CR
	XRI	$37 ;':'
L0E6B:
	BZ L0E81
	XRI	$18 ;'"'
	BNZ L0E7D
	INC	R10
L0E72:
	LDN	R10
	XRI	$22 ;'"'
	BZ SKIPPER ;L0E60
	LDA	R10
	CALL TERMSND
	BR L0E72
L0E7D:
	XRI	$0E ;','
	BZ SKIPPER ;L0E60
L0E81:
	RETN

;---------------------------------------------------
;routine to input keyboard to a variable...numeric or string
;if string variable, then input chars need to be enclosed by quotes..ie. "abc"
INPUT:
	CALL SKIPPER+1
L0E85:
	LBZ SUBROUT3 ;L07DF ;SKIPPER returns zero if targeted byte caused return 
	;display prompt for input
	CALL SNDCHAR
	BYTE $3F,$20,$00
	;get keyboard input to line buffer storage zone
	CALL LINEBUFFINPUT
	;temp save of start address of line buffer
	LDI HIGH(LINE_BUFB)
	STXD
	LDI	LOW(LINE_BUFB) ;$60
	STXD
L0E96:
	;??setup pointers to existing variable or new variable
	CALL VARIABLE_SETUP
	;save R10 to R7
	GHI	R10
	PHI R7
	GLO	R10
	PLO	R7
	;load save line buffer address to R10
	INC	R2
	LDA	R2
	PLO	R10
	LDN	R2
	PHI	R10
	;check for error code 
	LDI HIGH(ERROR_CODE)
	PHI	R15
	LDI LOW(ERROR_CODE)
	PLO	R15 ;R15-->$1400
	LDN	R15 ;get error code
	LBNZ L0E81 ;if error exists, return from INPUT
	;
	GHI	R9 ;R9.1 contains numeric or string var id
	SHL
	LBDF L0ED1 ;branch if string
	;
	CALL INIT_ARITHSTACK_POINTER
	CALL SKIPPER+1
	BNZ	L0EBA
L0EB5:
	CALL ERRCODE
	BYTE $0A
	RETN
L0EBA:
	CALL GETNUM
	CALL COPY_ARITHSTACK_TO_VARIABLE
L0EC0:
	GHI	R10
	STXD
	GLO	R10
	STXD
	GHI	R7
	PHI	R10
	GLO	R7
	PLO	R10
	CALL SKIPPER+1
	LBNZ	L0E96
	INC	R2
	INC	R2
	LBR	L0E85
L0ED1:
	;string variable detected
	;SKIP2 transfers input text between "" to var slot
	CALL SKIP2
	LBZ	L0EB5
	LBR	L0EC0

;------------------------------------------------------
; SKIP2 is used by INPUT and READ statements
; R10 char pointer:
;   skip space, comma
;   return on CR, colon
;   on double quotes, move string to var pointer??
;L0ED8:
SKIP2:
	LDA	R10
	XRI	$20 ;SPACE
	LBZ SKIP2 ;skip space L0ED8
	XRI	$2D ;CR
	LSZ	
	XRI	$37 ;':'
L0EE2:
	LBZ L0EF2 ;colon signals end of statement
	XRI	$18 ;'"'
	LBZ L0EEC ;double quote is beginning of string to sent to var
	XRI	$0E ;','
	LBZ SKIP2 ;L0ED8
L0EEC:
	DEC	R10
	CALL S2VPNT
	LDI	$01
L0EF2:
	RETN
	
;-------------------------------------------
;L0EF3
READ:
	;GHI R2
	LDI HIGH(DATAPOINTER)
	PHI	R7
	LDI	LOW(DATAPOINTER)
	PLO	R7
	CALL INIT_ARITHSTACK_POINTER
L0EFB:
	CALL SKIPPER+1
	LBZ	SUBROUT3 ;this terminates this function
	CALL VARIABLE_SETUP

	;check for error
	LDI $00
	CALL L00BB
	LSZ
	RETN ;return if error set
	NOP
	
	;storing current R10 statement text location for later retrieval
	GHI	R10
	STXD
	GLO	R10
	STXD
	
	;checking if DATAPOINTER has a previously stored location
	;if it has a non-zero value, then branch and set R10 value to that location to get next data value
	LDA	R7 ;!!potential problem since not also checking if following low address byte of DATAPOINTER is also zero
	LBNZ L0F43
	
	;if DATAPOINTER was zero, then start searching for DATA statement from beginning of program text
	CALL INITTXT ;sets R10 to PGM_START and inserts $02
L0F14:
	LDN	R10
	XRI	$03 ;checking for end of text
	LBNZ L0F20
	
	;unexpected end of text, set error
	CALL ERRCODE
	BYTE $10
	INC R2 ;increment R2 to reset location for previously saved R10 location
	INC R2
	RETN
	
	;looking for statement begging with "DA" for DATA
L0F20:
	LDA	R10
	SMI	$41
	LBNF L0F20 ;skip if less than 'A'...skipping line numbers, spaces, tabs, etc
	
	DEC	R10
	LDA	R10
	XRI	$44 ;'D'
	LBNZ L0F38
	LDA	R10
	XRI	$41 ;'A'
	LBNZ L0F38
	
	;"DA" was found, skipping over any following alpha chars
L0F30:
	LDA	R10
	SMI	$41 ;alpha char check
	LBDF L0F30
	DEC	R10
	LBR	L0F46
	
	;statement not DATA, so find end of statement (CR or :)
	;and loop back to check next statement
L0F38:
	LDA	R10
	XRI	$0D ;CR
	LBZ L0F14
	XRI	$37 ;':'
	LBZ L0F14
	BR L0F38
	
L0F43:
	PHI	R10
	LDN	R7
	PLO	R10
	
L0F46:
	;getting number or string within DATA statement depending on type of variable
	;R9.1 was set after VARIABLE SETUP. Use to determine variable type
	GHI	R9
	SHL 
	BDF L0F62 ;branch if string var
	
	CALL SKIPPER+1
	LBZ L0F14 ;??find next DATA statement
	
	;getting number from DATA statement and assign to READ variable
	CALL GETNUM
	CALL COPY_ARITHSTACK_TO_VARIABLE

	;save current position in DATA statement to DATAPOINTER bytes
	;via R7.  Then restore pgm text location to R10 from mem stack.
L0F55:
	GLO	R10
	STR	R7
	DEC	R7
	GHI	R10
	STR	R7

;made following changes in 2024...they perform same as old...just using X machine codes
	;INC	R2
	;LDA	R2
	IRX
	LDXA
	PLO	R10
	;LDN	R2
	LDX
	PHI	R10
	LBR	L0EFB
	
	;??getting string from DATA statement
L0F62:
	CALL SKIP2
	LBZ L0F14
	BR L0F55

;-----------------------------------------------------
;L0F6B:
PLOT:
	LDA	R10
	SMI	$30
	LBNF PLOT ;L0F6B
	DEC	R10
	LBR	ARRAY_INDEX ;L09F3

;--------------------------------------------------
;This is a modified holdover from when 1802 micro was connected to TTL video board and CRT monitor
;Modified to use VT100 ESC commands to plot ascii character at specified screen position.
;usage is SET [x int val], [y int val], [ascii char as decimal], ie. SET 10, 15, 65 to plot 'A' at x=10, y=15

;NOTE: Use CLS statement before plotting, and use a waiting loop with INKEY statement after plot generated.

;MONITOR_PLOTXY will use code at L00C2 to output row, col numbers to serial terminal.
;Since L00C2 code fragment uses R8, R9, and R7, this routine uses
;R12 and R11 in SET to pass values to MONITOR_PLOTXY

SET:
	CALL PLOT
	GLO	R9
	PHI R12 ;R12.1 = col position
	SMI	$40
	LBDF L0F8F
	CALL PLOT
	GLO	R9
	PLO	R12 ;R12.0 = row position
	SMI	$10
	LBDF L0F8F
	CALL PLOT
	;R11.0 holds char to plot
	GLO R9
	PLO R11 
	CALL MONITOR_PLOTXY
	DEC	R10
L0F8F:
	LBR REM 

;--------------------------------------------------
;L0F92
FOR:
	CALL LET
	GHI	R2
	PHI	R8
	LDI	$00
	PLO	R8
	LDN	R8
	LBNZ	L0FAE
	LDI	$05
	PLO	R8
	LDA	R8
	PHI	R9
	LDA	R8
	PLO	R9
	LDA	R8
	PHI	R7
	XRI	$14
	BNZ	L0FAF
	CALL ERRCODE
	BYTE $13
L0FAE:
	RETN
L0FAF:
	LDA	R8
	PLO	R7
	DEC	R9
	DEC	R9
	LDA	R9
	STR	R7
	INC	R7
	LDN	R9
	STR	R7
	INC	R7
	LDI	$05
	PLO	R8
	GHI	R7
	STR	R8
	INC	R8
	GLO	R7
	STR	R8
	CALL AOS
	GLO R7
	ADI $06
	PLO	R7
	DEC	R10
	LDA	R10
	XRI	$0D ;CR
	LBZ	L0FF1
	XRI	$37 ;':'
	LBZ	L0FF1
	GHI	R2
	PHI	R8
	LDI	$06
	PLO	R8
	GLO	R7
	STR	R8
	CALL AOS
	GLO R7
	ADI $06
	PLO	R7
L0FE0:
	GHI	R2
	PHI	R8
	LDI	$07
	PLO	R8
	GHI	R10
	STR	R7
	INC	R7
	GLO	R10
	STR	R7
	INC	R7
	GHI	R7
	STR	R8
	INC	R8
	GLO	R7
	STR	R8
	RETN
L0FF1:
	STR	R7
	INC	R7
	LDI	$01
	STR	R7
	INC	R7
	LDI	$10
	STR	R7
	INC	R7
	LDI	$00
	STR	R7
	INC	R7
	STR	R7
	INC	R7
	STR	R7
	INC	R7
	LBR	L0FE0

;--------------------------------------------
;L1006
NEXT:
	CALL INIT_ARITHSTACK_POINTER
	CALL SKIPPER
	CALL VAR1
	GHI	R2
	PHI	R7
	LDI	$00
	PLO	R7
	LDN	R7
	LBR L1071
L1017:
	CALL L0B89 ;SVPNT
	LDI	$08
	PLO	R7
	LDN	R7
	PLO	R12
	SEX	R12
	LDI	HIGH(SCRATCHMEMB) ;$13
	PHI	R12
	;LBR L11EE
	; next continued moved to NEXT
L11EE:
;NEXT_CONT:
	GLO	R12
	SMI	$10
	PLO	R12
	SMI	$60
	LBNF L0B35
	GHI	R9
	XOR
	LBNZ	L11EE
	INC	R12
	GLO	R9
	XOR
	DEC	R12
	LBNZ L11EE
	GLO	R12
	ADI	$10
	STR	R7
	SMI	$08
	PLO	R12
	;LBR	L1026
L1026:
	CALL L0CC5 ;V2ASTCK
	GLO	R12
	SMI	$08
	PLO	R7
	CALL ADD
	CALL COPY_ARITHSTACK_TO_VARIABLE
	GLO	R13
	SMI	$06
	PLO	R13
	LDI	HIGH(SCRATCHMEMB);$13
	PHI	R12
	GLO	R7
	PLO	R12
	CALL L0CC5 ;V2ASTCK
	CALL SUB
	INC R13
	LDA R13
	LBZ	L1054
L1046:
	LDI	HIGH(SCRATCHMEMB);$13 ;??? HIGH ADDRESS LOCATION
	PHI	R7
	GLO	R7
	ADI	$0C
	PLO	R7
	LDA	R7
	PHI	R10
	LDA	R7
	PLO	R10
	LBR	SUBROUT3 ;L07DF
L1054:
	INC	R13
	CALL EQTEST
	LBZ	L1046
	GHI	R2
	PHI	R8
	LDI	$08
	PLO	R8
	GLO	R7
	SMI	$02
	STR	R8
	LBR	REM ;L08E2

;---------------------------------------
;L1066
EQTEST:
	LDI	$04
	PLO	R7
L1069:
	LDA	R13
	LBNZ	L1070
	DEC	R7
	GLO	R7
	LBNZ	L1069
L1070:
	RETN
L1071:
	LBNZ	L1070
	DEC	R12
	DEC	R12
	LBR L1017

;---------------------------------------
;L1077:
IF_THEN:
	LDA	R10
	XRI	$20
	LBZ IF_THEN ;skip space
	;non-space char
	INC	R10
	LDN	R10 ;get 3rd char after non-space 1st
	DEC	R10 ;pointing to 2nd char
	XRI	$24 ;'$'
	LBZ L1086 ;branch if string var exmpl A3$
	LDN	R10 ;get 2nd char
	XRI	$24 ;'$'
L1086:
	DEC	R10 ;pointing to 1st char
	LBZ	L10F8 ;branch if string var exmpl A$
	CALL AOS
	DEC R10
	LDA R10 ;1st char
	STR	R2
	LDN	R10 ;2nd char
	SMI	$3C
	LBNF L109C
	SMI	$03 
	BDF	L109C
	LDA	R10 ;2nd char
	ADD ;add 1st and 2nd ???don't know why I did this
	STR	R2 
L109C:
	DEC	R2
	CALL AOS1
	CALL SUB ;subtract AOS and AOS1 results for comparison
	INC R13
	INC R2
	LDN	R2
	XRI	$3D ;'='
	LBNZ	L10C1
	INC	R13
L10AB:
	INC	R13
	CALL EQTEST
L10AF:
	LBNZ REM ;L08E2
L10B2:
	LDA	R10
	XRI	$20
	LBZ	L10B2
	DEC	R10
	LDN	R10
	SMI	$3A
	LBNF BGOTO ;L0881
	LBR SUBROUT3 ;L07DF
L10C1:
	XRI	$03 ;'>'
	LBNZ	L10D1
	LDA	R13
	LBNZ	L10AF
L10C8:
	INC	R13
	CALL EQTEST
L10CC:
	LBZ	REM ;L08E2
	LBR L10B2
L10D1:
	XRI	$02 ;'<'
	LBNZ	L10DA
	LDA	R13
	LBZ	L10CC
	LBR L10C8
L10DA:
	XRI	$46 ;'<>'
	LBNZ L10E1
	INC	R13
	LBR L10C8
L10E1:
	XRI	$01 ;'>='
	LBNZ L10EA
	LDA	R13
	LBZ L10B2
	LBR L10AB
L10EA:
	XRI	$02 ;'<='
	LBNZ L10F3
	LDA	R13
	LBNZ L10B2
	LBR L10AB
L10F3:
	CALL ERRCODE
	BYTE $07
	RETN
L10F8:
	;handle string var
	CALL VARIABLE_SEARCH
	LBZ	V2TEMP1 ;L0A35
	GHI	R12
	PHI	R11
	GLO	R12
	PLO	R11
L1102:
	LDA	R10
	XRI	$20 ;' '
	LBZ L1102
	XRI	$1D ;'='
	LBNZ L10F3 ;ERROR
L110C:
	LDA	R10
	XRI	$20 ;' '
	LBZ L110C
	XRI	$02 ;'"'
	LBNZ L111B
	GHI	R10
	PHI	R12
	GLO	R10
	PLO	R12
	BR	L1122
L111B:
	DEC	R10
	CALL VARIABLE_SEARCH
	LBZ	V2TEMP1; L0A35
L1122:
	SEX	R12
L1123:
	LDA	R11
	BZ L112D
	XOR
L1127:
	LBNZ REM ;L08E2
L112A:
	INC	R12
	BR L1123
L112D:
	LDN	R12
	LBZ	L1138
	XRI	$22 ;'"'
	BNZ	L1127
	GHI	R12
	PHI	R10
	GLO	R12
	PLO	R10
L1138:
	LDA	R10
	XRI	$54 ;'T'
	LBNZ L1138
L113D:
	LDA	R10
	SMI	$41
	LBDF L113D
	DEC	R10
	LBR	L10B2

;---------------------------------------
;L1146
ABS:
	INC	R13
	LDI	$00
	STR	R13
	DEC	R13
	RETN

;---------------------------------------
;L114C
SGN:
	GLO	R13
	PLO	R7
	INC	R13
	INC	R13
	INC	R13
	CALL EQTEST
	BNZ	L1159
	GLO	R7
	PLO	R13
	RETN
L1159:
	GLO	R7
	ADI	$06
	PLO	R13
	SEX	R13
	LDI	$00
	STXD
	STXD
	STXD
	LDI	$10
	STXD
	LDI	$01
	STXD
	DEC	R13
	RETN

;-------------------------------------------
;L116B
INT:
	GHI	R13
	PHI	R12
	LDI	$00
	PLO	R12
	CALL COPY_ARITHSTACK_TO_NUMA ;L04A8
	CALL UNPACK
	GLO	R8
	SHL
	LBDF ZNUMB ;L05F2
	SMI	$10 ; 08 before SHL
	LBNF	L1184
	GLO	R13
	SMI	$06
	PLO	R13
	RETN
L1184:
	GLO	R8
	PLO	R12
L1186:
	LDI	$00
	STR	R12
	INC	R12
	GLO	R12
	XRI	$08
	LBNZ L1186
	LDI $00	;GHI R4
	PLO	R12
	LBR	L0439

;----------------------------------------
FIX:
	CALL SKIPPER+1
	CALL NUMCON
	DEC R10
	LDI HIGH(FIXNUMBER)
	PHI	R7
	LDI LOW(FIXNUMBER)
	PLO	R7
	GLO	R9
	STR	R7
L11A2:
	LBR	REM

;-------------------------------------------
;this routine allows for input of a byte via 1802 port specified by INPUTPORT
;as binary and converts to decimal values for packed number and saving to variable
IPT:
	CALL INIT_ARITHSTACK_POINTER
	CALL SKIPPER+1
	CALL VARIABLE_SETUP
	GHI	R13
	PHI	R12
	LDI	$0D ;scratch memory pointer
	PLO	R12
	SEX	R12
	INP	INPUTPORT
	STXD
	LDI $00
	STXD
	STXD
	STR	R12
	;[R12 low address:value] = [0A:00] [0B:00] [0C:00] [0D:input byte]
	PHI	R8
	LDI	$0D
	PLO	R12
	;convert 4-byte binary number to 8-digit decimal values in 02:09 address range
	CALL BIN2DEC
	;BIN2DEC creates right justified decimal. NORM works with left justified.
	;correct exponent for NORM by assigning $08 to R8.0
	LDI	$08
	PLO	R8
	;CALL L0433 ;normalize and pack decimal number
	CALL NORM
	CALL PACK
	CALL COPY_TO_ARITHSTACK
	CALL COPY_ARITHSTACK_TO_VARIABLE ;copy packed number to variable
	;LBR L11A2
	LBR REM

;---------------------------------------------
;send byte to 1802 IO port specified by OUTPUTPORT definition
;value to be sent can be 0-255 decimal, hex value (&hh), or variable
;L11D4:
OPT:
	;skip spaces, etc until number or variable
	CALL SKIPPER+1
	;2024 mod - check if dealing with hex value
	LDN R10
	XRI "&"
	BNZ OPT1
	CALL GETHEX
	BR OPT2
OPT1:	
	;using array_index function to convert 0-255 decimal to byte stored in R9.0, or
	;get value of variable and convert to byte
	CALL ARRAY_INDEX
OPT2:
	GLO	R9
	STR	R2
	OUT	OUTPUTPORT
	DEC	R2
	;LBR L11A2
	LBR REM
	
;-----------------------------------------------
KEY:
	;rev 5/19/21 - routine gets a single char stored at KEYVAL
	;for usage in IF_THEN statement. Note that this routine appears
	;to be manipulating the CALL/RETURN mem stack stored values
	;to perform evaluations.
	;adding an INKEY statement which will store its val to KEYVAL
	;for evaluation with KEY.
 	GLO	R2 
 	ADI	$0A
 	PLO	R2
 	LDXA
 	PLO	R6
 	LDX
 	PHI	R6
	;GHI R2 ;$14
	LDI HIGH(LINE_BUFB)
	PHI	R11
	LDI LOW(LINE_BUFB) ;$60
	PLO	R11 ;R11-->$1460 BEGINNING OF INPUT BUFFER
	LDI HIGH(KEYVAL)
	PHI R7
	LDI LOW(KEYVAL) ;$0F
	PLO R7 ;R7-->$140F KEY VALUE
	LDN R7 ;GET KEY VALUE TO D
	STR	R11 ;STORE KEY VALUE @ $1460
	INC	R11
	LDI $00
	STR R11 ;STORE 00 TO $1461
	DEC	R11 ;R11-->$1460
	LBR	L1102

;---------------------------------------
;L12B9
RND:
	;GHI R2
	LDI HIGH(RNDSEEDMEM)
	PHI	R7
	LDI	LOW(RNDSEEDMEM)+3 ;$44
	PLO	R7
	SEX	R13 ;ASTCK POINTER
	LDN	R7
	STXD
	DEC	R7
	LDN	R7
	STXD
	DEC	R7
	LDN	R7
	STXD
	DEC	R7
	LDN	R7
	STXD
	LDI $00 ;GHI R4
	STXD
	STXD
	CALL MULT
	LDI $14
	PLO	R12
	SEX	R12
	LDI	$04
	PLO	R8
RND1:
	LDA	R12
	SHLC
	ADC
	PHI	R8
	SMI	$9A
	LBDF RND2
	GHI	R8
RND2:
	STR	R7
	INC	R7
	DEC	R8
	GLO	R8
	LBNZ RND1
	RETN

;------------------------------------------------------------
	
;---------------------------------------------------
;L12EB:
;DIM_CONT: ;ADD TO DIM
;	STR	R11
;	INC	R11
;	GHI	R9
;	STR	R11
;	LBR	L0A5A

;-----------------------------------------------------
;L1445
;this is BASIC CALL statement, had to label as BCALL1 since CALL label used for D4 routine
;Usage: 'CALL (decimal high address),(decimal low address)'
BCALL1:
	CALL SKIPPER
	CALL NUMCON
	GLO R9
	PHI	R7
	CALL NUMCON
	DEC R10
	GHI	R7
	PHI	R9
	CALL PERF
	LBR	REM ;L08E2

;-----------------------------------------
;L1550: - not using this for now 2024
;INTERRUPT_ROUTINE:
;	RET
;	DEC	R2
;	SAV
;	DEC	R2
;	STR	R2
;	LDI	$27
;	PHI	R0
;	LDI	$00
;	PLO	R0
;	LDA	R2
;	LBR INTERRUPT_ROUTINE ;L1550

;------------------------------------------------
;L1560: ;not using this for now 2024
;	LDI	$15 ;??? R1 is pointing to interrupt routine +1
;	PHI	R1
;	LDI	$51
;	PLO	R1
;	LDI	$00
;	LBR	L0003 ;THIS BRANCHS TO BEGINNING?

;------------------------------------------------
;L1226
COPY_ASTCK_DOWN_INIT:
	GHI R13
	PHI R12
	GLO R13
	ADI $06
	PLO R12
COPY_ASTCK_DOWN:
	;copy last value in astack down stack to next slot
	;facilitates setting up arith stack argument for iterative processes
	;enter with R13 pointing to LSB of available astack slot
	;enter with R12 pointing to LSB of value to copy down
	;R7.0 is loop counter
	LDI $06
	PLO R7
CAD1:
	LDN R12
	STR R13
	DEC R12
	DEC R13
	DEC R7
	GLO R7
	LBNZ CAD1
	;returns with R13 pointing to next available astck slot, R12 pointing to LSB of copy value
	RETN

;----------------------------------------------
	; Calculate SQRT of floating point number
	; believe I got this algorithm from some published routine around early 1980s
	; iterative procedure (5 times) is used to estimate square root

	;enter with argument in arithmetic stack (R13 pointing to next available arith stack slot)
SQR:
	GHI	R13
	PHI	R12
	GLO	R13
	ADI	$03
	PLO	R12 ;R12 -> msb of 4 byte packed number
	
	LDI	$04 ;iteration counter used by zero_check1
	PLO	R7
	CALL ZERO_CHECK1 ;check if argument is zero using R12
	LBZ SQR_RETN ;return if number is zero as indicated by D=0 returned by zero check
	
	;check for neg argument, return error if neg 
	INC	R13
	LDA	R13 ;R13 pointing to exponent
	LBZ	SQR2
	CALL ERRCODE
	BYTE $15
	
SQR_RETN:
	RETN

SQR2:
	;setting up argument in arith stack to be copied down stack so as
	;iterative processes in the algorithm can take place.
	LDN	R13
	PHI	R7 ;copy argument exponent to R7.1
	INC	R7 ;??iteration count R7.0 = 5, need to review how this value assigned??
	SEX	R13
	LDI $00
	STXD ;store $00 to argument exponent
	STXD ;store $00 to argument sign (should have been 00 anyhow)

;	CALL COPY_PACKED_NUM ;copied stack num to next slot in stack
;	CALL COPY_PACKED_NUM ;copied that num to next slot in stack
	CALL COPY_ASTCK_DOWN_INIT
	CALL COPY_ASTCK_DOWN

	;now have 3 copies of num in stack - slot1=original, slot2=copy, slot3=copy
SQR3:
	;divide last two numbers in arith stack (slot2/slot3) with result in slot2
	CALL DIV
	;add slot3 to slot2, result to slot2
	;move arith stack pointer R13 to setup for addition
	GLO	R13
	SMI	$06
	PLO	R13
	CALL ADD
	;addition result now in slot2

	;assigning 0.5 to slot3 for multiplication with slot2
	SEX	R13
	LDI $00 
	STXD
	STXD
	STXD
	LDI	$50
	STXD
	LDI	$00
	STXD
	STXD
	CALL MULT ;result to slot2

	;check if algorithm iteration counter (R7.0) = 0
	DEC R7
	GLO	R7
	LBZ SQR4
	
;	CALL COPY_PACKED_NUM; copy slot2 to slot3
	CALL COPY_ASTCK_DOWN_INIT

	GLO	R13
	ADI	$0C
	PLO	R13
	LBR SQR9
	
SQR4:
	INC	R13
	INC	R13
	GHI	R7
	SHL
	LBDF SQR8 ;negative exponent
	GHI	R7
	SHR ;positive exponent
	LBDF SQR6 ;odd positive
SQR5:
	STR	R13
	;even positive
	DEC	R13
	GLO	R13
	ADI	$06
	PLO	R12
	LBR	L0D5F ;this is in COPY_ARITHSTACK_TO_VARIABLE
SQR6:
	CALL SQR_CORRECTION
	GHI	R7
	ADI	$01
SQR7:
	SHRC
	LBR SQR5
SQR8:
	GHI	R7
	SHRC
	LBNF SQR5 ;even negative

;	CALL COPY_PACKED_NUM ;original code was labeled CORR (now called SQR_CORRECTION), but call address was $1226 which was MOVE (now called COPY_PACKED_NUM)
	CALL COPY_ASTCK_DOWN_INIT

	GHI	R7
	SMI	$01
	LBR SQR7

SQR9:
	;CALL COPY_PACKED_NUM ;copy slot1 to slot2
	CALL COPY_ASTCK_DOWN_INIT

	GLO	R13
	SMI	$06
	PLO	R13
	LBR	SQR3 ;iterate algorithm again

;-----------------------------------------------
; multiply arith stack argument with .31622779
;L122C
SQR_CORRECTION:
	DEC	R13
	DEC	R13
	SEX	R13
	LDI	$79
	STXD
	LDI	$27
	STXD
	LDI	$62
	STXD
	LDI	$31 
	STXD
	LDI $00
	STXD
	STXD
	CALL MULT
	INC	R13
	INC	R13
	RETN

;------------------------------
;2024 additions to facilitate taylor series expansions for cos and sin calcs

COPY_ASTCK_NUM_TO_TEMPSTORE:
	;temporary storage of intermittent arithstack result to mem storage
	;after a call to mult function, R13 astack pointer is at next open slot.
	;so need to increment R13 by $06 to point to LSB of result.
	;R2 is X and points to basic temp mem storage
	;R7.0 is loop counter for six bytes to be moved
	GLO R13
	ADI $06
	PLO R13
	LDI $06
	PLO R7
CANTM1:
	LDN R13
	STXD
	DEC R13
	DEC R7
	GLO R7
	LBNZ CANTM1
	;returns with R2 pointing to next available slot, and R13 pointing to next slot in arith stack
	RETN

RETRIEVE_TEMP_TO_ASTCK:
	;retrieve intermittent result from basic temporary storage to arith stack slot
	;R2 is X and points to basic open temp storage
	;increment R2 to point to sign byte of temp stored result.
	;decrement R13 by $05 to point to corresponding byte
	;R7.0 is loop counter for six bytes to be moved
	IRX
	GLO R13
	SMI $05
	PLO R13
	LDI $06
	PLO R7
RTMTA1:
	LDXA
	STR R13
	INC R13
	DEC R7
	GLO R7
	LBNZ RTMTA1
	DEC R2
	GLO R13
	SMI $07
	PLO R13
	;returns with R2 pointing to next available slot, and R13 pointing to next slot in arith stack
	RETN
	
SIN:

	CALL RADIAN_RANGE_REDUCTION
	;WIP - taylor series to calc sine of radians value.
	;SIN(x) = x - (x^3/3!) + (x^5/5!) - (x^7/7!) + (x^9/9!); - (x^11/11!)
	;where x = radians angle in arith stack
	
	;consider a pre-check for zero argument and return fixed value
	
	;step 1 - calc x^3, x^5, x^7, x^9 and save values to mem storage

	;copy down radian value 9 times in arith stack using R13 and R12
	CALL COPY_ASTCK_DOWN_INIT
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	
	;multiply bottom 2 astack values to get x^2
	CALL MULT
	;multiply bottom 2 astack values to get x^3
	CALL MULT
	;save that result to memory stack via R2
	CALL COPY_ASTCK_NUM_TO_TEMPSTORE
	;multipy bottom 2  astack values to get x^4
	CALL MULT
	;multipy bottom 2  astack values to get x^5
	CALL MULT
	;save that result to memory stack via R2
	CALL COPY_ASTCK_NUM_TO_TEMPSTORE
	;multipy bottom 2  astack values to get x^6
	CALL MULT
	;multipy bottom 2  astack values to get x^7
	CALL MULT
	;save that result to memory stack via R2
	CALL COPY_ASTCK_NUM_TO_TEMPSTORE
	;multipy bottom 2  astack values to get x^8
	CALL MULT
	;multipy bottom 2  astack values to get x^9
	CALL MULT
	
	;with exponential numerator vals calculated, now begin factorial division to arrive at series terms
	
	;calculate x^9/9! term
	;x^9 is last value in arith stack
	;load 9!=362880 [00 06 36 28 80 00] to next arith stack slot which R13 is pointing to
	SEX R13
	LDI $00
	STXD
	LDI $80
	STXD
	LDI $28
	STXD
	LDI $36
	STXD
	LDI $06
	STXD
	LDI $00
	STXD
	SEX R2
	CALL DIV
	
	;add term to x value
	CALL ADD
	
	;calculate x^7/7! term
	;retrieve x^7 from temp mem storage to arith stack
	CALL RETRIEVE_TEMP_TO_ASTCK
	;load 7!=5040 [00 04 50 40 00 00] to next arith stack
	SEX R13
	LDI $00
	STXD
	STXD
	LDI $40
	STXD
	LDI $50
	STXD
	LDI $04
	STXD
	LDI $00
	STXD
	SEX R2
	CALL DIV
	
	;subtract term from composite result
	CALL SUB
	
	;compute x^5/5! term
	;retrieve x^5 from temp mem storage to arith stack
	CALL RETRIEVE_TEMP_TO_ASTCK
	;load 5!=120 [00 03 12 00 00 00] to arith stack
	SEX R13
	LDI $00
	STXD
	STXD
	STXD
	LDI $12
	STXD
	LDI $03
	STXD
	LDI $00
	STXD
	SEX R2
	CALL DIV
	
	;add term to composite result
	CALL ADD
	
	;compute x^3/3! term
	;retrieve x^3 from temp mem storage to arith stack
	CALL RETRIEVE_TEMP_TO_ASTCK
	;load 3!=6 [00 01 60 00 00 00] to arith stack
	SEX R13
	LDI $00
	STXD
	STXD
	STXD
	LDI $60
	STXD
	LDI $01
	STXD
	LDI $00
	STXD
	SEX R2
	CALL DIV
	
	;subtract term from composite result
	CALL SUB

	;arith stack result is now final series expansion computation
	;need to negate result if R14.0 is non-zero (R14.0 was set in range reduction code segment)
	GLO R14
	LBZ SIN1

	INC R13
	;pointing to sign byte of result. set bit 7 = 1 for negative value
	LDN R13
	ORI $80
	STR R13
	DEC R13
	
SIN1:
	RETN
	
	
;================================================================================	
	;calc cosine of radian using taylor series
	;cosine(x) = 1-(x^2/2!)+(x^4/4!)-(x^6/6!)+(x^8/8!)
	;for sequencing in arith stack, rearranging to:
	;cos(x)=(x^8/8!)-(x^6/6!)+(x^4/4!)-(x^2/2!)+1
COS:
	;shift cos angle to match sin angle for radian reduction routine, then return back
	;add PIDIV2 to angle to match sin
	CALL PIDIV2
	CALL ADD
	CALL RADIAN_RANGE_REDUCTION
	CALL PIDIV2
	CALL SUB
	
	;calc x^2, x^4, x^6, x^8 and save values to mem storage
	;copy down arith stack values using R13 and R12
;	GHI R13
;	PHI R12
;	GLO R13
;	ADI $06
;	PLO R12
;	;R12 now points to LSB of argument and R13 points to next arith stack slot

	;revised sequence using x^2 as copy-down to reduce number of multiplies
	CALL COPY_ASTCK_DOWN_INIT
	CALL COPY_ASTCK_DOWN
	;multiply and save as x^2
	CALL MULT
	CALL COPY_ASTCK_NUM_TO_TEMPSTORE
	;copy down x^2 three times to get X^8
	;[reset R12 pointer for correct copy down due to MULT]
;	GLO R13
;	ADI $06
;	PLO R12
	CALL COPY_ASTCK_DOWN_INIT
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	;last two arith stack values are x^2, multiply to get x^4 and save
	CALL MULT
	CALL COPY_ASTCK_NUM_TO_TEMPSTORE
	;mult x^4 with x^2 to get x^6 and save
	CALL MULT
	CALL COPY_ASTCK_NUM_TO_TEMPSTORE
	;mult x^6 with x^2 to get x^8
	CALL MULT
	
	;calculate x^8/8! term
	;x^8 is last arith stack value
	;load 8!=40320 [00 05 40 32 00 00] to next arith stack slot which R13 is pointing to
	SEX R13
	LDI $00
	STXD
	STXD
	LDI $32
	STXD
	LDI $40
	STXD
	LDI $05
	STXD
	LDI $00
	STXD
	SEX R2
	CALL DIV
	;div result is start of composite value
	
	;calculate x^6/6! term
	;retrieve x^6 from temp mem storage to arith stack
	CALL RETRIEVE_TEMP_TO_ASTCK
	;load 6!=720 [00 03 72 00 00 00] to next arith stack
	SEX R13
	LDI $00
	STXD
	STXD
	STXD
	LDI $72
	STXD
	LDI $03
	STXD
	LDI $00
	STXD
	SEX R2
	CALL DIV
	;subtract term from composite result
	CALL SUB
	
	;compute x^4/4! term
	;retrieve x^4 from temp mem storage to arith stack
	CALL RETRIEVE_TEMP_TO_ASTCK
	;load 4!=24 [00 02 24 00 00 00] to arith stack
	SEX R13
	LDI $00
	STXD
	STXD
	STXD
	LDI $24
	STXD
	LDI $02
	STXD
	LDI $00
	STXD
	SEX R2
	CALL DIV
	;add term to composite result
	CALL ADD
	
	;compute x^2/2! term
	;retrieve x^2 from temp mem storage to arith stack
	CALL RETRIEVE_TEMP_TO_ASTCK
	;load 2!=2 [00 01 20 00 00 00] to arith stack
	SEX R13
	LDI $00
	STXD
	STXD
	STXD
	LDI $20
	STXD
	LDI $01
	STXD
	LDI $00
	STXD
	SEX R2
	CALL DIV
	;subtract term from composite result
	CALL SUB
	
	;now add result to 1 
	SEX R13
	LDI $00
	STXD
	STXD
	STXD
	LDI $10
	STXD
	LDI $01
	STXD
	LDI $00
	STXD
	SEX R2
	CALL ADD

	;arith stack result is now final series expansion computation
	;need to negate result if R14.0 is non-zero (R14.0 was set in range reduction code segment)
	GLO R14
	LBZ COS1

	INC R13
	;pointing to sign byte of result. set bit 7 = 1 for negative value
	LDN R13
	ORI $80
	STR R13
	DEC R13
	
COS1:
	RETN
	
PI:
	;lood PI value to next arith stack slot
	;R13 pointing to next available slot
	SEX R13
	LDI $27
	STXD
	LDI $59
	STXD
	LDI $41
	STXD
	LDI $31
	STXD
	LDI $01
	STXD
	LDI $00
	STXD
	SEX R2
	RETN
	
;PI2:
;	;lood 2*PI=6.2831853 to next arith stack slot
;	;R13 pointing to next available slot
;	SEX R13
;	LDI $53
;	STXD
;	LDI $18
;	STXD
;	LDI $83
;	STXD
;	LDI $62
;	STXD
;	LDI $01
;	STXD
;	LDI $00
;	STXD
;	SEX R2
;	RETN

PIDIV2:
	;lood PI/2=1.5707963 to next arith stack slot
	;R13 pointing to next available slot
	SEX R13
	LDI $63
	STXD
	LDI $79
	STXD
	LDI $70
	STXD
	LDI $15
	STXD
	LDI $01
	STXD
	LDI $00
	STXD
	SEX R2
	RETN

RADIAN_RANGE_REDUCTION:
	;6/7/24 tests of taylor series expansion suggested that need to reduce argument
	;to be between 0 and PI for good approximation.

	;reduce radian argument to be between 0 and 2PI by
	;dividing argument by 2PI, truncate result to remainder
	;and multiply remainder times the divide result
	
;	;load 2*PI to arith stack
;	;CALL PI2
	CALL PI
	;divide argument by PI ;2*PI
	CALL DIV
	;need to determine if in 2nd half of sine wave which indicates result will be negative
	;if exponent of result is zero, then argument is L.E. to PI, result is positive
	;if exponent is greater than zero, determine if it is odd or even.
	;check most significant digit if bit one = 1. if so, then it is odd and result is negative
	;using R14.0 to set low bit to 1 for flag to negate series approx result, else 0 to leave positive
	LDI $00
	PLO R14
	
	;now point R12 to exponent of division result
	GHI R13
	PHI R12
	GLO R13
	PLO R12
	INC R12
	INC R12
	
	;get exponent
	LDN R12
	;if zero, continue to series expansion since div result is PI or less. Also sine will be positive so R9.0 left at zero
	LBZ RRR1
	;exponent greater than zero, check for odd/even most significant digit of division result
	;[TODO: if value = 1.0000000, should leave positive expansion result]
	INC R12
	LDN R12
	;dealing with packed number, so left 4 bits are value of first digit.
	;test if bit 4 of byte is = 1. if bit = 1, then packed digit is odd 
	ANI $10
	LBZ RRR1
	;digit is odd so set R14.0 to non-zero to signify to set series expansion result to negative after it is computed
	INC R14
	
RRR1:	
	;obtain remainder of division result by
	;copy down result and convert to integer
	;then subtract integer to get just remainder
	;GHI R13
	;PHI R12
	GLO R13
	ADI $06
	PLO R12
	CALL COPY_ASTCK_DOWN
	CALL INT
	CALL SUB
	;remainder is now in arith stack...multiply with PI2
	CALL PI ;PI2
	CALL MULT
	;range reduced radian value now in arith stack (0-2*PI)
	RETN

;-------------------------------------------

TAN:
	;calc tangent of argument x using Taylor Series expansion where |x|<PI/2
	;tan x = x + ((1/3)*(x^3)) + ((2/15)*(x^5)) + ((17/315)*(x^7)) + ((62/2835)*(x^9))
	
	;calc powers of arith stack argument x: x^3, x^5, x^7, x^9 and save values to R2 mem storage
	
	;copy down radian value 9 times in arith stack using R13 and R12
	CALL COPY_ASTCK_DOWN_INIT
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	CALL COPY_ASTCK_DOWN
	
	;multiply bottom 2 astack values to get x^2
	CALL MULT
	;multiply bottom 2 astack values to get x^3
	CALL MULT
	;save that result to memory stack via R2
	CALL COPY_ASTCK_NUM_TO_TEMPSTORE
	;multipy bottom 2  astack values to get x^4
	CALL MULT
	;multipy bottom 2  astack values to get x^5
	CALL MULT
	;save that result to memory stack via R2
	CALL COPY_ASTCK_NUM_TO_TEMPSTORE
	;multipy bottom 2  astack values to get x^6
	CALL MULT
	;multipy bottom 2  astack values to get x^7
	CALL MULT
	;save that result to memory stack via R2
	CALL COPY_ASTCK_NUM_TO_TEMPSTORE
	;multipy bottom 2  astack values to get x^8
	CALL MULT
	;multipy bottom 2  astack values to get x^9
	CALL MULT
	
	;calculate ((62/2835)*(x^9)) term
	;x^9 is last value in arith stack
	;load 62/2835=.21869489E-1 [00 FF 21 86 94 89] to next arith stack slot which R13 is pointing to
	SEX R13
	LDI $89
	STXD
	LDI $94
	STXD
	LDI $86
	STXD
	LDI $21
	STXD
	LDI $FF
	STXD
	LDI $00
	STXD
	SEX R2
	CALL MULT

	;add term to x argument
	CALL ADD
	
	;retrieve x^7 from temp mem storage to arith stack
	CALL RETRIEVE_TEMP_TO_ASTCK

	;calculate ((17/315)*(x^7)) term
	;load 17/315=.53968254E-1 [00 FF 53 96 82 54] to next arith stack slot which R13 is pointing to
	SEX R13
	LDI $54
	STXD
	LDI $82
	STXD
	LDI $96
	STXD
	LDI $53
	STXD
	LDI $FF
	STXD
	LDI $00
	STXD
	SEX R2
	CALL MULT

	;add term to composite result
	CALL ADD
	
	;retrieve x^5 from temp mem storage to arith stack
	CALL RETRIEVE_TEMP_TO_ASTCK

	;calculate ((2/15)*(x^5)) term
	;load 2/15=.13333333 [00 00 13 33 33 33] to next arith stack slot which R13 is pointing to
	SEX R13
	LDI $33
	STXD
	STXD
	STXD
	LDI $13
	STXD
	LDI $00
	STXD
	STXD
	SEX R2
	CALL MULT

	;add term to composite result
	CALL ADD
	
	;retrieve x^3 from temp mem storage to arith stack
	CALL RETRIEVE_TEMP_TO_ASTCK

	;calculate ((1/3)*(x^3)) term
	;load 1/3=.33333333 [00 00 33 33 33 33] to next arith stack slot which R13 is pointing to
	SEX R13
	LDI $33
	STXD
	STXD
	STXD
	STXD
	LDI $00
	STXD
	STXD
	SEX R2
	CALL MULT

	;add term to composite result
	CALL ADD

	RETN

;------------------------------

UNFINISHEDCODE1 EQU 0

	IF UNFINISHEDCODE1
	
;2024 - I think this unfinished code from early '80s may have been an attempt to handle
;modifying string variables.

;-------------------------------------------
;L1544
;???don't know purpose of this
FIND: ;this is never called
	CALL MSTRING
	LBNZ L15A5
L1549:
	LDI	$01
	PLO	R9
	LBR	L02C1

;L157B:
;match string??? only called from EXCHANGE or FIND
MSTRING:
	GHI	R10 ;RC = initial line pointer
	PHI	R12
	GLO	R10
	PLO	R12
L157F:
	LDA	R11 ;what is R11 pointing to and where initialized?  PILOT line pointer?
	XRI	$1C ;?? orig notes indicated check for '\' which should be $5C...old keyboard produced $1C for odd key
	LBNZ L157F
	SEX	R11
	GLO	R11
	STR	R2
L1587:
	LDN	R2
	PLO	R11
	GHI	R10 ;R8 = beginning of string
	PHI	R8
	GLO	R10
	PLO	R8
L158D:
	LDN	R10
	XRI	$04 ;$03
	LBNZ L159B
	GHI	R12
	PHI	R10
	GLO	R12
	PLO	R10
	CALL ERRCODE
	BYTE $11 ;string not found
	RETN
L159B:
	LDA	R10  ;RA = 1 char past end of match string
	XOR
	LBNZ	L1587
	INC	R11
	LDN	R11
	XRI	$1C ;'\'   RB = 1 short of exchange string
	LBNZ	L158D
L15A5:
	RETN

;---------------------------------------------
;L15A6:
;??? purpose ... not called from anywhere...was this code not finished or used in my original work?
EXCHANGE:
	CALL MSTRING
	BNZ	L15A5
	INC	R11
	GHI	R8
	PHI	R7
	GLO	R8
	PLO	R7
	CALL L0350
	GHI	R8
	PHI	R10
	GLO	R8
	PLO	R10
	GHI	R11
	PHI	R7
	GLO	R11
	PLO	R7
L15BB:
	LDA	R7
	XRI $1C
	LBNZ L15BB
	DEC	R7
	LDI	$0D
	STR	R7
	CALL I_CMD ;L0238 INSERT COMMAND
	GHI	R10
	PHI	R7
	GLO	R10
	PLO	R7
	DEC	R7
L15CC:
	LDA	R10
	STR	R7
	INC	R7
	XRI	$03
	BNZ	L15CC
	GHI	R8
	PHI	R10
	GLO	R8
	PLO	R10
	LBR	L1549
	
	ENDI
	
;--------------------------------------------
;L15D9:
ON_GOTO:
	CALL PLOT
	GLO	R9
	LBZ	REM ;L08E2
L15E0:
	LDA	R10
	SMI	$30
	LBNF	L15E0
	SMI	$0A
	LBDF	L15E0
	DEC	R10
L15EA:
	DEC	R9
	GLO	R9
	LBZ	LINE_SEARCH ;L0881
L15EF:
	LDA	R10
	XRI	$0D ;CR
L15F2:
	LBZ SUBROUT3 ;L07DF
	XRI	$37 ;':'
	LBZ	L15F2
	XRI	$16 ;','
	LBNZ	L15EF
	LBR L15EA

;-----------------------------------------------------------
	IDL
	
;-----------------------------------------------------------
;currently developing and testing peek and poke code
PEEKPOKECODE EQU 1

	IF PEEKPOKECODE

	;BIN2DEC: binary to decimal conversion performed in scratch mem area
	;enter with R12 pointing to B4 of 4-byte binary 
	;returns with R12 pointing to D1
	
	;scratch mem assignments: 8-digit decimal from 02 to 09, binary in 0A to 0D
	; 00 01 02 03 04 05 06 07 08 09   0A 0B 0C 0D
	;       D1 D2 D3 D4 D5 D6 D7 D8   B1 B2 B3 B4
	;
	; R8.1 = sign, R8.0 = exponent
BIN2DEC:
	;zero out decimal vals in scratch
	LDI $09
	PLO R12
	SEX	R12
	LDI	$08
	PLO	R7
BIN2DEC1:
	LDI	$00
	STXD
	DEC R7
	GLO	R7
	LBNZ BIN2DEC1

	SEX	R2
	;bit shift iteration counter = 32 
	LDI	$20
	PHI	R7

BIN2DEC2:
	LDI $0D
	PLO R12
	;binary byte shift left with carry counter = 4
	LDI	$04
	PLO	R7
	SHL ;zero DF
	;shift 4-byte binary left one bit with carry
BIN2DEC3:
	LDN	R12
	SHLC
	STR	R12
	DEC	R12
	DEC	R7
	GLO	R7
	LBNZ BIN2DEC3
	
	;set decimal vals iteration counter 
	LDI	$08
	PLO	R7
BIN2DEC4:
	LDN	R12
	SHLC
	STR	R12
	SMI	$0A
	LBNF BIN2DEC5
	STR	R12
BIN2DEC5:
	DEC	R12
	DEC	R7
	GLO	R7
	LBNZ BIN2DEC4
	
	;check total SHLC bit counter, doing 32 times
	GHI	R7
	SMI	$01
	PHI	R7
	LBZ	BIN2DEC6
	LBR	BIN2DEC2
	
BIN2DEC6:
	INC	R12
	RETN

;-----------------------------------------------------------------------------------------------
	;DEC2BIN is 8-digit decimal to 4-byte binary conversion routine
	;Used for POKE and PEEK routines.  Even though 1802 addresses use only 2-byte binaries,
	;this function left at capability for 4-byte binaries for potential future use.
	
	;Enter with X=R2, and R12 pointing to D1 of 8-digit number.
	;Returns with R12 pointing to B4 of 4-byte binary number.

	;if GETNUM1 precedes call to DEC2BIN:
	;scratch mem assignments - 8-digit decimal from 02 to 09, binary in 0A to 0D:
	;================================================
	;	 00 01 02 03 04 05 06 07 08 09   0A 0B 0C 0D
	;	       D1 D2 D3 D4 D5 D6 D7 D8   B1 B2 B3 B4
	;================================================
	; R8.1 = sign, R8.0 = exponent

	;GETNUM1 transfers decimal number from program text left justified in D1-D9
	;returns with RC pointing to D1
	
DEC2BIN:
	GHI	R12
	PHI R11
	;after GETNUM1, right justify decimal number in D1-D9 range based on exponent in R8.0
	GLO R8
	STR R2
	GLO R12
	ADD
	PLO R12
	DEC R12 ;R12 -> end of left justified number from GETNUM1
	GLO R8
	PLO R7 ;loop counter
	LDI $09
	PLO R11 ;R11 -> D8
	
	;using exponent as loop counter, copy M(R12) to M(R11) 
D2B_LOOP1:	
	LDN R12
	STR R11
	LDI $00
	STR R12 ;zero out moved digit
	DEC R12
	DEC R11
	DEC R7
	GLO R7
	LBNZ D2B_LOOP1

	;init iteration counter for number of bit shifts for 4 binary bytes (32 bits)
	LDI	$20
	PHI	R7

DEC2BIN1:
	;reset R12 to D1
	LDI $02
	PLO R12

	;init loop counter for number of decimal values to bit shift
	LDI	$08
	PLO	R7

	;set DF=0
	SHL

DEC2BIN2:
	LDN	R12
	;if DF=1 from previous SHR, then add 10 to current byte to rotate in DF (only right 4 bits of decimal being used)
	LSNF
	ADI $0A
	SHR
	STR	R12
	INC	R12
	DEC	R7
	GLO	R7
	LBNZ DEC2BIN2

	;after bit-shift-right decimals, right rotate DF thru 4 binary bytes
	LDN	R12
	SHRC
	STR	R12
	INC	R12
	LDN	R12
	SHRC
	STR	R12
	INC	R12
	LDN	R12
	SHRC
	STR	R12
	INC	R12
	LDN	R12
	SHRC
	STR	R12

	;iterate again for 32 times
	GHI	R7
	SMI	$01
	PHI	R7
	LBNZ DEC2BIN1
	RETN
	
;-----------------------------------------------
;Sept 2024 - at this point ,unable to figure out way to incorporate AOS into the 
;POKE command so it can be used similar to PEEK.
; As such, cannot use variables or arith expressions as POKE address and values.
;POKE address and values can only be numeric values.
	;exmpls:
	;POKE 1500, &F8, &10, 255:
	;POKE &FA00, 255, 01, &FF CR
	;enter with R10 pointing to char after POKE statement command
POKE:
	;use R1.0 as flag for whether getting poke address or getting poke values
    ;R1.0=00 when getting poke address first; =01 when getting poke value to insert at address
	LDI $00
	PLO R1
POKE1:
	;skip spaces and commas prior to decimal or hex value in statement
	LDA R10
	XRI $20 ; ' ' space
	BZ POKE1
	XRI $0C ; ',' $2C
	BZ POKE1
	;exit POKE routine if CR or colon encountered
	XRI $21 ; CR $0D
	LBZ POKE_END
	XRI $37 ; colon $3A
	LBZ POKE_END
	;check for '&' which signifies start of hex value
	XRI $1C ; &=$26
	BZ POKE_HEXVAL

	;check if statement char is non-number	
	;need error statement here
	DEC R10
	LDN	R10
	SMI	$30
	LBNF POKE_ERROR;branch if non-number less than ascii 0
	SMI	$0A
	LBDF POKE_ERROR ;branch if non-number greater than ascii 9

	;char is beginning of decimal number
	GLO R1
	BNZ POKE_DECVAL ;get decimal poke value if poke address set

	;R9.0 = 00, get poke decimal address [note: no check if val greater than 65K]
	CALL GETNUM1
	CALL DEC2BIN
	;load binary lSBs to R8 as poke address (since 65k is max 1802 address, just use lower 2 bytes of binary)
	LDN R12
	PLO R8
	DEC R12
	LDN R12
	PHI R8
POKE2:
	;done with getting poke address, set R1.0>0 as indicator, get poke values
	INC R1
	LBR POKE1

POKE_DECVAL:
	;get decimal poke value and store to poke address R8
	CALL NUMCON ;returns 0-255 in R9.0 [routine uses R10, R11, R7, R9]
POKE3:
	GLO R9
	STR R8
	INC R8
POKE4:
	DEC R10 ;need to point R10 back to char that terminated NUMCON decimal value
	LBR POKE1
	
POKE_HEXVAL: 
	;handle hex number which can be 1 to 4 chars [0-FFFF]
	;valid ascii hex chars are: 30-39, 41-46
	;use R9 to form binary value. transfer to R8 if result is POKE address, leave in R9 if POKE val
	LDI $00
	PHI R9
	PLO R9
	
POKE_HEXVAL5:	
	LDA R10
	PHI R1 ;temp store hex char
	CALL CHKHEXCHAR ;char passed to call via D
	;if call returns D=0, treat as end of hex value
	LBNZ POKE_HEXVAL6
	
	;chk if hex poke address or poke value
	GLO R1
	LBNZ POKE3 ;R1.0 ne. 0, so hex number is a POKE value
	
	;R1.0 = 0, so hex number is POKE address
	GHI R9
	PHI R8
	GLO R9
	PLO R8
	INC R1 ;set flag so that next hex val is treated as POKE value
	LBR POKE4
	
POKE_HEXVAL6:
	GHI R1 ;retrieve hex char to D
	CALL HEXCHAR2BIN
	;save D to temp mem
	STR R2
	;rotate R8 left 4 bits for D insertion using R7 counter
	LDI $04
	PLO R7
POKE_HEXVAL1:	
	GLO R9
	SHL
	PLO R9
	GHI R9
	SHLC
	PHI R9
	DEC R7
	GLO R7
	LBNZ POKE_HEXVAL1
	;insert saved 4 bit val to R8.0
	GLO R9
	ADD
	PLO R9
	;check for potential next hex char
	LBR POKE_HEXVAL5
	
POKE_ERROR:
	CALL ERRCODE
	BYTE $14
POKE_END:
	DEC R10
	RETN
	
	
;-----------------------------------------------
CHKHEXCHAR:
	;pass in ascii char via D (and RF.1) to check if valid hex char $30-$39 or $41-$46
	;returns D=0 if not valid, D=1 if valid
	SDI $2F
	LBDF CHKHEXCHAR_NOTVALID ;char not valid, less than '0'
	GHI R15
	SDI $46
	LBNF CHKHEXCHAR_NOTVALID ;char not valid, greater than 'F'
	GHI R15
	SDI $3A
	LBNF CHKHEXCHAR_VALID ;valid hex char, between '0' and '9'
	GHI R15
	SDI $40
	LBNF CHKHEXCHAR_NOTVALID ;char in range $3A-$40
CHKHEXCHAR_VALID:
	LDI $01
	LSKP
CHKHEXCHAR_NOTVALID:
	LDI $00
	RETN

;-----------------------------------------------
HEXCHAR2BIN:
	;Pass in hex char via D and return bin val in left 4 bits of D
	;note: no check for whether or not valid hex chars
	;LDA R10
	SHL
	SHL
	LSNF ;DF=0 if char is 0-9, =1 if A-F
	;LBNF POKE_HEXVAL_09 ;DF=0 if char is 0-9, =1 if A-F
	ADI $24 ;add $24 if A-F
	SHR		;$35 shl-shl = $D4, then shr-shr = $35
	SHR		;$41 shl-shl = $04 + $24 = $28 shr-shr = $0A
	ANI $0F
	;right 4 bits of D contains bin for A-F
	;save D to temp mem
	;STR R2
	RETN
	
;-----------------------------------------------
PEEK:
	;PEEK is handled as an arithmetic function
	;example statement: A = PEEK(address) ; where address is value from 0-65K
	;retrieved byte ranges from 0-255
	
	GHI	R13
	PHI	R12
	LDI	$02 ;$00
	PLO	R12
	CALL COPY_ARITHSTACK_TO_NUMA
	CALL UNPACK
	;decimal number starts at scratch $02 with R8 holding sign and exponent
	CALL DEC2BIN
	;load binary LSBs to R8 as PEEK address (since 65k is max 1802 address, just use lower 2 bytes of binary)
	LDN R12
	PLO R8
	DEC R12
	LDN R12
	PHI R8
	;convert M(R8) to decimal by placing byte in right most binary byte of BIN2DEC routine
	;since M(R8) is one byte, zero out preceding 3 bytes
	DEC R12
	DEC R12
	LDI $00
	STR R12
	INC R12
	STR R12
	INC R12
	STR R12
	INC R12
	LDN R8
	STR R12
	;note: BIN2DEC is a 4-byte conversion to decimal. Only need a 1-byte conversion.  Maybe develop another smaller routine
	CALL BIN2DEC
	; normalize decimal
	;NORM assumes decimal is left justified in scratch mem and calculates
	;an exponent stored in R8.0 based on being left justified. However,
	;the BIN2DEC routine creates a right justified number, which is always
	;a positive integer.  To correct the exponent created by NORM to reflect
	;a right justified number, R8.0 is initialized to $08, and sign
	;byte in R8.1 is set to $00
	LDI $08
	PLO R8
	LDI $00
	PHI R8
	CALL NORM
	CALL PACK
	LBR	COPY_TO_ARITHSTACK
	
	
	ENDI
;-----------------------------------------------
INKEY:
	;added 5/20/21 - routine to wait for single char input
	;from serial terminal for storage to KEYVAL
	LDI HIGH(KEYVAL)
	PHI R11
	LDI LOW(KEYVAL)
	PLO R11
	LDI $3F
	CALL TERMSND
	CALL BTERMRCV ;wait and get char from serial terminal keypress
	STR	R11 ;store value of keypress at KEYVAL
	;check for ESC keypress to terminate program execution
	XRI $1B
	LBZ ENTRY1
	LDN R11
	CALL TERMSND ;echo char back to terminal
	CALL LINEADVANCE
	RETN

;-----------------------------------------------
;MONITOR_PLOTXY - this is a coarse char plotting routine for VT100 type serial terminal.
;WORK IN PROGRESS - original code (c.1980) had this plotting to TTL video board connected to CRT

;This routine is called by the SET statement which sets R12.1=col value, R12.0=row value, R11.0=plot char.
;Routine focuses on using the VT100 ESC command to move cursor to screen position row, col (ESC[row;colH)
;Function makes use of the code fragment from 1802 Pilot at L00C2 which is used to output an error
;code to the terminal by converting a binary byte value to ascii characters.  Hence range is 0-255.
;This is used to output the row, col values needed for ESC command.

MONITOR_PLOTXY:

	;CALL SNDCHAR
	;BYTE $1B,'[s',$00 ;save current cursor position

	;send beginning of ESC command
	CALL SNDCHAR
	BYTE $1B,'[',$00
	;output row position
	GLO R12
	PLO R9
	CALL L00C2
	CALL SNDCHAR
	BYTE ';',$00
	;output col position
	GHI R12
	PLO R9
	CALL L00C2
	CALL SNDCHAR
	BYTE 'H',$00
	;output specified char to current screen cursor position
	GLO R11
	CALL SERIALSND

	;CALL SNDCHAR
	;BYTE $1B,'[u',$00 ;restore saved cursor position

	RETN


;-----------------------------------------------
;test program Quest inserted here for copy to BASIC program memory area
;by MP1802Monitor.  The EMMA emulator does not have function to load binary
;file to program memory via the MP1802Monitor load binary file function.

;[derivation of 'Quest, Roger Chaffee, BYTE Magazine, July, 1979, p.176]

	ORG ($ AND $FF00)+$100 

	BYTE $02
	BYTE 'CLS\r'
	BYTE '80 D=0\r'
	BYTE '120 PRI\"YOU WERE WALKING THROUGH THE WOODS, AND YOU\"\r'
	BYTE 'PRI\"CAME ACROSS THE ENTRANCE OF A CAVE COVERED WITH BRUSH.\"\r'
	BYTE 'PRI\"PEOPLE SAY THAT MANY YEARS AGO A PIRATE HID HIS TREASURE\"\r'
	BYTE 'PRI\"IN THESE WOODS, BUT NO ONE HAS EVER FOUND IT AND IT MAY\"\r'
	BYTE 'PRI\"STILL BE HERE.\"\r'
	BYTE 'REA M9\r'
	BYTE '85 T1=INT(RND(M9+1))\r'
	BYTE 'IF T1=0 THE 85\r' ;2024 mod
	BYTE 'IF T1=2 THE 85\r' ;2024 mod
	BYTE 'IF T1=17 THE 85\r' ;2024 mod
	BYTE 'IF T1=26 THE 85\r'
	BYTE '90 T2=INT(RND(M9+1))\r'
	BYTE 'IF T1=T2 THE 90\r'
	BYTE 'IF T2=26 THE 90\r'
	BYTE 'DIM W(42),M(6,42)\r'
	BYTE 'FOR I=1 TO M9\r'
	BYTE 'REA N\r'
	BYTE 'IF I=N THE 570\r'
	BYTE 'PRI\"DATABASE PROBLEM\";I,N\r'
	BYTE 'STOP\r'
	BYTE '570 FOR J=1 TO 6\r'
	BYTE 'REA M(J,I)\r'
	BYTE 'NEX J\r'
	BYTE 'PRI "-";\r' ;2024 mod to show data array read progress...its rather slow
	BYTE 'NEX I\r'
	BYTE 'N=5:M0=0:M6=0:T=T1:P=0:P1=0\r' ;N=5 starts outside the cave
	;BYTE 'PRI \"T1=\";T1\r' ;display treasure node for debugging
	BYTE 'FOR J=1 TO M9\r'
	BYTE 'W(J)=0\r'
	BYTE 'NEX J\r'
	BYTE 'PRI\r'
	BYTE 'GOS 8000\r'
	BYTE '1400 REM\r'
	BYTE '1420 M0=M0+1\r'
	BYTE 'GOS 6000\r'
	BYTE 'GOS 2000\r'
	BYTE 'GOS 4000\r'
	BYTE 'IF T>0 THE 1400\r'
	BYTE 'IF N<>5 THE 1400\r'
	BYTE 'GOS 3000\r'
	BYTE 'PRI\r'
	BYTE 'X9=S+10\r'
	BYTE 'PRI\"CONGRATULATIONS!  YOU GOT THE TREASURE OUT IN \";M0\r'
	BYTE 'PRI\"MOVES AND YOU GOT \";X9;\"POINTS!\"\r'
	BYTE 'PRI\"WANT TO HUNT AGAIN (Y/N)?\"\r'
	BYTE '1430 INKEY\r'
	BYTE 'IF KEY=\"Y\" THE 6000\r'
	BYTE 'IF KEY=\"N\" THE END\r'
	BYTE 'GOT 1430\r'
	BYTE '2000 REM\r'
	BYTE 'IF T<>N THE RET\r'
	BYTE 'IF T<0 THE RET\r'
	BYTE 'IF M6+5>M0 THE RET\r'
	BYTE 'PRI\"DO YOU WANT TO TAKE IT WITH YOU (Y/N)?\"\r'
	BYTE '2010 INKEY\r'
	BYTE 'IF KEY=\"Y\" THE 2300\r'
	BYTE 'IF KEY=\"N\" THE 2400\r'
	BYTE 'GOT 2010\r'
	BYTE '2300 T=(-1)\r'
	BYTE 'PRI\r'
	BYTE 'PRI\"OK, LETS GET OUT OF HERE!\"\r'
	BYTE 'RET\r'
	BYTE '2400 PRI\r'
	BYTE 'PRI\"WE\'LL LEAVE IT HERE AND YOU CAN EXPLORE SOME MORE.\"\r'
	BYTE 'M6=M0\r'
	BYTE 'RET\r'
	BYTE '3000 REM\r'
	BYTE 'S=0\r'
	BYTE 'IF T=(-1) THE S=S+5\r'
	BYTE 'IF P=1 THE S=S+10\r'
	BYTE 'FOR J=2 TO M9\r'
	BYTE 'S=S+W(J)\r'
	BYTE 'NEX J\r'
	BYTE 'RET\r'
	BYTE '4000 REM\r'
	BYTE 'IF N=T2 THE RET\r'
	BYTE 'IF P=1 THE RET\r'
	BYTE 'IF T1=T2 THE RET\r'
	BYTE 'IF T<>(-1) THE RET\r'
	BYTE 'IF N<>INT(RND(M9+1)) THE RET\r'
	BYTE 'PRI\r'
	BYTE 'PRI\"SUDDENLY THE PIRATE LEAPS OUT OF THE GLOOM AND GRABS\"\r'
	BYTE 'PRI\"THE TREASURE FROM YOU!  \'HAH!\', HE SHOUTS, \'YOU FOUND\"\r'
	BYTE 'PRI\"MY TREASURE, DID YOU?  WELL, I\'LL HIDE IT BETTER THIS\"\r'
	BYTE 'PRI\"TIME!\'  AND HE DISAPPEARS INTO THE DARKNESS.\"\r'
	BYTE 'P=1:T=T2\r'
	BYTE 'RET\r'
	BYTE '6000 REM\r'
	BYTE 'N9=N:N8=0\r'
	BYTE 'GOS 7000\r'
	BYTE 'IF N=1 THE 6120\r'
	BYTE 'N0=N\r'
	BYTE 'A0=A1\r'
	BYTE '6120 PRI\r'
	BYTE 'I=M(A1,N)\r'
	BYTE '6200 IF I=(-2) THE I=N9\r'
	BYTE 'IF D<>0 THE PRI\"DEBUG\";N;\"TO\";I\r'
	BYTE 'IF I<500 THE 6300\r'
	BYTE 'I=I-500\r'
	BYTE 'GOT 6200\r'
	BYTE '6300 IF INT(I/100)=1 THE 6340\r'
	BYTE 'IF INT(I/100)=2 THE 6370\r'
	BYTE 'N=I\r'
	BYTE 'GOT 6400\r'
	BYTE '6340 N=I-100\r'
	BYTE 'IF T=(-1) THE N=N+1\r'
	BYTE 'GOT 6400\r'
	BYTE '6370 N=I-200\r'
	BYTE 'IF T=(-1) THE N=N+P\r'
	BYTE '6400 IF N<>1 THE 6500\r'
	BYTE 'FOR J=1 TO 6\r'
	BYTE 'M(J,N)=2\r'
	BYTE 'NEX J\r'
	BYTE 'X9=7-A0\r'
	BYTE 'M(X9,N)=N0\r'
	BYTE '6500 IF N8<>2 THE GOS 8000\r'
	BYTE 'W(N)=1\r'
	BYTE 'N8=N\r'
	BYTE 'IF M(1,N)<>(-2) THEN 6800\r'
	BYTE 'I=M(6,N)\r'
	BYTE 'IF M(4,N)>100*RND(1) THEN I=M(5,N)\r'
	BYTE 'IF M(2,N)>100*RND(1) THEN I=M(3,N)\r'
	BYTE 'J=(-1)\r'
	BYTE 'IF D<>0 THEN PRI\"DEBUG BOUNCE TO\";I\r'
	BYTE 'GOTO 6200\r'
	BYTE '6800 RET\r'
	BYTE '7000 REM\r'
	BYTE 'PRI\r'
	BYTE 'PRI\"WHICH WAY (N,S,E,W,U,D,P)?\";\r'
	BYTE '7410 INKEY\r'
	BYTE 'A1=0:IF KEY=\"N\" THEN A1=1:IF KEY=\"E\" THEN A1=2\r'
	BYTE 'IF KEY=\"U\" THEN A1=3:IF KEY=\"D\" THEN A1=4\r'
	BYTE 'IF KEY=\"W\" THEN A1=5:IF KEY=\"S\" THEN A1=6\r'
	BYTE 'IF KEY=\"P\" THEN A1=7\r'
	BYTE 'IF A1=0 THEN 7410\r'
	BYTE 'IF A1<7 THEN 7400\r'
	BYTE 'GOS 3000\r'
	BYTE 'PRI\"YOU HAVE \";S;\" POINTS!\"\r'
	BYTE 'GOT 7000\r'
	BYTE '7400 IF A1=1 THEN PRI\"NORTH\"\r'
	BYTE 'IF A1=2 THEN PRI\"EAST\":IF A1=3 THEN PRI\"UP\"\r'
	BYTE 'IF A1=4 THEN PRI\"DOWN\"\r'
	BYTE 'IF A1=5 THEN PRI\"WEST\":IF A1=6 THEN PRI\"SOUTH\"\r'
	BYTE 'RET\r'
	BYTE '8000 REM\r'
	BYTE 'I=INT(N/5)\r'
	BYTE 'J=N-5*I+1\r'
	BYTE 'X9=I+1\r'
	BYTE '8100 ON X9 GOTO 8200,8210,8220,8230,8240,8250,8260,8270,8280,8290\r'
	BYTE '8200 ON J GOTO 9000,9010,9020,9030,9040\r'
	BYTE '8210 ON J GOTO 9050,9060,9070,9080,9090\r'
	BYTE '8220 ON J GOTO 9100,9110,9120,9130,9140\r'
	BYTE '8230 ON J GOTO 9150,9160,9170,9180,9190\r'
	BYTE '8240 ON J GOTO 9200,9210,9220,9230,9240\r'
	BYTE '8250 ON J GOTO 9250,9260,9270,9280,9290\r'
	BYTE '8260 ON J GOTO 9300,9310,9320,9330,9340\r'
	BYTE '8270 ON J GOTO 9350,9360,9370,9380,9390\r'
	BYTE '8280 ON J GOTO 9400,9410,9420,9430,9440\r'
	BYTE '8290 ON J GOTO 9450,9460,9470,9480,9490\r'
	BYTE '8400 IF T<>N THE 8500\r'
	BYTE 'PRI:PRI\"THE TREASURE IS HERE\"\r'
	BYTE '8500 IF T<>T2 THE 8600\r'
	BYTE 'IF T1=T2 THEN 8600\r'
	BYTE 'IF T1<>N THEN 8600\r'
	BYTE 'PRI\r'
	BYTE 'PRI\"A NOTE ON THE WALL SAYS  \'PIRATES NEVER LEAVE THEIR\"\r'
	BYTE 'PRI\"TREASURE TWICE IN THE SAME PLACE!\'\"\r'
	BYTE '8600 RET\r'
	BYTE '9000 REM\r'
	BYTE 'DAT 42\r'
	BYTE '9010 DAT 1,0,0,0,0,0,0\r'
	BYTE 'PRI\"YOU\'RE AT A DEAD END!\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9020 DAT 2,-2,101,-2,0,0,0\r'
	BYTE 'PRI\"YOU CAN\'T GO IN THAT DIRECTION\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9030 DAT 3,33,2,1,10,106,4\r'
	BYTE 'PRI\"A TUNNEL GOES NORTH-SOUTH.\"\r'
	BYTE 'PRI\"THERE IS AN OPENING TO THE WEST.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9040 DAT 4,3,30,2,11,2,1\r'
	BYTE 'PRI\"YOU\'RE ON THE BRINK OF A PIT.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9050 DAT 5,8,8,15,10,8,16\r'
	BYTE 'PRI\"YOU\'RE OUTSIDE THE CAVE.  GO SOUTH TO ENTER.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9060 DAT 6,16,3,2,10,2,2\r'
	BYTE 'PRI\"YOU\'RE AT THE HOME OF THE GNOME-KING.\"\r'
	BYTE 'PRI\"FORTUNATELY, HE\'S GONE FOR THE DAY.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9070 DAT 7,-2,101,-2,0,0,0\r'
	BYTE 'PRI\"THE GNOME KING IS HERE!  YOU\'D BETTER GET OUT!\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9080 DAT 8,18,18,15,10,18,9\r'
	BYTE 'PRI\"YOU\'RE LOST IN THE WOODS.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9090 DAT 9,-2,33,5,1,0,-2\r'
	BYTE 'GOT 8400\r'
	BYTE '9100 DAT 10,-2,101,-2,0,0,0\r'
	BYTE 'PRI\"YOU\'RE NOT GOING TO GET FAR, DIGGING THROUGH ROCK.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9110 DAT 11,1,13,4,2,1,2\r'
	BYTE 'PRI\"YOU\'RE AT THE BOTTOM OF A PIT.  A LITTLE\"\r'
	BYTE 'PRI\"STREAM FLOWS OVER THE ROCKS HERE.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9120 DAT 12,36,2,1,2,1,2\r'
	BYTE 'PRI\"YOU\'RE AT A DEAD END!\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9130 DAT 13,2,37,2,1,11,14\r'
	BYTE 'PRI\"YOU\'RE AT A WIDE SPOT.  THERE IS A SOOTY PATCH WHERE\"\r'
	BYTE 'PRI\"SOMEBODY HAS RESTED A TORCH AGAINST THE WALL.  THERE\"\r'
	BYTE 'PRI\"ARE JAGGED ROCKS ABOVE YOU.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9140 DAT 14,13,1,19,2,31,31\r'
	BYTE 'PRI\"YOUR\'RE IN A CANYON.  HIGH ON THE WALL ABOVE YOU IS\"\r'
	BYTE 'PRI\"SCRATCHED THE MESSAGE   \'BILBO WAS HERE\'\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9150 DAT 15,-2,101,-2,0,0,0\r'
	BYTE 'PRI\"YOU\'RE NOT A BIRD.  YOU CAN\'T FLY!\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9160 DAT 16,5,33,2,10,1,106\r'
	BYTE 'PRI\"YOU\'RE IN A LOW CHAMBER.  A TIGHT TUNNEL GOES EAST,\"\r'
	BYTE 'PRI\"AND YOU CAN WALK TO THE SOUTH OR WEST.  THERE IS\"\r'
	BYTE 'PRI\"LIGHT TO THE NORTH.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9170 DAT 17,-2,101,-2,0,0,0\r'
	BYTE 'PRI\"IT\'S A TIGHT SQUEEZE.  YOU CAN\'T GET PAST\"\r'
	BYTE 'PRI\"WITH THE TREASURE.\"\r'
	BYTE 'PRI\r'
	BYTE 'GOT 8400\r'
	BYTE '9180 DAT 18,-2,101,8,0,0,0\r'
	BYTE 'PRI\"I DON\'T THINK YOU CAN FIND THE CAVE.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9190 DAT 19,224,2,2,14,1,42\r'
	BYTE 'PRI\"YOU\'RE AT THE TOP OF A CLIMB.\"\r'
	BYTE 'PRI\"BELOW YOU A MESSAGE SAYS\"\r'
	BYTE 'PRI\"   \'BILBO WAS HERE\'.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9200 DAT 20,226,1,2,2,25,2\r'
	BYTE 'PRI\"YOU\'RE AT THE NORTH SIDE OF A CHASM,\"\r'
	BYTE 'PRI\"TOO WIDE TO JUMP.  RINGING ECHOES FROM\"\r'
	BYTE 'PRI\"BELOW ARE THE ONLY INDICATION OF DEPTH.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9210 DAT 21,1,226,2,2,38,25\r'
	BYTE 'PRI\"YOU\'RE IN XANADU.  BELOW YOU ALPH, THEN SACRED RIVER\"\r'
	BYTE 'PRI\"RUNS THROUGH CAVERNS MEASURELESS TO MAN,\"\r'
	BYTE 'PRI\"DOWN TO A SUNLESS SEA.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9220 DAT 22,-2,33,13,50,29,30\r'
	BYTE 'GOT 8400\r'
	BYTE '9230 DAT 23,2,1,2,31,2,2\r'
	BYTE 'PRI\"YOU\'RE ON THE LEDGE ABOVE THE GUILLOTINE ROOM.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9240 DAT 24,-2,101,19,0,0,0\r'
	BYTE 'PRI\"I HEAR THE GIANT THERE!!!\"\r'
	BYTE 'PRI\"YOU\'D BETTER GO BACK!\"\r'
	BYTE 'PRI\r'
	BYTE 'GOT 8400\r'
	BYTE '9250 DAT 25,21,20,2,2,1,19\r'
	BYTE 'PRI\"YOU\'RE IN THE GIANT\'S CAVERN.  BETTER\"\r'
	BYTE 'PRI\"NOT BE HERE WHEN THE GIANT COMES!\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9260 DAT 26,-2,65,-2,50,11,14\r'
	BYTE 'PRI\"YOU\'RE IN THE QUEST RESEARCH AND\"\r'
	BYTE 'PRI\"DEVELOPMENT AREA.  I\'M SORRY, BUT VISITORS\"\r'
	BYTE 'PRI\"ARE NOT ALLOWED.  YOU\'LL HAVE TO LEAVE.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9270 DAT 27,2,40,2,2,21,20\r'
	BYTE 'PRI\"YOU\'RE IN THE CRYSTAL PALACE.  THE\"\r'
	BYTE 'PRI\"WALLS RESONATE WITH AWESOME MUSIC.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9280 DAT 28,-2,60,221,50,14,19\r'
	BYTE 'GOT 8400\r'
	BYTE '9290 DAT 29,2,42,2,13,1,1\r'
	BYTE 'PRI\"YOU\'RE AT THE TOP OF A GIANT STALACTITE.\"\r'
	BYTE 'PRI\"YOU COULD SLIDE DOWN, BUT YOU COULDN\'T\"\r'
	BYTE 'PRI\"CLIMB BACK UP.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9300 DAT 30,34,34,2,1,4,2\r'
	BYTE 'PRI\"YOU\'RE IN A LITTLE GROTTO.  THERE IS A\"\r'
	BYTE 'PRI\"BOOK HERE CALLED JANE\'S FIGHTING SHIPS\"\r'
	BYTE 'PRI\"DATED 1763.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9310 DAT 31,14,14,23,2,1,2\r'
	BYTE 'PRI\"YOU\'RE IN THE GUILLOTINE ROOM.  A SHARP\"\r'
	BYTE 'PRI\"ROCK BALANCES PRECARIOUSLY ON THE LEDGE ABOVE YOU.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9320 DAT 32,-2,101,516,0,0,0\r'
	BYTE 'PRI\"YOU\'RE IN A CHUTE, SCRAMBLING DOWN THE\"\r'
	BYTE 'PRI\"ROCKS!  NO WAY TO STOP!  HANG ON!\"\r'
	BYTE 'PRI\r'
	BYTE 'GOT 8400\r'
	BYTE '9330 DAT 33,2,1,2,1,116,3\r'
	BYTE 'PRI\"THE TIGHT TUNNEL TURNS A CORNER.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9340 DAT 34,1,35,2,1,30,30\r'
	BYTE 'PRI\"YOU\'RE IN A LITTLE TWISTY MAZE\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9350 DAT 35,2,1,2,37,34,36\r'
	BYTE 'PRI\"YOU\'RE IN A LITTLE TWISTING MAZE\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9360 DAT 36,35,2,1,37,34,12\r'
	BYTE 'PRI\"YOU\'RE IN A TWISTING LITTLE MAZE\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9370 DAT 37,2,1,35,2,13,2\r'
	BYTE 'PRI\"YOU\'RE IN A TWISTY LITTLE MAZE\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9380 DAT 38,2,21,2,116,1,2\r'
	BYTE 'PRI\"YOU\'RE IN A PREHISTORIC DWELLING.  ON THE WALL\"\r'
	BYTE 'PRI\"ARE DRAWINGS OF BISON DONE IN RED CLAY.  THE FLOOR\"\r'
	BYTE 'PRI\"IS STREWN WITH BONES, THE REMAINS OF ANCIENT RITUALS.\"\r'
	BYTE 'PRI\"A SMALL TUNNEL GOES THROUGH THE FLOOR.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9390 DAT 39,2,40,2,32,21,26\r'
	BYTE 'PRI\"YOU\'RE IN A BLACK HOLE.  THE FORCE OF GRAVITY\"\r'
	BYTE 'PRI\"IS OVERWHELMING.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9400 DAT 40,40,40,2,2,40,41\r'
	BYTE 'PRI\"YOU\'RE IN THE LABYRINTHE\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9410 DAT 41,40,40,40,2,40,39\r'
	BYTE 'PRI\"YOU\'RE IN THE LABYRINTHE.\"\r'
	BYTE 'PRI\"IT\'S VERY DARK IN HERE.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9420 DAT 42,28,28,28,28,28,28\r'
	BYTE 'PRI\"YOU\'RE IN THE ASHRAM.  INCENSE IS HEAVY\"\r'
	BYTE 'PRI\"IN THE AIR, AND ALL DIRECTIONS SEEM THE SAME.\"\r'
	BYTE 'GOT 8400\r'
	BYTE '9430 STOP \r'
	BYTE '9999 END\r'
	BYTE $03

	END
