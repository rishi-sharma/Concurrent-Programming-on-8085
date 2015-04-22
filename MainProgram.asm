cpu "8085.tbl"
hof "int8"
;	 		First line added
; 			Memory Layout
;
;			8000H		____
;			|	 |			|  3984 Bytes of memory.
;			|	 |			|  (Reserved for Main program, User program's addresses)
;			|	 |		____|
;			8F8FH
;			.	 .		____  
;			.	 .		____|	Memory utilized by system.
;			9000H		____
;			|	 |			|  28761 Bytes of memory.
;			|	 |			|  (User programs and their stacks)
;			|	 |			|
;			|	 |		____|
;			FFFFH
;
;			9000H - 9018H      Array of program addresses (MSB AND THEN LSB)
;			9019H - 9400H	   Stack for Process 1
;			9401H - 97E8H	   Stack for Process 2
;			97E9H - 9BD0H	   Stack for Process 3
;			9BD1H - 9FB8H	   Stack for Process 4
;			9FB9H - A3A0H	   Stack for Process 5
;			A3A1H - A788H	   Stack for Process 6
;			A789H - A794H	   List of running processes
;			A795H,A796H		   List pointer (Points to the next process to be run)
;			A797H			   Main program stack pointer LSB
;			A798H 			   Main program stack pointer MSB
;			A799H			   Initial processes count
;			A79AH  			   Temporary variable
;			A79BH			   Time slice
; 			A79CH   		   Temporary 16 bit variable
;			A79EH			   Runnning process count
;			A79FH 			   Current process index
;			A7A0H - A7A6H 	   Array for storing time slices alloted to each process
;			A7A7H 			   Pointer to array of time slices
;			A7A9H			   Pointer to array (Only LSB as MSB is always 90H)
;			A7AAH - A7C1H	   Array for storing program exit status code
;			A7C2H - A7C7H 	   Array for storing index of each process in program array.
;			STACK ORGANIZTION
;
;			|__BC___|
;			|__DE___|
;			|__HL___|
;			|__PSW__|
;			|__PC___|
;			| USER	|
;			|_SPACE_|
;
;
; Guidelines:
;
; 
;
;

ORG 8FB3H
JMP CONT_SWITCH

ORG 8000H
JMP MAINPROG

INIT:NOP	  	   ; Fills the stack PC with starting address of programs and rest all zeroes.
				   ; and initializes the queue with initial stack pointers 
	LXI B,03E8H	   ; Hex value of 1000 to be added to stack pointer to get to next stack
	JMP WHILE1

	PUSH_STACK:NOP ; Push the PC for process into its stack
		
		LXI H,0H 		; Storing the main program SP  
		DAD SP 			; to 9797H and 9798H	
		SHLD 0A797H 	;

		LHLD 0A7A9H		; 
		MVI H,90H 		; 
		MOV C,M 		; Loading the array pointer and moving the 
		INX H 			; program's PC (Program Counter) to BC register pair
		MOV B,M 		;
		

   		LHLD 0A79CH     ; Stack pointer to the current stack
		SPHL 			; Setting Stack pointer
		PUSH B 			; Push the starting address of the program and 
		LXI B,0H 		; then its registers values all intialized to zero.
		PUSH B 			; 
		PUSH B 			;
		PUSH B 			;
		PUSH B 			;

		LXI H,00H 		;
		DAD SP 			;
		DCX H 			;
		MOV B,H 		; Stores the value of stack pointer in queue
		MOV C,L 		; and increments the queue pointer
		LHLD 0A795H 	;
		MOV M,C 		;
		INX H 			;
		MOV M,B 		;
		INX H 			;
		SHLD 0A795H 	;
		
		LHLD 0A797H 	; Restore the main program stack pointer.
		SPHL 			;
		
		LHLD 0A79CH     ; Stack pointer to the current stack (initially to 0 level)
		LXI B,03E8H 	; 
		DAD B 			; Increment the stack pointer by 1000 (03E8H) to point
		SHLD 0A79CH		;
		LHLD 0A7A9H		; to next stack base. And increase the array pointer to
		MVI H,90H       ;
		INX H 			; point to next program's address.
		INX H 			;
		RET 			;

	WHILE1:NOP
		

		LDA 0A799H 		;
		INR A 			; Increment  the initial process count by 1
		STA 0A799H 		;

		CALL PUSH_STACK ; Push initial values to the stack

		MOV A,L 	 	; Update the array pointer
		STA 0A7A9H	 	;
		CPI 0CH  	 	; Check if we have filled the queue fully
		RZ			 	;

		MOV A,M 	 	;
		CPI 0H  	 	; Or if all the programs have been loaded by checking
		RZ 		    	; the memory content.

	JMP WHILE1

START_EXEC:NOP

	MOV E,M 			; Store the stack pointer to DE register pair
	INX H 				;
	MOV D,M 			;
	INX H

	SHLD 0A795H 		; Update the queue pointer and set 
	XCHG 				; the current stack's pointer
	SPHL 				;

	POP B 				; Pop out the initial values of registers except
	POP D 				; for PSW because A register needs to be modified
	POP H 				; to set the timer

	MVI A,090H  ; CONFIGURE TIMER 2 TO COUNT DOWN AND ON COMPLETION SET OUT2 TO HIGH (1) 
	OUT 13H     ;
	LDA 0A79BH  ; Load the time slice
	OUT 12H     ;

	POP PSW     ; Pop the PSW register pair
	NOP  		; 
	EI 			; Enable interrupts and RET instruction for restoring
	RET 		; the program counter


INC_QUEUE_POINTER:NOP ; Function to check if queue pointer has 
					  ; reached end point reset it to start.
	
	LDA 0A799H   	  ; Calculating the address of last queue point
	ADD A 			  ; using the initial process count
	ADI 89H 		  ;

	CMP L
	RNZ
	LXI H,0A789H
	SHLD 0A795H
	MVI B,00H
	RET

CONT_SWITCH:NOP 	    ; Sub routine for performing context switch

	PUSH PSW 		    ; 
	PUSH H 			    ; Push all the register pairs onto the current stack
	PUSH D 			    ;
	PUSH B 				;


	LXI H,00H 			; Get the current stack pointer and update it
	DAD SP 				; in the queue at the current stack's position
	XCHG 				; by moving queue pointer
	LHLD 0A795H 		;
	DCX H 				;
	MOV M,D 			;
	DCX H 				;
	MOV M,E 			;
	INX H 				;
	INX H 				;

	LDA 0A79FH 			; Load the current process index
	MOV B,A
	CHECK_EMPTY:NOP     		; Checks if the process in the queue is over.

		INR B 					; Increase the current process index
		CALL INC_QUEUE_POINTER
		MOV E,M
		INX H
		MOV D,M
		INX H
		MVI A,00H 				; Compare if the MSB of the stack pointer is 00. If YES
		CMP D 					; then continue checking further.

	JZ CHECK_EMPTY

	MOV A,B
	STA 0A79FH 			; Update the current process index
	ADI 0A0H
	STA 0A7A7H 			; Update the time slice array pointer
	SHLD 0A795H 		; Update the queue pointer
	XCHG
	MVI A,090H   		; CONFIGURE TIMER 2 TO COUNT DOWN AND  
	OUT 13H 			; ON COMPLETION SET OUT2 TO HIGH
	XCHG 				;
	LHLD 0A7A7H			; Load the time slice from 0A7A7H
	MOV A,M 			;
	XCHG 				;
	OUT 12H 			;

	SPHL 				; Change the stack pointer and restore all the 
	POP B 				; register pairs of the new stack
	POP D 				;
	POP H 				;
	POP PSW 			;

	NOP 				; Enable interrupts and RET instruction to
	EI 					; restore the program counter (PC)
	RET 				;


ENDALL:NOP 				; Sub routine to be called if all the processes are over.
	RST 5


TERMINATE:NOP 			; Sub routine to exit the current process and bring other if available
	
	LHLD 0A7A7H
	LDA 0A79BH
	MOV D,A
	MVI B,06H
	LOOP5:NOP
	MOV A,M
	ADD D
	MOV M,A
	INX H
	MVI A,0A6H
	CMP L
	JNZ CONT10
	MVI L,0A0H
	CONT10:NOP
	DCR B
	MVI A,00H
	CMP B
	JNZ LOOP5
	LDA 0A79BH
	MOV M,A
	LHLD 0A7A9H 			; Load the array pointer and check if the 
	MVI H,90H 			; MSB of the array value is 0. If NO, it means more
	MOV D,M 			; programs are available, so jump to the New process
	MVI A,00H 			; bringing part of the code other wise write 00H to
	CMP D 				; current stack's location in queue
	JNZ NEW_PROCESS 	;

	LHLD 0A795H 		; Load the queue pointer and decrease
	DCX H 				; it and write 00H to the current 
	MVI M,00H 			; stack's location in queue pointer.
	DCX H 				;
	MVI M,00H 			;

	OUT 12H 			; Stop the timer by writing 1 byte to its data bus

	LDA 0A79EH 			; If running count has become zero then
	DCR A 				; exit the whole program other wise just decrease 
	JZ ENDALL 			; the running process count by 1
	STA 0A79EH 			;

	LDA 0A79FH
	MOV B,A 			; Jump to the CHECK_EMPTY part of the context switch
	LHLD 0A795H 		; as we don't need to store current process's registers
	JMP CHECK_EMPTY 	; as it is over and no more programs are available in array

	NEW_PROCESS:NOP 	; Sub routine to bring new program into the queue

		INX H 			; Get the array pointer and increment it to point
		MOV E,M 		; to next program. Program counter is stored in DE 
		INX H 			; register pair
		MOV A,L 		;
		STA 0A7A9H 		;


		LDA 0A79FH 		; Load the current process index
		LXI H,0A7C2H
		ADD L
		MOV L,A
		LDA 0A7A9H
		DCR A
		DCR A
		MOV M,A

		LXI H,9400H 	;
		LXI B,3E8H 		;
		LDA 0A79FH
		WHILE:NOP 		; Loop for adding 3E8H to 9400H 
			CPI 0H 		; as many times as shift of queue pointer
			JZ CONT 	;
			DAD B 		;
			DCR A 		;
		JMP WHILE 		;

		CONT:NOP 		; Set the stack pointer and program counter to start
		SPHL 			; the execution of new brought program
		XCHG 			;
		PCHL 			;

PUSH_PSW:NOP 			; Subroutine for Pushing PSW

	PUSH PSW 				; Store the PSW reg pair to HL reg pair
	POP H 					;

	SHLD 0A79CH 			; Store it to memory for further use

	LDA 0A79FH		; Load the current process index to get the shift of	
	LXI H,9019H 	; queue pointer from the start of queue. Then
	LXI B,3E8H 		; add 1000 (3E8H) to upper limit of stack 1 for number
	WHILE2:NOP 		; of times it is shifted from start to get the
		CPI 0H 		; upper limit of current stack
		JZ CONT2 	;
		DAD B 		;
		DCR A 		;
	JMP WHILE2 		;

	CONT2:NOP 		;
	XCHG 			; Load the stack pointer to HL reg and compare
	LXI H,00H 		; it with the Upper limit. If found lower
	DAD SP 			; then jump to TERMINATE otherwise
	DCX H 			; jump to the code for pushing
	DCX H 			;
	MOV A,H 		;
	CMP D 			;
	JC O_TERMINATE 	;
	CMP D 			;
	JNZ PUSHA 		;
	MOV A,L 		;
	CMP E 			;
	JC O_TERMINATE 	;

	PUSHA:NOP 		; Pop the program counter, store it in HL reg pair,
	LHLD 0A79CH		; Push the required reg pair and then restore
	POP D 			; the program counter
	PUSH H 			;
	XCHG 			;
	PCHL 			;

PUSH_B:NOP 			; Subroutine for Pushing B

	PUSH B 				; Store the B reg pair to HL reg pair
	POP H 					;

	SHLD 0A79CH 			; Store it to memory for further use

	LDA 0A79FH		; Load the current process index to get the shift of	
	LXI H,9019H 	; queue pointer from the start of queue. Then
	LXI B,3E8H 		; add 1000 (3E8H) to upper limit of stack 1 for number
	WHILE3:NOP 		; of times it is shifted from start to get the
		CPI 0H 		; upper limit of current stack
		JZ CONT3 	;
		DAD B 		;
		DCR A 		;
	JMP WHILE3 		;

	CONT3:NOP 		;
	XCHG 			; Load the stack pointer to HL reg and compare
	LXI H,00H 		; it with the Upper limit. If found lower
	DAD SP 			; then jump to TERMINATE otherwise
	DCX H 			; jump to the code for pushing
	DCX H 			;
	MOV A,H 		;
	CMP D 			;
	JC O_TERMINATE 	;
	CMP D 			;
	JNZ PUSHB 		;
	MOV A,L 		;
	CMP E 			;
	JC O_TERMINATE 	;

	PUSHB:NOP 		; Pop the program counter, store it in HL reg pair,
	LHLD 0A79CH		; Push the required reg pair and then restore
	POP D 			; the program counter
	PUSH H 			;
	XCHG 			;
	PCHL 			;


PUSH_D:NOP 			; Subroutine for Pushing D

	PUSH D 				; Store the D reg pair to HL reg pair
	POP H 					;

	SHLD 0A79CH 			; Store it to memory for further use

	LDA 0A79FH		; Load the current process index to get the shift of	
	LXI H,9019H 	; queue pointer from the start of queue. Then
	LXI B,3E8H 		; add 1000 (3E8H) to upper limit of stack 1 for number
	WHILE4:NOP 		; of times it is shifted from start to get the
		CPI 0H 		; upper limit of current stack
		JZ CONT4 	;
		DAD B 		;
		DCR A 		;
	JMP WHILE4 		;

	CONT4:NOP 		;
	XCHG 			; Load the stack pointer to HL reg and compare
	LXI H,00H 		; it with the Upper limit. If found lower
	DAD SP 			; then jump to TERMINATE otherwise
	DCX H 			; jump to the code for pushing
	DCX H 			;
	MOV A,H 		;
	CMP D 			;
	JC O_TERMINATE 	;
	CMP D 			;
	JNZ PUSHD 		;
	MOV A,L 		;
	CMP E 			;
	JC O_TERMINATE 	;

	PUSHD:NOP 		; Pop the program counter, store it in HL reg pair,
	LHLD 0A79CH		; Push the required reg pair and then restore
	POP D 			; the program counter
	PUSH H 			;
	XCHG 			;
	PCHL 			;


PUSH_H:NOP 			; Subroutine for Pushing H

	PUSH H 				    ; Store the H reg pair to HL reg pair
	POP H 					;

	SHLD 0A79CH 			; Store it to memory for further use

	LDA 0A79FH		; Load the current process index to get the shift of	
	LXI H,9019H 	; queue pointer from the start of queue. Then
	LXI B,3E8H 		; add 1000 (3E8H) to upper limit of stack 1 for number
	WHILE5:NOP 		; of times it is shifted from start to get the
		CPI 0H 		; upper limit of current stack
		JZ CONT5 	;
		DAD B 		;
		DCR A 		;
	JMP WHILE5 		;

	CONT5:NOP 		;
	XCHG 			; Load the stack pointer to HL reg and compare
	LXI H,00H 		; it with the Upper limit. If found lower
	DAD SP 			; then jump to TERMINATE otherwise
	DCX H 			; jump to the code for pushing
	DCX H 			;
	MOV A,H 		;
	CMP D 			;
	JC O_TERMINATE 	;
	CMP D 			;
	JNZ PUSHH 		;
	MOV A,L 		;
	CMP E 			;
	JC O_TERMINATE 	;

	PUSHH:NOP 		; Pop the program counter, store it in HL reg pair,
	LHLD 0A79CH		; Push the required reg pair and then restore
	POP D 			; the program counter
	PUSH H 			;
	XCHG 			;
	PCHL 			;


O_TERMINATE:NOP 	; Subroutine called when stack overflow occurs
	POP B 			; Store the program counter of program as exit status code
	LDA 0A79FH
	LXI H,0A7C2H
	ADD L
	MOV L,A
	MOV A,M
	LXI H,0A7AAH
	ADD L
	MOV L,A
	MOV M,B
	INX H
	MOV M,C
	JMP TERMINATE

TIME_INIT:NOP 	   ; Code for initiazling the time slices array
	LHLD 0A7A7H
	LDA 0A79BH
	MOV C,A
	MVI A,06H
	LOOP6:NOP
		MOV M,C
		INX H
		DCR A
	JNZ LOOP6
	RET

STATUS_INIT:NOP     ; Code for initializing the all programs exit status with zero
	LXI H,0A7AAH 
	MVI A,0CH
	LOOP7:NOP
		MVI M,00H
		INX H
		MVI M,00H
		INX H
		DCR A
	JNZ LOOP7
	LXI H,0A7C2H
	MVI A,00H
	LOOP8:NOP
		MOV M,A
		INX H
		INR A
		INR A
		CPI 0CH
	JNZ LOOP8
	RET

MAINPROG:NOP
	
	MVI A,1EH		; Unmask to use RST 5.5 as INTERRUPT
	SIM				;

	MVI A,76H		;
	OUT 13H			; Configuring TIMER 1 to use as square wave generator
	MVI A,0FEH		; with FREQUENCY = (1.5 MHZ)/(FFFEH) for TIMER 2'S CLOCK
	OUT 11H			;
	MVI A,0FFH		;
	OUT 11H			;

	LXI H,0A789H	; Initializing QUEUE POINTER
	SHLD 0A795H		;

	LXI H,8F8FH		; Setting MAIN PROGRAM'S STACK POINTER to 8F8FH
	SPHL			;

	MVI A,00H 		; Initializing ARRAY'S POINTER, INITIAL PROCESS COUNT and
	STA 0A7A9H		; CURRENT PROCESS INDEX to zero (0)
	STA 0A799H 		;
	STA 0A79FH

	LXI H,9400H		;
	SHLD 0A79CH     ; Initializing the FIRST STACK'S POINTER
	CALL INIT  		; Calling STACK AND QUEUE INITIALIZATION subroutine

	MVI A,02H		; Setting the TIME SLICE
	STA 0A79BH 		;
	LXI H,0A7A0H 	; Initialize time slice array pointer
	SHLD 0A7A7H 	;
	CALL TIME_INIT  ; Initialize each process's time slice
	CALL STATUS_INIT; Initialize each program's status code

	LDA 0A799H 		; Initialiazing RUNNING PROCESS COUNT
	STA 0A79EH		;

	LXI H,0A789H    ; Reseting the queue pointer and starting the EXECUTION of 
	SHLD 0A795H     ; FIRST PROGRAM
	CALL START_EXEC ;
	RST 5
