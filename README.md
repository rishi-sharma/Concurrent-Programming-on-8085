# Concurrent-Programming-on-8085
An 8085 assembly program for running assembly level programs concurrently on 8085

USER GUIDELINES

a) Programming Guidelines

	1.Stack is of fixed size of 1 KB

	2.Instead of PUSH instruction user need to call following memory locations for corresponding pair of registers
		8150H - PSW
		8188H - BC
		81C0H - DE
		81F8H - HL

	3.RST 5.5 interrupt can't be used.

	4.For using any other interrupt, user must enable the interrupts at the starting of ISR.

	5.Interrupt can be used only in one of the 12 programs.

	6.For terminating a program, “JMP 80DB” (for priority based) or “JMP 80D3” (for Round Robin) instruction must be used. Any other instruction such as HLT or RST 5 are not allowed.

	7.Memory available for source codes and other purposes is from A7C8H to FFFFH which amounts to 22854 Bytes of memory. No two programs should share memory for working, as they may corrupt each other’s data while running concurrently.

b) Execution Guidelines

	1.Load the program in user space (A7C8H to FFFFH). Maximum of 12 programs should be loaded

	2.Write the starting addresses of each program starting from memory location 9000H and terminate it with 00H.
	  Example: 3 programs with starting addresses A800H, A900H and AA00H. Then values at memory locations starting from 9000H should be
		9000H – A8H
		9001H – 00H
		9002H – A9H
		9003H – 00H
		9004H – AAH
		9005H – 00H
		9006H – 00H //00H Terminated array

	3. Start execution from memory location 8000H.

	4. On completion of all programs, the default message (“UPS 85”) gets displayed on MPS 85-3 board LEDs.

	5. Program's exit status code (2 Bytes) can be checked from memory location starting at A7AAH. Two bytes for each
	   program’s status code. 
	   Example: 4th program’s exit status code saved at A7AAH + (2*(4-1))H = A7AAH + 6H = A7B0H.
