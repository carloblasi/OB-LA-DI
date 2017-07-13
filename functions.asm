;default rel 
sys_write:		equ		0x2000004
sys_read:		equ		0x2000003
sys_exit:		equ		0x2000001

section .bss
intbuff:		resb	21

section .text
;---------------------------------------------------------------------------------------------------;
; int readint()
; Function which reads a string and converts it to int
; Buffer to write into goes into RAX, the string will be stored in the buffer

getint:
	
	push	rcx				; save RCX value in the stack BECAUSE I say so
	push	rdx				; save RDX value in the stack
	push	rdi				; save RDI value in the stack
	push	rsi				; save RSI value in the stack
	push	rax

	mov		rsi, intbuff	; move the memory address of the buffer from RAX to RSI
	mov		rdx, 21			; number of bytes to read, last byte is for the 'new line' character
	mov		rdi, 0			; read from 'stdin'
	mov		rax, sys_read	; 3 is the number of sys_read
	syscall

.findnewline:				; RSI contains the string now
	
	mov		rdi, rsi		; move the address of the buffer into RDI to find the 'new line' 
	mov		al, 10			; move the 'new line' ASCII value (10) into AL 
	mov		rcx, 21			; move the length of the string
	repnz	scasb			; look for the 'new line'

.getnewlineindex:

	mov		rax, 21			; the character WILL BE FOUND, now we need the position of the 'new line'
	sub		rax, rcx		; 1024 (string length) - the position at which the counter stopped (it went backward)
	sub		rax, 1			; subtract 1 from the index, because the index stopped one character after finding the 'new line'

.removenewline:

	add		rsi, rax		; RSI contains the string, we add the index of the new line
	mov		rdi, rsi		; move the string into RDI to remove the character using stosb
	mov		al, 0			; the character to store in the position of the 'new line', in our case a terminator (0)
	stosb					; substitute the character

.finished:

	pop		rax
	pop		rsi				; restore RSI
	pop		rdi				; restore RDI
	pop		rdx				; restore RDX
	pop		rcx				; restore RCX
	
	mov		rax, intbuff
	call	parseint		; the integer will be stored in RAX
	ret

;---------------------------------------------------------------------------------------------------;
; int getstringlength(String message)
; String length calculation function
; String goes into RAX, string length returned in RAX

getstringlength:
	
	push	rbx 			; push the value in RBX onto the stack to preserve it while we use RBX in this function
	mov		rbx, rax		; move the address of RAX into RBX as well, both now point to the same address

.nextchar:
	
	cmp		byte [rax], 0	; compare the byte pointed to by RAX at this address with zero ('end of string')
	jz		.finished		; jump (if the zero flagged has been set) to the point in the code labeled 'finished'
	inc		rax				; increment the address in RAX by one byte (if the zero flagged has NOT been set)
	jmp		.nextchar		; loop back to 'nextchar'

.finished:
	
	sub		rax, rbx		; subtract the address in RBX from the address in RAX
							; both registers started pointing to the same address but RAX has been incremented one byte for each 
							; character in the message string, when you subtract one memory address from another of the same type
							; the result is the number of segments between them - in this case the number of bytes
	pop		rbx				; restore RBX value from the stack
	ret

;---------------------------------------------------------------------------------------------------;
; void printstring(String message)
; String printing function
; String to print goes into RAX

printstring:
	
	push	rcx				; save RCX value in the stack BECAUSE I say so
	push	rdx				; save RDX value in the stack
	push	rdi				; save RDI value in the stack
	push	rsi				; save RSI value in the stack
	push	rax				; RAX now holds the memory address of the string, we save its value in the stack while getstringlength 
							; use it

	call	getstringlength	; string length will be stored in RAX

	mov		rdx, rax		; move RAX value (string length) into RDX
	pop		rax				; retrieve RAX value from the stack, it now holds the memory address of the string

	mov		rsi, rax		; move the memory address of our string (msg) into RSI
	mov		rdi, 1			; write to 'stdout'
	mov		rax, sys_write	; 4 is the number of sys_write
	syscall

	pop		rsi				; restore RSI
	pop		rdi				; restore RDI
	pop		rdx				; restore RDX
	pop		rcx				; restore RCX
	ret

;---------------------------------------------------------------------------------------------------;
; void printstringnl(String message)
; String printing with new line function
; String to print goes in RAX

printstringnl:
	
	call	printstring		 

	push	rax 			; push RAX onto the stack to preserve it while we use the RAX register in this function
	mov		rax, 10			; move 10 into RAX - 10 is the ascii character for 'new line'
	push	rax				; push the 'new line' onto the stack so we can get the address
	mov		rax, rsp		; move the address of the current stack pointer (the address of the char '\n') into RAX for printstring
	call	printstring

	pop		rax				; remove our linefeed character from the stack
	pop		rax				; restore the original value of RAX before our function was called
	ret

;---------------------------------------------------------------------------------------------------;
; String readstring()
; Function which reads a string of max 1024 characters, (including 'new line' character -NOT ANYMORE-)
; Buffer to write into goes into RAX, the string will be stored in the buffer

readstring:
	
	push	rcx				; save RCX value in the stack BECAUSE I say so
	push	rdx				; save RDX value in the stack
	push	rdi				; save RDI value in the stack
	push	rsi				; save RSI value in the stack

	mov		rsi, rax		; move the memory address of the buffer from RAX to RSI
	mov		rdx, 1024		; number of bytes to read, last byte is for the 'new line' character
	mov		rdi, 0			; read from 'stdin'
	mov		rax, sys_read	; 3 is the number of sys_read
	syscall

.findnewline:				; RSI contains the string now
	
	mov		rdi, rsi		; move the address of the buffer into RDI to find the 'new line' 
	mov		al, 10			; move the 'new line' ASCII value (10) into AL 
	mov		rcx, 1024		; move the length of the string
	repnz	scasb			; look for the 'new line'

.getnewlineindex:

	mov		rax, 1024		; the character WILL BE FOUND, now we need the position of the 'new line'
	sub		rax, rcx		; 1024 (string length) - the position at which the counter stopped (it went backward)
	sub		rax, 1			; subtract 1 from the index, because the index stopped one character after finding the 'new line'

.removenewline:

	add		rsi, rax		; RSI contains the string, we add the index of the new line
	mov		rdi, rsi		; move the string into RDI to remove the character using stosb
	mov		al, 0			; the character to store in the position of the 'new line', in our case a terminator (0)
	stosb					; substitute the character

.finished:

	pop		rsi				; restore RSI
	pop		rdi				; restore RDI
	pop		rdx				; restore RDX
	pop		rcx				; restore RCX
	ret

;---------------------------------------------------------------------------------------------------;
; String printint(int number)
; Integer printing function, it prints negative numbers too
; Integer to print goes into RAX

printint:
	
	push	rax				; save RAX value in the stack
	push	rcx				; save RCX value in the stack
	push	rdx				; save RDX value in the stack
	push	rsi				; save RSI value in the stack
	cmp		rax, 0			; check if number to print is negative
	jl		.lessthanzero	; if it is, jump to lessthanzero
	jmp		.continue		; else, continue

.lessthanzero:				; it prints '-' before printing the number (that becomes positive)
	
	neg		rax				; negate the number to print so that it becomes positive
	push	rax				; save the number to print, we're going to use RAX
	push	45				; push 45 (ASCII code for '-') onto the stack
	mov		rax, rsp		; move the address of the top of the stack to RAX to print it
	call	printstring	
	pop		rax				; remove 45 from the top of the stack
	pop		rax				; recover the number to print (now positive) and continue

.continue:

	mov		rcx, 0			; counter of how many bytes we need to print in the end

.divideloop:
	
	inc		rcx				; count each byte (character) to print - number of characters
	mov		rdx, 0			; empty RDX
	mov		rsi, 10			; move 10 into RSI
	idiv	rsi				; RAX/RSI (n/10)
	add		rdx, 48			; convert RDX to its ASCII representation - RDX holds the remainder after a divide instruction
	push	rdx				; push RDX (string representation of an integer) onto the stack
	cmp		rax, 0			; can the integer be divided anymore?
	jnz		.divideloop		; jump if not zero to the label 'divideloop'

.printloop:

	dec		rcx				; count down each byte that we put on the stack
	mov		rax, rsp		; move the stack pointer into RAX for printing (with printstring)
	call	printstring		
	pop		rax				; remove last character from the stack to move RSP forward
	cmp		rcx, 0			; have we printed all bytes (characters) we pushed onto the stack?
	jnz		.printloop		; jump if not zero to the label 'printloop'

.finished:

	pop		rsi				; restore RSI
	pop		rdx				; restore RDX
	pop		rcx				; restore RCX
	pop		rax				; restore RAX
	ret

;---------------------------------------------------------------------------------------------------;
; String printintnl(int number)
; Integer printing with new line function
; Integer to print goes into RAX

printintnl:

	call	printint

	push	rax 			; push RAX onto the stack to preserve it while we use the RAX register in this function
	mov		rax, 10			; move 10 into RAX - 10 is the ascii character for 'new line'
	push	rax				; push the 'new line' onto the stack so we can get the address
	mov		rax, rsp		; move the address of the current stack pointer (the address of the char '\n') into RAX for printstring
	call	printstring

	pop		rax				; remove our linefeed character from the stack
	pop		rax				; restore the original value of RAX before our function was called
	ret

;---------------------------------------------------------------------------------------------------;
; int parseint(String stringnumber)
; Integer parsing function
; Move buffer to RAX, integer gets returned in RAX

parseint:

	push	rbx				; save RBX value in the stack
	push	rcx				; save RCX value in the stack
	push	rdx				; save RDX value in the stack
	push	rsi				; save RSI value in the stack
	xor		rdx, rdx		; reset RDX so it can be used as a flag for negative numbers
	mov		rsi, rax		; move pointer in RAX (number to convert) into RSI 
	mov		rax, 0			; initialise RAX with decimal value 0
	mov		rcx, 0			; initialise RCX (counter) with decimal value 0

.multiplyloop:

	xor		rbx, rbx		; resets both lower and uppper bytes of RBX to be 0
	mov		bl, [rsi + rcx]	; move a single byte into ebx register's lower half

	cmp		bl, 45			; compare RBX register's lower half value against ASCII value 45 ('-' character)
	je		.negative		; jump if equal to label 'negative'

	cmp		bl, 48			; compare RBX register's lower half value with ASCII value 48 (char value '0')
	jl		.finished		; jump if less than to label 'finished'
	cmp		bl, 57			; compare RBX register's lower half value against ASCII value 57 (char value '9')
	jg		.finished		; jump if greater than to label 'finished'
	cmp		bl, 10			; compare RBX register's lower half value against ASCII value 10 ('new line' character)
	je		.finished		; jump if equal to label 'finished'
	cmp		bl, 0			; compare RBX register's lower half value against decimal value 0 (end of string)
	jz		.finished		; jump if zero to label finished

	sub		bl, 48			; convert RBX register's lower half to decimal representation of ASCII value
	add		rax, rbx		; add RBX to our interger value in RAX
	mov		rbx, 10			; move decimal value 10 into RBX
	mul		rbx				; multiply RAX by RBX to get place value
	inc		rcx				; increment RCX (our counter register)
	jmp		.multiplyloop	; continue multiply loop

.negative:

	inc		rcx				; increment RCX (our counter register)
	push	1618			; move phi number on the stack, this will be the flag
	jmp		.multiplyloop	; continue multiply loop

.finished:
	
	mov		rbx, 10			; move decimal value 10 into RBX
	div		rbx				; divide RAX by 10

	pop		rdx				; retrieve the flag from the stack
	cmp		rdx, 1618		; is the negative flag set?
	jne		.resetstack		; if not, put the value back on the stack
	neg		rax				; else, negate RAX (the number)
	jmp		.return			; and leave the function

.resetstack:
	
	push	rdx				; put the retrieved value back on the stack, or it might break

.return:
	
	pop		rsi				; restore RSI
	pop		rdx				; restore RDX
	pop		rcx				; restore RCX
	pop		rbx				; restore RBX
	ret	

;---------------------------------------------------------------------------------------------------;
; void exit()
; Exit program and restore resources

quit:
	
	mov		rax, sys_exit
	mov		rbx, 0
	syscall
	ret

;------------div/idiv------------;
;								 ;
; dividend goes into RAX		 ;
; divisor goes into any register ;
; quotient goes into RAX		 ;
; remainder goes into RDX(?)	 ;
;								 ;
;---------------ex---------------;
;								 ;
;	mov		rax, 4				 ;
;	mov		rsi, 2				 ;
;	(i)div	rsi		; (4/2)		 ;
;								 ;
;--------------------------------;

;--------------mul/imul-------------;
;								 	;
; 1st factor goes into RAX		 	;
; 2nd factor goes into any register ;
;								 	;
;-----------------ex----------------;
;								 	;
;	mov		rax, 4				 	;
;	mov		rsi, 2				 	;
;	(i)mul	rsi		; (4*2)		 	;
;								 	;
;-----------------------------------;

;------------------32 bit---------64 bit----;
;											;
; 1st argument		ebx				rdi 	;
; 2nd argument		ecx				rsi		;
; 3rd argument		edx				rdx		;
; 4th argument		esi				rcx		;
; 5th argument		edi				r8		;
; 6th argument		stack			r9		;
;											;
;-------------------------------------------;
