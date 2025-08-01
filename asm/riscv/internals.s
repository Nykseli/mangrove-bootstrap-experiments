.global __print_str
__print_str:
	addi sp, sp, -32  # allocate space on stack
	sd   ra, 24(sp)   # save return address onto stack

	add   a2, x0, a1  # Move the size argument to a2
	add   a1, x0, a0  # Move the char* argument to a1
	addi  a0, x0, 1   # print stdout
	addi  a7, x0, 64  # set ecall to write function
	ecall             # Call the function

	ld   ra, 24(sp)   # load return address from stack
	addi sp, sp, 32   # restore stack pointer

	ret

.global __print_char
__print_char:
	addi sp, sp, -32  # allocate space on stack
	sd   ra, 24(sp)   # save return address onto stack
	addi t0, sp, 32   # save start of stack pointer to t0

	# create a char s[1] = {arg0}
	sb   a0, -16(t0)  # load the first byte of the argument to stack
	add  a0, t0, -16  # point arg to the stack
	addi a1, zero, 1  # add the length argument
	call __print_str

	ld   ra, 24(sp)   # load return address from stack
	addi sp, sp, 32   # restore stack pointer

	ret
