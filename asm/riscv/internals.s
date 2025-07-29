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


