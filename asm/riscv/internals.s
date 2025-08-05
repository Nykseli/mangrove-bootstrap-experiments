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

___print_int_rec:
	addi sp, sp, -32  # allocate space on stack
	sd   ra, 24(sp)   # save return address onto stack
	addi t0, zero, 10 # add 10 division

	# TODO: why doesn't it work with t1 or a1, and only s1?
	rem s1, a0, t0
	sd  s1, 16(sp)

	div a0, a0, t0
	beq a0, zero, ___print_int_rec_end
	call ___print_int_rec

___print_int_rec_end:
	ld s1, 16(sp)
	addi a0, s1, 48
	call __print_char

	ld   ra, 24(sp)   # load return address from stack
	addi sp, sp, 32   # restore stack pointer
	ret

# print 32 bit value
.global __print_int
__print_int:
	addi sp, sp, -32  # allocate space on stack
	sd   ra, 24(sp)   # save return address onto stack
	sd   a0, 16(sp)

	blt a0, zero, ___print_int_minus
	bgt a0, zero, ___print_int_value

___print_int_zero:
	addi a0, zero, 48 # 48 is '0' in ascii
	call __print_char
	j ___print_int_end

___print_int_minus:
	addi a0, zero, 45 # 45 is '-' in ascii
	call __print_char
	li t0, 0x7fffffff # 32 bit two's compliment value
	ld t1, 16(sp)
	and t1, t1, t0
	xor t1, t1, t0
	addi a0, t1, 1

___print_int_value:
	call ___print_int_rec

___print_int_end:
	ld   ra, 24(sp)   # load return address from stack
	addi sp, sp, 32   # restore stack pointer

	ret

# print 64 bit value
.global __print_int64
__print_int64:
	addi sp, sp, -32  # allocate space on stack
	sd   ra, 24(sp)   # save return address onto stack

	call __print_int

	ld   ra, 24(sp)   # load return address from stack
	addi sp, sp, 32   # restore stack pointer
	ret
