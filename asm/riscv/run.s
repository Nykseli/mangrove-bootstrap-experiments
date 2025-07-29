.global _start

_start:
	call main

	# syscall exit 0
	addi    a0, x0, 0
	addi    a7, x0, 93
	ecall
