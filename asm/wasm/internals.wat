(module
	(import "wasi_unstable" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))

	(memory 1)
	;; Internals exports memory for all the other modules to use
	;; Internals also reserves the first 4096 bytes
	(export "memory" (memory 0))

	;; reserves bytes 0-11
	(func $__print_str (param i32) (param i32)
		(i32.store (i32.const 0) (local.get 0))
		(i32.store (i32.const 4) (local.get 1))

		(call $fd_write
			(i32.const 1) ;; file_descriptor - 1 for stdout
			(i32.const 0) ;; *iovs - The pointer to the iov array, which is stored at memory location 0
			(i32.const 1) ;; iovs_len - We're printing 1 string stored in an iov - so one.
			(i32.const 8) ;; nwritten - A place in memory to store the number of bytes written
		)
		;; TODO: return as result
		drop ;; Discard the number of bytes written from the top of the stack
	)
	(export "__print_str" (func $__print_str))

	;; reserves bytes 12-27
	(func $__print_char (param i32)
		;; Save the single character to stack and print it
		(i32.store (i32.const 12) (local.get 0))
		(i32.store (i32.const 16) (i32.const 12))
		;; Char is always length of 1
		(i32.store (i32.const 20) (i32.const 1))

		(call $fd_write
			(i32.const 1) ;; file_descriptor - 1 for stdout
			(i32.const 16) ;; *iovs - The pointer to the iov array, which is stored at memory location 0
			(i32.const 1) ;; iovs_len - We're printing 1 string stored in an iov - so one.
			(i32.const 24) ;; nwritten - A place in memory to store the number of bytes written
		)
		;; TODO: return as result
		drop ;; Discard the number of bytes written from the top of the stack
	)
	(export "__print_char" (func $__print_char))

	(func $__print_int (param i32)
		(local $int i32)
		(local $rem i32)

		(set_local $int (local.get 0))
		;; Get the reminder and print it out
		(i32.rem_u (get_local $int) (i32.const 10))
		;; 0 is ascii 48 so val+48 gives you the ascii value
		(i32.add (i32.const 48))
		;; Save reminder that we'll print later
		(set_local $rem)

		;; Divide $int by 10
		(set_local $int (i32.div_s (get_local $int) (i32.const 10)))
		;; If int is not 0, recursively call $__print_int
		(if (i32.ne (get_local $int) (i32.const 0))
			(then (call $__print_int (get_local $int)))
		)

		(get_local $rem)
		(call $__print_char)
	)
	(export "__print_int" (func $__print_int))
)
