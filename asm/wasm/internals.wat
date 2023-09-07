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
)
