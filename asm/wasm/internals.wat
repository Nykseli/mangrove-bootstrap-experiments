(module
	(import "wasi_unstable" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))

	;; Count of 64KiB pages
	(memory 100)
	;; Internals exports memory for all the other modules to use
	;; Internals also reserves the first 1kib bytes.
	;; 4096 bytes for internal things, rest is for stack.
	(export "memory" (memory 0))

	;; reserves bytes 0-11
	(func $__print_str (param i64)
		;; Function structure is packed into i64 that we can just point
		;; load into data structre and treat the data as i32 and wasm
		;; will understand ptr is stored in the high i32 and length in low i32
		(i64.store (i32.const 0) (local.get 0))

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
		;; print - prefix if negative bit is set
		(if (i32.and (i32.const 0x80000000) (get_local $int))
			(then
				;; 45 is acii '-'
				(call $__print_char (i32.const 45))
				;; Turn the two's compiliment negative value into regular positive value
				(set_local $int (i32.and (i32.const 0x7fffffff) (get_local $int)))
				(set_local $int (i32.xor (i32.const 0x7fffffff) (get_local $int)))
				(set_local $int (i32.add (i32.const 1) (get_local $int)))
			)
		)
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

	(func $__print_int64 (param i64)
		(local $int i64)
		(local $rem i32)

		(set_local $int (local.get 0))
		;; print - prefix if negative bit is set
		;; if requires a i32 value so we need to be a bit hacky with right shift
		(if (i32.wrap_i64 (i64.shr_u (get_local $int) (i64.const 63)))
			(then
				;; 45 is acii '-'
				(call $__print_char (i32.const 45))
				;; Turn the two's compiliment negative value into regular positive value
				(set_local $int (i64.and (i64.const 0x7fffffffffffffff) (get_local $int)))
				(set_local $int (i64.xor (i64.const 0x7fffffffffffffff) (get_local $int)))
				(set_local $int (i64.add (i64.const 1) (get_local $int)))
			)
		)
		;; Get the reminder and print it out
		(i64.rem_u (get_local $int) (i64.const 10))
		;; 0 is ascii 48 so val+48 gives you the ascii value
		(i64.add (i64.const 48))
		;; Down cast to i32 so we can print the value with __print_char
		(i32.wrap_i64)
		;; Save reminder that we'll print later
		(set_local $rem)
		;; Divide $int by 10
		(set_local $int (i64.div_s (get_local $int) (i64.const 10)))
		;; If int is not 0, recursively call $__print_int
		(if (i64.ne (get_local $int) (i64.const 0))
			(then (call $__print_int64 (get_local $int)))
		)

		(get_local $rem)
		(call $__print_char)
	)
	(export "__print_int64" (func $__print_int64))

	(func $__print_str_ptr (param i32)
		(call $fd_write
			(i32.const 1) ;; file_descriptor - 1 for stdout
			(local.get 0) ;; *iovs - The pointer to the iov array, which is stored at memory location 0
			(i32.const 1) ;; iovs_len - We're printing 1 string stored in an iov - so one.
			(i32.const 8) ;; nwritten - A place in memory to store the number of bytes written
		)
		;; TODO: return as result
		drop ;; Discard the number of bytes written from the top of the stack
	)
	(export "__print_str_ptr" (func $__print_str_ptr))

	;; Storing and loading 8bit numbers
	;; https://developer.mozilla.org/en-US/docs/webassembly/reference/memory/store

	;; Helper function for finding memory blocks
	(func $__next_block (param i32) (result i32)
		(local $rem i32)
		;; How many bytes until the next 64 byte
		(i32.rem_u (local.get 0) (i32.const 64))
		(set_local $rem)
		(i32.sub (i32.const 64) (get_local $rem))
		(set_local $rem)
		;; Add the amount of reminding bytes to start
		(i32.add (local.get 0) (get_local $rem))
	)

	;; Memory reserves bytes 28-127
	;; __init_memory sets the "heap" pointer to be the next 64 bit block.
	;; This is used to set the heap to start. The start is saved to address 28-31
	(func $__init_memory (param i32)
		(local $start i32)
		(local $rem i32)

		(call $__next_block (local.get 0))
		(set_local $start)
		;; save the amount to address 28
		(i32.store (i32.const 28) (get_local $start))
		;; Save the current pointer to 32
		(i32.store (i32.const 32) (get_local $start))
		;; Save the stack pointer addr (starts after internal memory)
		(i32.store (i32.const 128) (i32.const 4096))
	)
	(export "__init_memory" (func $__init_memory))

	;; Find a freed block of memory that fit's the required size.
	;; Returns zero if no address is found.
	(func $__find_freed_block (param i32) (result i32)
		(local $size i32)
		(local $current i32)

		;; loop from the start of the heap and try to find freed block of memory that's big enough.
		(set_local $current (i32.load (i32.const 28)))
		(loop $finder
			(set_local $size (i32.load (get_local $current)))
			;; Check if it's free
			(if (i32.xor (get_local $size) (i32.const 0x80000000))
				(then
					;; If the size is 0, this block is never used.
					;; It means that all the blocks are used or two small and
					;; a new one needs to be allocated
					(if (i32.eq (get_local $size) (i32.const 0))
						(then (return (i32.const 0)))
					)
					;; If the size is less than or equal to the block's size
					;; return the starting address
					(if (i32.le_s (get_local $size) (local.get 0))
						(then (return
							(i32.add (get_local $current) (i32.const 8))
						))
					)
				)
			)

			;; get the size without the used bit
			(set_local $size (i32.and (get_local $current) (i32.const 0x7fffffff)))
			;; Add the size + the 8 bytes for the metadata
			(set_local $current (i32.add (get_local $size) (i32.const 8)))
		)
		(i32.const 0)
	)

	;; Reserve N bytes that fit into 64 byte blocks
	;; The data is saved in a followign structure:
	;; +--------------------------------+
	;; |0         1         2         3 |
	;; +--------------------------------+
	;; |01234567890123456789012345678901|
	;; +-+------------------------------+
	;; |u|          data size           |
	;; |s|              in              |
	;; |e|             bytes            |
	;; |d|             (31)             |
	;; +-+------------------------------+
	;; |            reserved            |
	;; +--------------------------------+
	;; |        First data block        |
	;; |           (56 bytes)           |
	;; +--------------------------------+
	;; |    Next optional data blocks   |
	;; |         (64 bytes each)        |
	;; +--------------------------------+
	;;
	;; This function returns the address off the first data block.
	;; When you need to access the meta data, just subtract 8 from the address
	;; of the first data block.
	(func $__allocate_bytes (param i32) (result i32)
		(local $next i32)
		(local $current i32)
		(local $aligned_size i32)

		(set_local $current (call $__find_freed_block (local.get 0)))
		(if (get_local $current)
			(then (return (get_local $current)))
		)

		;; Get the current heap address
		(set_local $current (i32.load (i32.const 32)))
		;; Make sure there's enough room for the pointer structure
		(set_local $aligned_size (call $__next_block (i32.add (local.get 0) (i32.const 8))))
		;; update the current heap address
		(set_local $next (i32.add (get_local $current) (get_local $aligned_size)))
		(i32.store (i32.const 32) (get_local $next))
		;; set data size - the 8 bytes for metadata
		(i32.store (get_local $current) (i32.rem_u (get_local $aligned_size) (i32.const 8)))
		;; mark as used
		(i32.store
			(get_local $current)
			(i32.or (i32.load (get_local $current)) (i32.const 0x8000))
		)
		;; Save the start address
		(i32.store
			(i32.add (get_local $current) (i32.const 4))
			(i32.add (get_local $current) (i32.const 8))
		)
		;; return the start address
		(i32.add (get_local $current) (i32.const 8))
	)
	(export "__allocate_bytes" (func $__allocate_bytes))

	(func $__free_bytes (param i32)
		;; metadata pointer
		(local $md_ptr i32)
		(local $metadata i32)
		(local $heap_start i32)

		;; First "userspace" adrress is heapstart + metadata size
		(set_local $heap_start (i32.add (i32.load (i32.const 28)) (i32.const 8)))
		;; if the address is not in heap, it cannot be freed so we can just ignore it
		(if (i32.lt_u (local.get 0) (get_local $heap_start))
			(then return)
		)

		(set_local $md_ptr (i32.sub (local.get 0) (i32.const 8)))
		(set_local $metadata (i32.load (get_local $md_ptr)))
		;; Remove used flag
		(set_local $metadata (i32.and (get_local $metadata) (i32.const 0x7fffffff)))
		;; Store the metadata without the used flag
		(i32.store (get_local $md_ptr) (get_local $metadata))
	)
	(export "__free_bytes" (func $__free_bytes))

	;; target, source, n bytes
	(func $__mem_n_copy (param i32) (param i32) (param i32)
		(local $counter i32)
		(set_local $counter (i32.const 0))

		(loop $copy
			;; Store single byte from source to target
			(i32.store
				;; target address + offset
				(i32.add (local.get 0) (get_local $counter))
				;; value from src (address + offset)
				(i32.load (i32.add (local.get 1) (get_local $counter)))
			)

			(set_local $counter (i32.add (get_local $counter) (i32.const 1)))
			;; Continue loop until we have copied n bytes
			(br_if $copy (i32.lt_s (get_local $counter) (local.get 2)))
		)
	)
	(export "__mem_n_copy" (func $__mem_n_copy))

	;; Concat string1 and string2 pointers, create a new string
	;; and return the pointer to the newly created string
	;; @depracated for in favor of __string_concat2
	(func $__string_concat (param i32) (param i32) (result i32)
		(local $size1 i32)
		(local $size2 i32)
		(local $str_size i32)
		(local $new_ptr i32)
		(local $new_string i32)

		;; string length is ptr+4
		(set_local $size1 (i32.load (i32.add (local.get 0) (i32.const 4))))
		(set_local $size2 (i32.load (i32.add (local.get 1) (i32.const 4))))
		;; Add sizes together to get total size
		(set_local $str_size (i32.add (get_local $size1) (get_local $size2)))
		;; Add 8 bytes to str_size to make sure we can save the ptr and size
		;; values to first 8 bytes
		(set_local $str_size (i32.add (get_local $str_size) (i32.const 8)))
		;; Allocate new string and get the addr pointer to it
		(set_local $new_string (call $__allocate_bytes (get_local $str_size)))
		;; Get the pointer to the heap allocated string (8 bytes after the data info)
		(set_local $new_ptr (i32.add (get_local $new_string) (i32.const 8)))
		;; Save the size info to the string pointer (offset 4)
		(i32.store
			;; size addr
			(i32.add (get_local $new_string) (i32.const 4))
			;; size value
			(i32.add (get_local $size1) (get_local $size2))
		)
		;; Save the pointer to string data to the first 4 bytes of the string ptr
		(i32.store (get_local $new_string) (get_local $new_ptr))

		;; Copy the first string into new string
		(call $__mem_n_copy (get_local $new_ptr) (i32.load (local.get 0)) (get_local $size1))
		;; Copy the second string into new string
		(call $__mem_n_copy
			;; string address + offset of the first string
			(i32.add (get_local $new_ptr) (get_local $size1))
			(i32.load (local.get 1))
			(get_local $size2)
		)

		;; finally return the newly created string
		(get_local $new_string)
	)
	(export "__string_concat" (func $__string_concat))

	;; __string_concat2 takes the two i64 structs, combines them
	;; and returns thew i64 that represents the string, the ptr value
	;; points to a newly allocated memory location that needs to be freed later
	(func $__string_concat2 (param i64) (param i64) (result i64)
		(local $size1 i32)
		(local $size2 i32)
		(local $str_size i32)
		(local $new_ptr i32)
		(local $new_string i64)

		;; Sizes are low i32 bits
		(set_local $size1 (i32.wrap_i64 (i64.shr_u (local.get 0) (i64.const 32))))
		(set_local $size2 (i32.wrap_i64 (i64.shr_u (local.get 1) (i64.const 32))))
		;; Add sizes together to get total size
		(set_local $str_size (i32.add (get_local $size1) (get_local $size2)))
		;; Allocate new string and get the addr pointer to it
		(set_local $new_ptr (call $__allocate_bytes (get_local $str_size)))

		;; Copy the first string into new string
		(call $__mem_n_copy (get_local $new_ptr) (i32.wrap_i64 (local.get 0)) (get_local $size1))
		;; Copy the second string into new string
		(call $__mem_n_copy
			;; string address + offset of the first string
			(i32.add (get_local $new_ptr) (get_local $size1))
			(i32.wrap_i64 (local.get 1))
			(get_local $size2)
		)

		;; size << 32 | ptr
		(set_local $new_string (i64.shl (i64.extend_i32_u (get_local $str_size)) (i64.const 32)))
		(set_local $new_string (i64.xor (i64.extend_i32_u (get_local $new_ptr)) (get_local $new_string)))

		(get_local $new_string)
	)
	(export "__string_concat2" (func $__string_concat2))

	;; Stack operations reserve 128-159 bytes (32 bytes)
	;; 128-131 stores the current stack ptr
	(func $__get_stack_ptr (result i32)
		(i32.load (i32.const 128))
	)
	(export "__get_stack_ptr" (func $__get_stack_ptr))

	(func $__set_stack_ptr (param i32)
		(i32.store (i32.const 128) (local.get 0))
	)

	;; Reserve N bytes by adding N to stack pointer and returning the prev ptr
	(func $__reserve_stack_bytes (param i32) (result i32)
		(local $__ptr i32)
		(set_local $__ptr (call $__get_stack_ptr))
		;; TODO: Stack overflow error
		(call $__set_stack_ptr (i32.add (get_local $__ptr) (local.get 0)))
		(get_local $__ptr)
	)
	(export "__reserve_stack_bytes" (func $__reserve_stack_bytes))

	;; Release N bytes by removing N from stack pointer
	(func $__release_stack_bytes (param i32)
		;; TODO: Stack underflow error
		(call $__set_stack_ptr (i32.sub (call $__get_stack_ptr) (local.get 0)))
	)
	(export "__release_stack_bytes" (func $__release_stack_bytes))
)
