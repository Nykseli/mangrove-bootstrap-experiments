#!/bin/bash

set -e

cargo run boot/compile_x86_64.grove > main.wat
wasmtime --preload internals=asm/wasm/internals.wat --preload main=main.wat asm/wasm/run.wat
