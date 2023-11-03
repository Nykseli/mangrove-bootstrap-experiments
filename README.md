# Experimental Bootstrap Compiler

Experimenting using WASM as a boostrapping step.

Please note that this is highly experimental and costantly changing.


## How to run

Add wasmtime to PATH (optional if you already have it in your path):
```sh
mkdir -p wasm/wasmtime
git clone https://github.com/bytecodealliance/wasmtime wasm/wasmtime
cd wasm/wasmtime
git checkout 36ca00c0a830ae6394cf188f9c02d1d5076ceb2d
git submodule update --init --recursive
cargo build --release
cd ../..
export PATH=`pwd`/wasm/wasmtime/target/release:$PATH
```

Running a simple program:
```sh
cat >main.grove <<EOL
function main() -> none {
	__print_str("Hello world\\n")
}
EOL
cargo run main.grove > main.wat
wasmtime --preload internals=asm/wasm/internals.wat --preload main=main.wat asm/wasm/run.wat
```
