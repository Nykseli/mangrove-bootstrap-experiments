#!/bin/bash

set -e

ROOT_PWD=$(dirname $0)
cd $ROOT_PWD

MACHINE=$(uname -m)
if [[ "$MACHINE" -ne "riscv64" ]]; then
	echo This script only works in riscv64 machines
	exit 1
fi

if [[ -z "$1" ]]; then
	echo "Give riscv64 main asm file as an argument"
	exit 1
fi

INPUT_FILE=$1
OUTPUT_FILE="${INPUT_FILE//\.grove/\.s}"
# cargo run -- -t riscv "$INPUT_FILE" > "$OUTPUT_FILE"
./target/release/mangrove-rs -t riscv "$INPUT_FILE" > "$OUTPUT_FILE"
