#!/bin/bash

set -e

###
# Simple compiler test that make sure that the programs are priting the right
# output. These test just do a very simple sanity tests to make sure that
# nothing is horribly broken.
##

# Terminal colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Make sure we're always in the root of the project
ROOT_PWD=$(dirname $0)
cd $ROOT_PWD
cd ..

# Always make we have the latest version of the software
cargo build --release

SYNTAX_ROOT="tests/cases/syntax/"
GROVE_FILES=($(find $SYNTAX_ROOT -name "*.grove" -not -name "fixme_*"))

check_output() {
	RUN_OUT="$1"
	OUTPUT_LINES="$2"

	if [[ "$RUN_OUT" == "$OUTPUT_LINES" ]]; then
		printf "${GREEN}Success${NC}\n"
	else
		printf "${RED}Failed${NC}\n"
		echo "---- Expected output ----"
		echo "$OUTPUT_LINES"
		echo "---- Capured output ----"
		echo "$RUN_OUT"
		exit 1
	fi
}

test_wasm_file() {
	FILE=$1
	WAT_PATH=$(echo $FILE | sed 's/\.grove/\.wat/')
	WAT_OPATH=$(echo $FILE | sed 's/\.grove/_opt\.wat/')
	echo Compiling $FILE

	FIRST_LINE=$(awk 'NR==1{print $0}' $FILE)
	if [[ "$FIRST_LINE" != "// Output:" ]]; then
		echo First line is not starting with output, skipping.
		return
	fi

	# Capture lines after output until a empty line occurs
	OUTPUT_LINES=$(awk 'NR==2,/^$/' $FILE | sed 's/\/\/ //')

	printf "testing output... "
	./target/release/mangrove-rs $FILE > $WAT_PATH
	# ./target/release/mangrove-rs --optimise $FILE > $WAT_OPATH
	RUN_OUT=$(wasmtime --preload internals=asm/wasm/internals.wat \
			 --preload main=$WAT_PATH \
			 asm/wasm/run.wat)
	check_output "$RUN_OUT" "$OUTPUT_LINES"

	# printf "testing optimised output... "
	# RUN_OUT=$(wasmtime --preload internals=asm/wasm/internals.wat \
	# 		 --preload main=$WAT_OPATH \
	# 		 asm/wasm/run.wat)
	# check_output "$RUN_OUT" "$OUTPUT_LINES"
}

test_wasm() {
	WASM_PATH=$(which wasmtime || printf "")
	if [ -z "$WASM_PATH" ]; then
		echo "Couldn't find wasmtime from PATH"
		exit 1
	fi

	if [[ ! -z "$1" ]]; then
		test_wasm_file $1
		exit 0
	fi

	for FILE in "${GROVE_FILES[@]}"
	do
		test_wasm_file $FILE
	done
}

test_riscv_file() {
	FILE=$1
	BIN_PATH="${FILE//\.grove/\.bin}"
	FIRST_LINE=$(awk 'NR==1{print $0}' $FILE)

	echo testing file $FILE

	if [[ "$FIRST_LINE" != "// Output:" ]]; then
		echo First line is not starting with output, skipping.
		return
	fi

	# Capture lines after output until a empty line occurs
	OUTPUT_LINES=$(awk 'NR==2,/^$/' $FILE | sed 's/\/\/ //')

	printf "testing output... "
	if [[ ! -f $BIN_PATH ]]; then
		printf "${RED}Failed${NC} (file not compiled)\n"
	else
		RUN_OUT=$($BIN_PATH || printf "")
		check_output "$RUN_OUT" "$OUTPUT_LINES"
	fi
}

test_riscv() {
	if [[ ! -z "$1" ]]; then
		./run_riscv.sh $1
		make -f Makefile.riscv tests || true
		test_riscv_file $1
		exit 0
	else
		for FILE in "${GROVE_FILES[@]}"; do
			S_PATH="${FILE//\.grove/\.s}"
			echo compiling file $FILE
			./run_riscv.sh $FILE &> /dev/null || rm $S_PATH
		done
		make -f Makefile.riscv tests || true
		for FILE in "${GROVE_FILES[@]}"; do
			test_riscv_file $FILE
		done
	fi
}


ARCH="wasm"
POSITIONAL_ARGS=()

while [[ $# -gt 0 ]]; do
	case $1 in
		-a|--arch)
			ARCH="$2"
			shift
			shift
			;;
		-*|--*)
			echo "Unknown option $1"
			exit 1
			;;
		*)
			POSITIONAL_ARGS+=("$1")
			shift
			;;
	esac
done

set -- "${POSITIONAL_ARGS[@]}"

if [[ "$ARCH" == "wasm" ]]; then
	test_wasm $1
elif [[ "$ARCH" == "riscv" ]]; then
	test_riscv $1
else
	echo unknown arch "\"$ARCH\""
fi
