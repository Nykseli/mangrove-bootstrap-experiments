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

WASM_PATH=$(which wasmtime || printf "")
if [ -z "$WASM_PATH" ]; then
	echo "Couldn't find wasmtime from PATH"
	exit 1
fi

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

test_file() {
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

if [[ ! -z "$1" ]]; then
	test_file $1
	exit 0
fi


SYNTAX_ROOT="tests/cases/syntax/"
GROVE_FILES=($(find $SYNTAX_ROOT -name "*.grove" -not -name "fixme_*"))

for FILE in "${GROVE_FILES[@]}"
do
	test_file $FILE
done
