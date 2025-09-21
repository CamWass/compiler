#!/bin/bash

# Displays the control flow generated graph by the google closure compiler.

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 input_file.js output_file.svg"
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

INPUT="$1"
OUTPUT="$2"

node "$SCRIPT_DIR/closure_cfg.js" "$INPUT" | dot -Tsvg -o "$OUTPUT" && open "$OUTPUT"