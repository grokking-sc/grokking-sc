#!/bin/bash

README_IN="README.md"
START_SH_IN="start.sh"
START_BAT_IN="start.bat"
DISK_IN="disk.qcow"
DEBUGGING_IN="Debugging.md"

OUT_FILE="disk.tar.gz"

tar -cvzf "$OUT_FILE" "$README_IN" "$DEBUGGING_IN" "$START_SH_IN" "$START_BAT_IN" "$DISK_IN"
