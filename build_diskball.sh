#!/bin/bash
ARTIFACT_DIR="../artifact"
DISK_IN="disk.qcow"
README_IN="README.md"
DISK_OUT="disk.tar.gz"
OUT_DIR="../zenodo_files"

tar -cvzf "$OUT_DIR/$DISK_OUT" "$ARTIFACT_DIR/$README_IN" "$ARTIFACT_DIR/$DISK_IN"
