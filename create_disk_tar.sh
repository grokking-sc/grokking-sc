#!/bin/bash
ARTIFACT_DIR="../artifact"
DISK_IN="disk.qcow"
README_IN="README_artifact.md"

README_OUT="README.md"
DISK_OUT="disk.tar.gz"
OUT_DIR="../zenodo_files"

rm -f "$ARTIFACT_DIR/$README_OUT"
cp "./$README_IN" "$ARTIFACT_DIR/$README_OUT"
tar -cvzf "$OUT_DIR/$DISK_OUT" "$ARTIFACT_DIR/$README_OUT" "$ARTIFACT_DIR/$DISK_IN"
