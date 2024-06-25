#!/bin/bash
INCLUDED_CONFIGS="cabal.project sequent-calculus.cabal fourmolu.yaml Makefile"
INCLUDED_SOURCES="app src test web-app examples"
INCLUDED_OTHERS="build-webdemo.sh LICENSE README.md"

OUT_DIR="../zenodo_files"
OUT_FILE="grokking-sc-AEC2.tar.gz"

make build-web
tar -czf $OUT_DIR/$OUT_FILE $INCLUDED_SOURCES $INCLUDED_CONFIGS $INCLUDED_OTHERS
