#!/bin/bash
PACK_FILES="app/  examples/ sequent-calculus.cabal src/ test/ web-app/ build-webdemo.sh cabal.project fourmolu.yaml "
OUT_FILE="web-demo.zip"

zip -r $OUT_FILE $PACK_FILES 
