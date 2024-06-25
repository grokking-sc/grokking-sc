#!/bin/bash
FILES=$(find examples/ -type f -name \*.sc)
for file in $FILES
do
    echo Testing $file
    cabal run exe:sequent-calculus -- $file > /dev/null || exit 1
done
