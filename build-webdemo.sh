#!/bin/bash
cabal build --with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg web-demo
cp dist-newstyle/build/javascript-ghcjs/ghc-9.10.0.20240413/sequent-calculus-0.1.0.0/x/web-demo/build/web-demo/web-demo.jsexe/all.js web-app
