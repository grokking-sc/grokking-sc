BUILD_SCRIPT = ./build-webdemo.sh
PACK_SCRIPT = ./pack.sh
JS_BUILD = ./examples/examples_to_js.sh

.PHONY: doc pack format

all: build-exe build-web doc

build-examples: 
	$(JS_BUILD)

build-web: 
	$(BUILD_SCRIPT) 

build-exe:
	cabal build exe:sequent-calculus

doc: 
	fourmolu -i src/*
	cabal haddock

run: 
	cabal run sequent-calculus $(filepath)

format:
	fourmolu --mode inplace $$(git ls-files '*.hs')
