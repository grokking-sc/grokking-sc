BUILD_SCRIPT = ./build-webdemo.sh
PACK_SCRIPT = ./pack.sh
JS_BUILD = ./examples/examples_to_js.sh

.PHONY: doc pack

all: build doc

build-examples: 
	$(JS_BUILD)

build-web: 
	$(BUILD_SCRIPT) 

build-exe:
	cabal build sequent-calculus

doc: 
	fourmolu -i src/*
	cabal haddock

pack: build-web
	$(PACK_SCRIPT)

run: 
	cabal run sequent-calculus $(filepath)
