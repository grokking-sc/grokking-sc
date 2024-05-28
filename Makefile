BUILD_SCRIPT = ./build-webdemo.sh


.PHONY: build doc 

all: build doc

build: 
	$(BUILD_SCRIPT) 

doc: 
	cabal haddock
