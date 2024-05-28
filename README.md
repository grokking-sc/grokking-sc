# Sequent Calculus

This is an artifact for the functional perl "Grokking the Sequent Calculus" implementing the introduced programming languages.


## Requirements

For both the binary and the web demo cabal>=3.0 is required. 
If only using the binary, additinally, GHC>=9.4 is also required. 

### Web Demo 

The web demo uses the GHC-JS cross compiler  
This is still a bit experimental, but you can use the instructions here: https://www.haskell.org/ghcup/guide/#cross-support
Be sure to first install an emscripten toolchain.

Once you have verified that you can run the crosscompiler, for example by checking its version:

```console
> javascript-unknown-ghcjs-ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.10.0.20240413

```
## Buidling

In order to build the binary, simply run `cabal build sequent calculus` or using the Makefile: `make build-exe`

### Web Demo 
You can use the `build-webdemo.sh` script or the make target `make build-web` to build:
    
```console
> ./build-webdemo.sh
Building executable 'web-demo' for sequent-calculus-0.1.0.0..
[1 of 1] Compiling Main             ( web-app/Main.hs, /Users/david/GitRepos/introduction-to-sequent-calculus/code/dist-newstyle/build/javascript-ghcjs/ghc-9.6.2/sequent-calculus-0.1.0.0/x/web-demo/build/web-demo/web-demo-tmp/Main.o )
[2 of 2] Linking /Users/david/GitRepos/introduction-to-sequent-calculus/code/dist-newstyle/build/javascript-ghcjs/ghc-9.6.2/sequent-calculus-0.1.0.0/x/web-demo/build/web-demo/web-demo.jsexe
```

If this has been successfully built, the `all.js` script in the `web-demo` directory will be updated to contain the current version.

## Running

In order to run the web-demo, open the `web-app/index.html` file in a browser. 
This will then show a web interface, that given an input in the `Fun` language, compiles it to `Core`, focuses and simplifies it and evaluates the `main` function if it is available, giving the results of every step.


In order to run the compiled binary either use `cabal run sequent-calculus FILENAME` or the build target `make run -filepath=FILENAME` where `FILENAME` is the path to the source file to be run.
The file will then be parsed, type checked, compiled, focused and evaluated, outputting the resulting `Core` program, before and after focusing, along with the trace of evaluating the `main` function. 

## Packing 

All the source code found in this directory can be packed into a zip folder using the `pack.sh` script or the `make pack` target.
This will also include the compiled `all.js` script, created by the `build-webdemo.sh`. When that build script is run a second time, the `all.js` file will be overwriten by the new version.
