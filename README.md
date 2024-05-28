# Functional Pearl: Grokking the Sequent Calculus

This is the artifact for the functional pearl "Grokking the Sequent Calculus".
The paper introduces sequent-calculus-based intermediate languages by compiling the functional surface language `Fun` to the sequent-calculus-based intermediate language `Core`.
This artifact implements a parser and typechecker for the surface language `Fun`, the compilation of `Fun` to `Core`, the focusing algorithm described in the paper and a simplifier and evaluator for `Core`.
The artifact can be used in two different ways: There is a small binary with a command-line interface which allows to compile and run a program on the command line, and there is a web-based demo providing a website which allows to run small examples without having to install anything on your device.

## Directory structure

```text
├── app                     CLI application
├── examples                Examples of `Fun` programs
├── src
│   ├── Compiler.hs         Compilation from Fun to Core
│   ├── Core
│   │   ├── Eval.hs         Substitution-based evaluation
│   │   ├── Focusing.hs     Implementation of the focusing algorithm
│   │   ├── Pretty.hs       Prettyprinting of Core programs
│   │   ├── Simplify.hs     Term simplification
│   │   ├── Substitution.hs Capture-avoiding Substitution
│   │   └── Syntax.hs       Syntax of the intermediate language Core
│   └── Fun
│       ├── Parser.hs       Parser for Fun using parser combinators
│       ├── Syntax.hs       Syntax of Fun
│       └── Types.hs        Hindley-Milner style type inference for Fun
├── test                    Testsuite
└── web-app                 Web-demo application

```

## Requirements

For both the binary and the web demo cabal>=3.0 is required.
If only using the binary, additionally, GHC>=9.4 is also required.

### Web Demo

The web demo uses the GHC-JS cross compiler.
This is still a bit experimental, but you can use the instructions here: https://www.haskell.org/ghcup/guide/#cross-support
Be sure to first install an emscripten toolchain.

Verify that you can run the cross compiler, for example by checking its version:

```console
> javascript-unknown-ghcjs-ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.10.0.20240413

```
## Building

In order to build the binary, simply run `cabal build sequent calculus` or use the Makefile: `make build-exe`

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

In order to run the web demo, open the `web-app/index.html` file in a browser.
This will show a web interface that compiles a given input in the `Fun` language to `Core`, focuses and simplifies it and evaluates the `main` function if it is available, giving the results of every step.

In order to run the compiled binary, either use `cabal run sequent-calculus FILENAME` or the build target `make run -filepath=FILENAME` where `FILENAME` is the path to the source file to be run.
The file will then be parsed, typechecked, compiled, focused and evaluated, outputting the resulting `Core` program, before and after focusing, along with the trace of evaluating the `main` function.

