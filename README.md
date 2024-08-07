# Functional Pearl: Grokking the Sequent Calculus

This is the artifact for the functional pearl "Grokking the Sequent Calculus", available as an artifact on [Zenodo](https://zenodo.org/doi/10.5281/zenodo.11491667).
The paper introduces sequent-calculus-based intermediate languages by compiling the functional surface language `Fun` to the sequent-calculus-based intermediate language `Core`.
This artifact implements a parser and typechecker for the surface language `Fun`, the compilation of `Fun` to `Core`, the focusing algorithm described in the paper and a simplifier and evaluator for `Core`.
The artifact can be used in two different ways: There is a small binary with a command-line interface which allows to compile and run a program on the command line, and there is a web-based demo providing a website which allows to run small examples without having to install anything on your device.


## Directory Structure

```text
├── app                     CLI application
├── examples                Examples of Fun programs
├── src
│   ├── Compiler.hs         Compilation from Fun to Core
│   ├── Core
│   │   ├── Eval.hs         Substitution-based evaluation
│   │   ├── Focusing.hs     Implementation of the focusing algorithm
│   │   ├── Pretty.hs       Prettyprinting of Core programs
│   │   ├── Simplify.hs     Term simplification
│   │   ├── Substitution.hs Capture-avoiding substitution
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

### Installing GHC and Cabal (using GHCUP)

First, ensure GHCUP is correctly installed:

```
> ghcup --version
The GHCup Haskell installer, version 0.1.22.0
```

If GHCUP is not installed on your system, follow the installation instructions [here](https://www.haskell.org/ghcup/install/).
Once GHCUP is successfully installed, you can use it to install GHC 9.4 using the following command:

```
ghcup install ghc 9.4 --set
```

The `--set` flag also adds ghc to the path, so ghc can be run from any directoy.
Next, install cabal the same way:

```
ghcup install cabal --set
```

If both of these commands successfully finish, you can verify your installations as follows:

```
> ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.4.8
> cabal --version
cabal-install version 3.10.2.1
compiled using version 3.10.2.1 of the Cabal library
```

Depending on which version of cabal is installed, the version number in the second command might be different, but as long as it is above 3.0, everything should work as expected.

### Web demo

The web demo uses the GHC-JS cross compiler.
This is still a bit experimental, but you can use the instructions [here](https://www.haskell.org/ghcup/guide/#cross-support).
Be sure to first install an emscripten toolchain.

Verify that you can run the cross compiler, for example by checking its version:

```console
> javascript-unknown-ghcjs-ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.10.0.20240413
```


## Building

To build the binary, simply run `cabal build exe:sequent-calculus` or use the Makefile: `make build-exe`.
To clean all built files for a rebuild, use the make target `make clean`.
This will remove the `dist-newstyle` directory along with all generated javascript files.

### Web demo

You can use the `build-webdemo.sh` script or the make target `make build-web` to build the web demo:

```console
> ./build-webdemo.sh
Building executable 'web-demo' for sequent-calculus-0.1.0.0..
[1 of 1] Compiling Main             ( web-app/Main.hs, /Users/david/GitRepos/introduction-to-sequent-calculus/code/dist-newstyle/build/javascript-ghcjs/ghc-9.6.2/sequent-calculus-0.1.0.0/x/web-demo/build/web-demo/web-demo-tmp/Main.o )
[2 of 2] Linking /Users/david/GitRepos/introduction-to-sequent-calculus/code/dist-newstyle/build/javascript-ghcjs/ghc-9.6.2/sequent-calculus-0.1.0.0/x/web-demo/build/web-demo/web-demo.jsexe
```

If this has been successfully built, the `all.js` script in the `web-app` directory will be updated to contain the current version.
When using the `build-webdemo.sh` script instead of the make target, make sure the `web-app/examples.js` file is present.
If it is not, please run the `examples/examples_to_js.sh` script, or use the `make build-examples` target after running `build-webdemo.sh`.
The target `make build-web` automatically runs this script as well, so `examples.js` should always be present in this case.


## Running

### Binary

To run the compiled binary, either use `cabal run sequent-calculus FILENAME` or the build target `make run filepath=FILENAME` where `FILENAME` is the path to the source file to be run.
The file will then be parsed, typechecked, compiled, focused and evaluated, outputting the resulting `Core` program, before and after focusing, along with the trace of evaluating the `main` function.

```console
make run filepath=examples/Stream.sc
cabal run sequent-calculus examples/Stream.sc
---------- Result of Type Checking --------
repeat(x::Int) :: Stream(Int)
const1 :: Stream(Int)
main :: Stream(Int)
---------- Result of Compilation --------
def repeat(x; a3) := 〈 cocase { hd(; a0) ⇒ 〈 x | a0 〉, tl(; a2) ⇒ 〈 μa1. repeat(x; a1) | a2 〉 } | a3 〉
def const1(; a3) := 〈 cocase { hd(; a0) ⇒ 〈 1 | a0 〉, tl(; a2) ⇒ 〈 μa1. const1(; a1) | a2 〉 } | a3 〉
def main(; a1) := 〈 μa0. repeat(1; a0) | a1 〉
---------- Result of Focusing --------
def repeat(x; a3) := 〈 cocase { hd(; a0) ⇒ 〈 x | a0 〉, tl(; a2) ⇒ 〈 μa1. repeat(x; a1) | a2 〉 } | a3 〉
def const1(; a3) := 〈 cocase { hd(; a0) ⇒ 〈 1 | a0 〉, tl(; a2) ⇒ 〈 μa1. const1(; a1) | a2 〉 } | a3 〉
def main(; a1) := 〈 μa0. repeat(1; a0) | a1 〉
---------- Result of Simlpification --------
def repeat(x; a3) := 〈 cocase { hd(; a0) ⇒ 〈 x | a0 〉, tl(; a2) ⇒ repeat(x; a2) } | a3 〉
def const1(; a3) := 〈 cocase { hd(; a0) ⇒ 〈 1 | a0 〉, tl(; a2) ⇒ const1(; a2) } | a3 〉
def main(; a1) := repeat(1; a1)
---------- Result of Evaluation --------
0: repeat(1; ★)
1: 〈 cocase { hd(; a1) ⇒ 〈 1 | a1 〉, tl(; a0) ⇒ repeat(1; a0) } | ★ 〉
```

### Web demo

The Web demo can be run by opening `web-app/index.html` in any browser, after the `examples.js` and `all.js` files have been built (see above).
This will show a web interface that compiles a given input in the `Fun` language to `Core`, focuses and simplifies it and evaluates the `main` function if it is available, giving the results of every step.
Since the VM disk image does neither contain a graphical environment nor a web-browser, running it from the VM is slightly more involved.

#### Method 1: Serving a HTTP server

To serve `index.html` using a simple HTTP server, follow the following steps

1. Start the VM as normal using the included `start.sh` or `start.bat` scripts (see the [README](./disk_image/README.md) included with the disk image).
    These scripts include port forwaring to the host system for port `8000`, which is not included in the start scripts of the base image, so make sure to use the provided ones.
    This will enable host forwarding and allow you to access the web demo from the host system.
2. In the directory containing the artifact code (usually `~/grokking-sc`), run the following command:
    ```
    python3 -m http.server --directory web-app/ 8000
    ```
    Please make sure the web app has been built before and inside the `web-app` directory both `all.js` and `examples.js` are present (if they are not, run the `make build-web` target).
    The command will then host the web demo locally and can be accessed in a browser from the host system.
3. In the host system, start a web browser and visit `127.0.0.1:8000`.
    If everything worked, the web-demo should show up fully functional.

For some people, there might be issues with the first step, and after starting the VM, no SSH connection can be established to the guest system.
In this case, we recommend the second method.

#### Method 2: Opening `index.html` directly on the host system

By using the included source code, one can simply open the file `web-app/index.html` in a web browser, which will directly open the demo fully functional.
As with the first method, this requires `web-app/examples.js` and `web-app/all.js` to be available for loading in `index.html`.
The zipped source code on Zenodo (see the [Zenodo README](./README_Zenodo.md)) already includes those files, so it should work out of the box.
If they are not present, they have to be built with the `make build-web` target.
However, since the disk image does not contain a web browser, this has to be done on the host system without the use of a VM, and thus all build requirements have to be manually installed.


## Language Grammar

We include a Parser for the `Fun` language (`src/Fun/Parser.hs`).
The language accepted by this parser is defined by the following grammar, where we use `a` to denote a fixed set of identifiers used for labels.

```
// Programs
P ::= Def | Def; P
// Top-level definitions
Def ::= def f(x,..; a,..) := T;
        | def f(x,..) := T;
// Terms
T ::= x                             // Variables
    | n                             // Integer Literals
    | (T)                           // Term in Parentheses
    | T * T | T + T | T - T         // Arithmetic Expressions
    | ifz(T, T, T)                  // If-Zero
    | let x = T in T                // Let-Binding
    | f(T,..; a,..)                 // Top-level call (with label a)
    | f(T,..)                       // Top-level call (without label)
    | K(T,..) | K                   // Constructor Term
    | case T of { PtC,.. }          // Case Term
    | T.D(T,..) | T.D               // Destructor Term
    | cocase { PtD }                // Cocase Term
    | \x => T                       // Lambda Abstraction
    | T T                           // Function Application
    | label a { T }                 // Label Term
    | goto(T; a)                    // Goto Term
PtC ::= K(x,..) => T | K => T       // Constructor Patterns
PtD ::= D(x,..) => T | D => T       // Destructor Patterns
K ::= Nil | Cons | Tup              // Constructors
D ::= hd | tl | fst | snd           // Destructors
```


## Paper Claims

We have added an additional example in `examples/paper_examples.sc` to show the specific claims in the paper.
Each definition in this example has a comment annotating which part of the paper it is referring to.
In the following, we give a summary of all of them and explain what they are doing.

Please note that in order to keep the parser simple, all top-level definitions can only be called using `()` even when they have no defined arguments.
For example, in `paper_examples.sc`, we have the definition `sec51` defining the example used in section 5.1.
This is included in the `main` function as `sec51()` instead of simply `sec51`.
When the brackets are omitted, this is interpreted as a variable and will not successfully compile.

### Example 2.1

```
def ex211 := 2 * 3;
def ex212 := ifz(2, 5, 10);
```

These are the terms we introduce to show compilation of arithmetic expressions as well as their evaluation.
Running the `paper_examples.sc` file gives the following outputs for these examples:

```console
---------- Result of Type Checking --------
ex211 :: Int
ex212 :: Int
---------- Result of Compilation --------
def ex211(; a1) := 〈 μa0. *(2, 3; a0) | a1 〉
def ex212(; a1) := 〈 μa0. ifz(2; 〈 5 | a0 〉, 〈 10 | a0 〉) | a1 〉
---------- Result of Focusing --------
def ex211(; a1) := 〈 μa0. *(2, 3; a0) | a1 〉
def ex212(; a1) := 〈 μa0. ifz(2; 〈 5 | a0 〉, 〈 10 | a0 〉) | a1 〉
---------- Result of Simplification --------
def ex211(; a1) := *(2, 3; a1)
def ex212(; a1) := ifz(2; 〈 5 | a1 〉, 〈 10 | a1 〉)
```

Changing the definition of the `main` function in the example to `def main := ex211();` (also included as a comment) to evaluate the first example then gives the evaluation:

```console
---------- Result of Evaluation --------
0: ex211(; ★)
1: *(2, 3; ★)
2: 〈 6 | ★ 〉
```

Similarly, using `def main := ex212();` gives:

```console
---------- Result of Evaluation --------
0: ex212(; ★)
1: ifz(2; 〈 5 | ★ 〉, 〈 10 | ★ 〉)
2: 〈 10 | ★ 〉
```

These results match the ones included in the paper.

### Example 2.2

```
def ex22 := let x = 2 * 2 in x * x;
```

Similar to the last example, this example is used in the paper to show compilation and evaluation of `let`-bindings.
Running this example gives the output:

```console
---------- Result of Type Checking --------
ex22 :: Int
---------- Result of Compilation --------
def ex22(; a3) := 〈 μa2. 〈 μa0. *(2, 2; a0) | ~μx. 〈 μa1. *(x, x; a1) | a2 〉 〉 | a3 〉
---------- Result of Focusing --------
def ex22(; a3) := 〈 μa2. 〈 μa0. *(2, 2; a0) | ~μx. 〈 μa1. *(x, x; a1) | a2 〉 〉 | a3 〉
---------- Result of Simplification --------
def ex22(; a3) := *(2, 2; ~μx0. *(x0, x0; a3))
```

Then, evaluating this example using `def main := ex22();` gives:

```console
---------- Result of Evaluation --------
0: ex22(; ★)
1: *(2, 2; ~μx1. *(x1, x1; ★))
2: 〈 4 | ~μx1. *(x1, x1; ★) 〉
3: *(4, 4; ★)
4: 〈 16 | ★ 〉
```

### Example 2.3

```
def fac(n) := ifz(n, 1, n * fac(n - 1));
def ex23 := fac(1);
```

This example shows compilation and evaluation of top-level definitions.
The output is:

```console
---------- Result of Type Checking --------
fac(n::Int) :: Int
ex23 :: Int
---------- Result of Compilation --------
def fac(n; a4) := 〈 μa3. ifz(n; 〈 1 | a3 〉, 〈 μa2. *(n, μa1. fac(μa0. -(n, 1; a0); a1); a2) | a3 〉) | a4 〉
def ex23(; a1) := 〈 μa0. fac(1; a0) | a1 〉
---------- Result of Focusing --------
def fac(n; a4) := 〈 μa3. ifz(n; 〈 1 | a3 〉, 〈 μa2. 〈 μa1. 〈 μa0. -(n, 1; a0) | ~μx0. fac(x0; a1) 〉 | ~μx0. *(n, x0; a2) 〉 | a3 〉) | a4 〉
def ex23(; a1) := 〈 μa0. fac(1; a0) | a1 〉
---------- Result of Simplification --------
def fac(n; a4) := ifz(n; 〈 1 | a4 〉, -(n, 1; ~μx1. fac(x1; ~μx1. *(n, x1; a4))))
def ex23(; a1) := fac(1; a1)
```

When evaluating `def main := ex23();`, we get:

```console
---------- Result of Evaluation --------
0: ex23(; ★)
1: fac(1; ★)
2: ifz(1; 〈 1 | ★ 〉, -(1, 1; ~μx0. fac(x0; ~μx0. *(1, x0; ★))))
3: -(1, 1; ~μx0. fac(x0; ~μx0. *(1, x0; ★)))
4: 〈 0 | ~μx0. fac(x0; ~μx0. *(1, x0; ★)) 〉
5: fac(0; ~μx1. *(1, x1; ★))
6: ifz(0; 〈 1 | ~μx1. *(1, x1; ★) 〉, -(0, 1; ~μx0. fac(x0; ~μx0. *(0, x0; ~μx1. *(1, x1; ★)))))
7: 〈 1 | ~μx1. *(1, x1; ★) 〉
8: *(1, 1; ★)
9: 〈 1 | ★ 〉
```

### Section 2.4

```
def sum(x) := case x of { Nil => 0,
                          Cons(y, ys) => y + sum(ys) };
def repeat(x) := cocase { hd => x, tl => repeat(x) };
```

These examples serve as an introduction to data and codata types, before we introduce their representation in the `Core` language.
Compiling and focusing these shows the compilation of data and codata types as introduced in  definition 2.5:

```console
---------- Result of Type Checking --------
sum(x::List(Int)) :: Int
repeat(x::Int) :: Stream(Int)
---------- Result of Compilation --------
def sum(x; a3) := 〈 μa2. 〈 x | case { Nil ⇒ 〈 0 | a2 〉, Cons(y, ys; ) ⇒ 〈 μa1. +(y, μa0. sum(ys; a0); a1) | a2 〉 } 〉 | a3 〉
def repeat(x; a3) := 〈 cocase { hd(; a0) ⇒ 〈 x | a0 〉, tl(; a2) ⇒ 〈 μa1. repeat(x; a1) | a2 〉 } | a3 〉
---------- Result of Focusing --------
def sum(x; a3) := 〈 μa2. 〈 x | case { Nil ⇒ 〈 0 | a2 〉, Cons(y, ys; ) ⇒ 〈 μa1. 〈 μa0. sum(ys; a0) | ~μx0. +(y, x0; a1) 〉 | a2 〉 } 〉 | a3 〉
def repeat(x; a3) := 〈 cocase { hd(; a0) ⇒ 〈 x | a0 〉, tl(; a2) ⇒ 〈 μa1. repeat(x; a1) | a2 〉 } | a3 〉
---------- Result of Simplification --------
def sum(x; a3) := 〈 x | case { Nil ⇒ 〈 0 | a3 〉, Cons(x0, x1; ) ⇒ sum(x1; ~μx1. +(x0, x1; a3)) } 〉
def repeat(x; a3) := 〈 cocase { hd(; a0) ⇒ 〈 x | a0 〉, tl(; a2) ⇒ repeat(x; a2) } | a3 〉
```

These results also demonstrate the duality between data and codata types as explained in section 5.2.
Compiled `cocase` and `case` terms are completely dual in `Core`, since the bound `x` in the `case` is moved to the cut between `x` and the actual `case` expression.

Evaluating this for an example list `[1,1,1]` using `def main := sum(Cons(1,Cons(1,Cons(1,Nil))));` shows the evaluation rules for data and codata types:

```console
---------- Result of Evaluation --------
0: sum(Cons(1, Cons(1, Cons(1, Nil; ); ); ); ★)
1: 〈 Cons(1, Cons(1, Cons(1, Nil; ); ); ) | case { Nil ⇒ 〈 0 | ★ 〉, Cons(x2, x3; ) ⇒ sum(x3; ~μx0. +(x2, x0; ★)) } 〉
2: sum(Cons(1, Cons(1, Nil; ); ); ~μx1. +(1, x1; ★))
3: 〈 Cons(1, Cons(1, Nil; ); ) | case { Nil ⇒ 〈 0 | ~μx1. +(1, x1; ★) 〉, Cons(x2, x3; ) ⇒ sum(x3; ~μx0. +(x2, x0; ~μx1. +(1, x1; ★))) } 〉
4: sum(Cons(1, Nil; ); ~μx1. +(1, x1; ~μx0. +(1, x0; ★)))
5: 〈 Cons(1, Nil; ) | case { Nil ⇒ 〈 0 | ~μx1. +(1, x1; ~μx0. +(1, x0; ★)) 〉, Cons(x2, x3; ) ⇒ sum(x3; ~μx0. +(x2, x0; ~μx1. +(1, x1; ~μx0. +(1, x0; ★)))) } 〉
6: sum(Nil; ~μx1. +(1, x1; ~μx0. +(1, x0; ~μx0. +(1, x0; ★))))
7: 〈 Nil | case { Nil ⇒ 〈 0 | ~μx1. +(1, x1; ~μx0. +(1, x0; ~μx0. +(1, x0; ★))) 〉, Cons(x2, x3; ) ⇒ sum(x3; ~μx0. +(x2, x0; ~μx1. +(1, x1; ~μx0. +(1, x0; ~μx0. +(1, x0; ★))))) } 〉
8: 〈 0 | ~μx0. +(1, x0; ~μx0. +(1, x0; ~μx0. +(1, x0; ★))) 〉
9: +(1, 0; ~μx1. +(1, x1; ~μx1. +(1, x1; ★)))
10: 〈 1 | ~μx1. +(1, x1; ~μx1. +(1, x1; ★)) 〉
11: +(1, 1; ~μx0. +(1, x0; ★))
12: 〈 2 | ~μx0. +(1, x0; ★) 〉
13: +(1, 2; ★)
14: 〈 3 | ★ 〉
```

Alternatively, using `def main := repeat(1);` gives evaluation output:

```console
---------- Result of Evaluation --------
0: repeat(1; ★)
1: 〈 cocase { hd(; a1) ⇒ 〈 1 | a1 〉, tl(; a0) ⇒ repeat(1; a0) } | ★ 〉
```

### Example 2.4

```
def swap(x) := case x of { Tup(y, z) => Tup(z, y) };
```

This example similarly shows evaluation and compilation for data types with the following results (using `def main := swap(Tup(1, 2));`):

```console
---------- Result of Type Checking --------
swap(x::Pair(a1, a2)) :: Pair(a2, a1)
---------- Result of Compilation --------
def swap(x; a1) := 〈 μa0. 〈 x | case { Tup(y, z; ) ⇒ 〈 Tup(z, y; ) | a0 〉 } 〉 | a1 〉
---------- Result of Focusing --------
def swap(x; a1) := 〈 μa0. 〈 x | case { Tup(y, z; ) ⇒ 〈 Tup(z, y; ) | a0 〉 } 〉 | a1 〉
---------- Result of Simplification --------
def swap(x; a1) := 〈 x | case { Tup(x0, x1; ) ⇒ 〈 Tup(x1, x0; ) | a1 〉 } 〉

```
```console
---------- Result of Evaluation --------
0: swap(Tup(1, 2; ); ★)
1: 〈 Tup(1, 2; ) | case { Tup(x2, x3; ) ⇒ 〈 Tup(x3, x2; ) | ★ 〉 } 〉
2: 〈 Tup(2, 1; ) | ★ 〉
```

### Example 2.5

```
def swaplazy(x) := cocase { fst => x.snd, snd => x.fst };
```

This example shows the analogon of example 2.4 for codata types, as well as the difference between (data) pairs and (codata) lazy pairs:

```console
---------- Result of Type Checking --------
swaplazy(x::LPair(a3, a4)) :: LPair(a4, a3)
---------- Result of Compilation --------
def swaplazy(x; a4) := 〈 cocase { fst(; a1) ⇒ 〈 μa0. 〈 x | snd(; a0) 〉 | a1 〉, snd(; a3) ⇒ 〈 μa2. 〈 x | fst(; a2) 〉 | a3 〉 } | a4 〉
---------- Result of Focusing --------
def swaplazy(x; a4) := 〈 cocase { fst(; a1) ⇒ 〈 μa0. 〈 x | snd(; a0) 〉 | a1 〉, snd(; a3) ⇒ 〈 μa2. 〈 x | fst(; a2) 〉 | a3 〉 } | a4 〉
---------- Result of Simplification --------
def swaplazy(x; a4) := 〈 cocase { fst(; a1) ⇒ 〈 x | snd(; a1) 〉, snd(; a3) ⇒ 〈 x | fst(; a3) 〉 } | a4 〉
```

Using `def main := swaplazy(cocase { fst => 1, snd => 2 }).snd;` shows the difference in evaluation:

```console
---------- Result of Evaluation --------
0: swaplazy(cocase { fst(; a0) ⇒ 〈 1 | a0 〉, snd(; a0) ⇒ 〈 2 | a0 〉 }; snd(; ★))
1: 〈 cocase { fst(; a0) ⇒ 〈 cocase { fst(; a0) ⇒ 〈 1 | a0 〉, snd(; a0) ⇒ 〈 2 | a0 〉 } | snd(; a0) 〉, snd(; a0) ⇒ 〈 cocase { fst(; a0) ⇒ 〈 1 | a0 〉, snd(; a0) ⇒ 〈 2 | a0 〉 } | fst(; a0) 〉 } | snd(; ★) 〉
2: 〈 cocase { fst(; a1) ⇒ 〈 1 | a1 〉, snd(; a1) ⇒ 〈 2 | a1 〉 } | fst(; ★) 〉
3: 〈 1 | ★ 〉
```

### Example 2.6

```
def ex26 := (\x => x * x) 2;
```

This example shows the compilation of lambda abstractions and function applications as well as how these two concepts are special cases of codata types (in particular, the codata type `Fun`).
We can see in the compilation and focusing output of this example:

```console
---------- Result of Type Checking --------
ex26 :: Int
---------- Result of Compilation --------
def ex26(; a3) := 〈 μa2. 〈 cocase { ap(x; a1) ⇒ 〈 μa0. *(x, x; a0) | a1 〉 } | ap(2; a2) 〉 | a3 〉
---------- Result of Focusing --------
def ex26(; a3) := 〈 μa2. 〈 cocase { ap(x; a1) ⇒ 〈 μa0. *(x, x; a0) | a1 〉 } | ap(2; a2) 〉 | a3 〉
---------- Result of Simplification --------
def ex26(; a3) := 〈 cocase { ap(x0; a0) ⇒ *(x0, x0; a0) } | ap(2; a3) 〉
```

Evaluating this using `def main := ex26();` shows how the results are the same as directly evaluating the term using the rules of the `Fun` langauge:

```console
---------- Result of Evaluation --------
0: ex26(; ★)
1: 〈 cocase { ap(x1; a1) ⇒ *(x1, x1; a1) } | ap(2; ★) 〉
2: *(2, 2; ★)
3: 〈 4 | ★ 〉
```

### Example 2.7

```
def mult(l) := label a { mult2(l; a) };
def mult2(l; a) := case l of { Nil => 1,
                               Cons(x, xs) => ifz(x, goto(0; a), x * mult2(xs; a))};
```

This example is identical to the one found in `examples/FastMultiplication.sc` (which is also used in the introduction) and shows how `label` and `goto` terms work when compiled to `Core`, as well as how they can be used to add shortcuts to evaluation.
The compilation and focusing output is as follows:

```console
---------- Result of Type Checking --------
mult(l::List(Int)) :: Int
mult2(l::List(Int);a::Int) :: Int
---------- Result of Compilation --------
def mult(l; a1) := 〈 μa. 〈 μa0. mult2(l; a, a0) | a 〉 | a1 〉
def mult2(l; a, a5) := 〈 μa4. 〈 l | case { Nil ⇒ 〈 1 | a4 〉, Cons(x, xs; ) ⇒ 〈 μa3. ifz(x; 〈 μa0. 〈 0 | a 〉 | a3 〉, 〈 μa2. *(x, μa1. mult2(xs; a, a1); a2) | a3 〉) | a4 〉 } 〉 | a5 〉
---------- Result of Focusing --------
def mult(l; a1) := 〈 μa. 〈 μa0. mult2(l; a, a0) | a 〉 | a1 〉
def mult2(l; a, a5) := 〈 μa4. 〈 l | case { Nil ⇒ 〈 1 | a4 〉, Cons(x, xs; ) ⇒ 〈 μa3. ifz(x; 〈 μa0. 〈 0 | a 〉 | a3 〉, 〈 μa2. 〈 μa1. mult2(xs; a, a1) | ~μx0. *(x, x0; a2) 〉 | a3 〉) | a4 〉 } 〉 | a5 〉
---------- Result of Simplification --------
def mult(l; a1) := 〈 μa. mult2(l; a, a) | a1 〉
def mult2(l; a, a5) := 〈 l | case { Nil ⇒ 〈 1 | a5 〉, Cons(x0, x1; ) ⇒ ifz(x0; 〈 μa0. 〈 0 | a 〉 | a5 〉, mult2(x1; a, ~μx1. *(x0, x1; a5))) } 〉
```

Evaluating `mult` with the example list `[2,2,0,3]` using `def main := mult(Cons(2,Cons(2,Cons(0,Cons(3,Nil)))));` shows how evaluation stops once we reach the list element `0`:

```console
---------- Result of Evaluation --------
0: mult(Cons(2, Cons(2, Cons(0, Cons(3, Nil; ); ); ); ); ★)
1: 〈 μa0. mult2(Cons(2, Cons(2, Cons(0, Cons(3, Nil; ); ); ); ); a0, a0) | ★ 〉
2: mult2(Cons(2, Cons(2, Cons(0, Cons(3, Nil; ); ); ); ); ★, ★)
3: 〈 Cons(2, Cons(2, Cons(0, Cons(3, Nil; ); ); ); ) | case { Nil ⇒ 〈 1 | ★ 〉, Cons(x2, x3; ) ⇒ ifz(x2; 〈 μa0. 〈 0 | ★ 〉 | ★ 〉, mult2(x3; ★, ~μx0. *(x2, x0; ★))) } 〉
4: ifz(2; 〈 μa0. 〈 0 | ★ 〉 | ★ 〉, mult2(Cons(2, Cons(0, Cons(3, Nil; ); ); ); ★, ~μx1. *(2, x1; ★)))
5: mult2(Cons(2, Cons(0, Cons(3, Nil; ); ); ); ★, ~μx1. *(2, x1; ★))
6: 〈 Cons(2, Cons(0, Cons(3, Nil; ); ); ) | case { Nil ⇒ 〈 1 | ~μx1. *(2, x1; ★) 〉, Cons(x2, x3; ) ⇒ ifz(x2; 〈 μa0. 〈 0 | ★ 〉 | ~μx1. *(2, x1; ★) 〉, mult2(x3; ★, ~μx0. *(x2, x0; ~μx1. *(2, x1; ★)))) } 〉
7: ifz(2; 〈 μa0. 〈 0 | ★ 〉 | ~μx0. *(2, x0; ★) 〉, mult2(Cons(0, Cons(3, Nil; ); ); ★, ~μx1. *(2, x1; ~μx0. *(2, x0; ★))))
8: mult2(Cons(0, Cons(3, Nil; ); ); ★, ~μx1. *(2, x1; ~μx0. *(2, x0; ★)))
9: 〈 Cons(0, Cons(3, Nil; ); ) | case { Nil ⇒ 〈 1 | ~μx1. *(2, x1; ~μx0. *(2, x0; ★)) 〉, Cons(x2, x3; ) ⇒ ifz(x2; 〈 μa0. 〈 0 | ★ 〉 | ~μx1. *(2, x1; ~μx0. *(2, x0; ★)) 〉, mult2(x3; ★, ~μx0. *(x2, x0; ~μx1. *(2, x1; ~μx0. *(2, x0; ★))))) } 〉
10: ifz(0; 〈 μa0. 〈 0 | ★ 〉 | ~μx0. *(2, x0; ~μx0. *(2, x0; ★)) 〉, mult2(Cons(3, Nil; ); ★, ~μx1. *(0, x1; ~μx0. *(2, x0; ~μx0. *(2, x0; ★)))))
11: 〈 μa0. 〈 0 | ★ 〉 | ~μx0. *(2, x0; ~μx0. *(2, x0; ★)) 〉
12: 〈 0 | ★ 〉
```

### Section 5.1

```
def sec51 := (2 * 3) * 4;
```

This example, included in section 5.1, shows how evaluation contexts are first-class in the `Core` language.
When we compile and focus this, we get the following:

```console
---------- Result of Type Checking --------
sec51 :: Int
---------- Result of Compilation --------
def sec51(; a2) := 〈 μa1. *(μa0. *(2, 3; a0), 4; a1) | a2 〉
---------- Result of Focusing --------
def sec51(; a2) := 〈 μa1. 〈 μa0. *(2, 3; a0) | ~μx0. *(x0, 4; a1) 〉 | a2 〉
---------- Result of Simplification --------
def sec51(; a2) := *(2, 3; ~μx1. *(x1, 4; a2))
```

After compilation we can see that covariables are introduced and act as continuations of the compilation.
This can be seen even more clearly when evaluating `def main := sec51();`:

```console
---------- Result of Evaluation --------
0: sec51(; ★)
1: *(2, 3; ~μx0. *(x0, 4; ★))
2: 〈 6 | ~μx0. *(x0, 4; ★) 〉
3: *(6, 4; ★)
4: 〈 24 | ★ 〉
```

### Section 5.3

This section considers `let/cc` and `call/cc` in the `Core` language.
Since these are not implemented in `Fun`, we instead use the examples of `let` and `label` to demonstrate how let-bindings are dual to control operators:

```
def letex := let x = 2 in x * x;
def labelex := label a { goto(0; a) };
```

Compiling these shows how `let` is translated to a ~μ-abstraction while `label` is translated to a μ-abstraction:

```console
---------- Result of Type Checking --------
letex :: Int
labelex :: Int
---------- Result of Compilation --------
def letex(; a2) := 〈 μa1. 〈 2 | ~μx. 〈 μa0. *(x, x; a0) | a1 〉 〉 | a2 〉
def labelex(; a1) := 〈 μa. 〈 μa0. 〈 0 | a 〉 | a 〉 | a1 〉
---------- Result of Focusing --------
def letex(; a2) := 〈 μa1. 〈 2 | ~μx. 〈 μa0. *(x, x; a0) | a1 〉 〉 | a2 〉
def labelex(; a1) := 〈 μa. 〈 μa0. 〈 0 | a 〉 | a 〉 | a1 〉
---------- Result of Simplification --------
def letex(; a2) := 〈 2 | ~μx0. *(x0, x0; a2) 〉
def labelex(; a1) := 〈 μa. 〈 μa0. 〈 0 | a 〉 | a 〉 | a1 〉
```

Evaluating these two examples using `def main := letex();` and `def main := labelex();`, we get:

```console
---------- Result of Evaluation --------
0: letex(; ★)
1: 〈 2 | ~μx1. *(x1, x1; ★) 〉
2: *(2, 2; ★)
3: 〈 4 | ★ 〉
```
```console
---------- Result of Evaluation --------
0: labelex(; ★)
1: 〈 μa0. 〈 μa2. 〈 0 | a0 〉 | a0 〉 | ★ 〉
2: 〈 μa1. 〈 0 | ★ 〉 | ★ 〉
3: 〈 0 | ★ 〉
```

### Section 5.4

```
def casecase := case (case Nil of { Nil => Nil, Cons(x, xs) => xs}) of {
                   Nil => Nil,
                   Cons(y, ys) => ys };
```

This example shows the case-of-case translation explained in section 5.4.
Since the `Fun` and `Core` languages do not include booleans (as opposed to the example we use in this section), we instead use lists as an example.
The translation is automatically done during compilation:

```console
---------- Result of Type Checking --------
casecase :: List(a15)
---------- Result of Compilation --------
def casecase(; a2) := 〈 μa1. 〈 μa0. 〈 Nil | case { Nil ⇒ 〈 Nil | a0 〉, Cons(x, xs; ) ⇒ 〈 xs | a0 〉 } 〉 | case { Nil ⇒ 〈 Nil | a1 〉, Cons(y, ys; ) ⇒ 〈 ys | a1 〉 } 〉 | a2 〉
---------- Result of Focusing --------
def casecase(; a2) := 〈 μa1. 〈 μa0. 〈 Nil | case { Nil ⇒ 〈 Nil | a0 〉, Cons(x, xs; ) ⇒ 〈 xs | a0 〉 } 〉 | case { Nil ⇒ 〈 Nil | a1 〉, Cons(y, ys; ) ⇒ 〈 ys | a1 〉 } 〉 | a2 〉
---------- Result of Simplification --------
def casecase(; a2) := 〈 Nil | case { Nil ⇒ 〈 Nil | case { Nil ⇒ 〈 Nil | a2 〉, Cons(x0, x1; ) ⇒ 〈 x1 | a2 〉 } 〉, Cons(x0, x1; ) ⇒ 〈 x1 | case { Nil ⇒ 〈 Nil | a2 〉, Cons(x0, x1; ) ⇒ 〈 x1 | a2 〉 } 〉 } 〉
```

From the compilation rules, one can also see how this generalizes to arbitrary data types (and `cocases` of codata types), and evaluating this example (`def main := casecase();`) shows how this translation does not change the result:

```console
---------- Result of Evaluation --------
0: casecase(; ★)
1: 〈 Nil | case { Nil ⇒ 〈 Nil | case { Nil ⇒ 〈 Nil | ★ 〉, Cons(x0, x1; ) ⇒ 〈 x1 | ★ 〉 } 〉, Cons(x0, x2; ) ⇒ 〈 x2 | case { Nil ⇒ 〈 Nil | ★ 〉, Cons(x0, x1; ) ⇒ 〈 x1 | ★ 〉 } 〉 } 〉
2: 〈 Nil | case { Nil ⇒ 〈 Nil | ★ 〉, Cons(x0, x2; ) ⇒ 〈 x2 | ★ 〉 } 〉
3: 〈 Nil | ★ 〉
```

### Section 5.5

As our implementation also does not contain references with `get` and `set` methods, we cannot directly use the example for direct and indirect consumers in section 5.5.
Instead, we use the example of the `tl` destructor of streams:

```
def tltltl := repeat(1).tl.tl.tl;
```

When compiling this to `Core`, we still have the chained destructors as in the surface language, which would be lost in a traditional CPS translation:

```console
---------- Result of Type Checking --------
tltltl :: Stream(Int)
---------- Result of Compilation --------
def tltltl(; a4) := 〈 μa3. 〈 μa2. 〈 μa1. 〈 μa0. repeat(1; a0) | tl(; a1) 〉 | tl(; a2) 〉 | tl(; a3) 〉 | a4 〉
---------- Result of Focusing --------
def tltltl(; a4) := 〈 μa3. 〈 μa2. 〈 μa1. 〈 μa0. repeat(1; a0) | tl(; a1) 〉 | tl(; a2) 〉 | tl(; a3) 〉 | a4 〉
---------- Result of Simplification --------
def tltltl(; a4) := repeat(1; tl(; tl(; tl(; a4))))
```

We can see on how the direct chaining of destructors is preserved (after simplification of the administrative μ-bindings).
When evaluating this example (`def main := tltltl();`), we obtain:

```console
---------- Result of Evaluation --------
0: tltltl(; ★)
1: repeat(1; tl(; tl(; tl(; ★))))
2: 〈 cocase { hd(; a1) ⇒ 〈 1 | a1 〉, tl(; a0) ⇒ repeat(1; a0) } | tl(; tl(; tl(; ★))) 〉
3: repeat(1; tl(; tl(; ★)))
4: 〈 cocase { hd(; a1) ⇒ 〈 1 | a1 〉, tl(; a0) ⇒ repeat(1; a0) } | tl(; tl(; ★)) 〉
5: repeat(1; tl(; ★))
6: 〈 cocase { hd(; a1) ⇒ 〈 1 | a1 〉, tl(; a0) ⇒ repeat(1; a0) } | tl(; ★) 〉
7: repeat(1; ★)
8: 〈 cocase { hd(; a1) ⇒ 〈 1 | a1 〉, tl(; a0) ⇒ repeat(1; a0) } | ★ 〉
```

### Section 5.6

```
def criticalEta1(; b) := let x = \y => goto(\z => 1; b) y in \z => 3;
def criticalEta2(; b) := let x = goto(\z => 1; b) in \z => 3;
```

We use these examples to demonstrate the differences between call-by-name and call-by-value and their connection to the η-laws, as explained in section 5.6.
The `goto` is η-expanded in `criticalEta1` but not in `criticalEta2`.
After compilation, `criticalEta2` contains a critical pair, i.e., a cut between a μ-abstraction and a ~μ-abstraction, but in `criticalEta1` the μ-abstraction for the `goto` is still η-expanded:

```console
---------- Result of Type Checking --------
criticalEta1(b::a23 -> Int) :: a26 -> Int
criticalEta2(b::a29 -> Int) :: a29 -> Int
---------- Result of Compilation --------
def criticalEta1(; b, a6) := 〈 μa5. 〈 cocase { ap(y; a3) ⇒ 〈 μa2. 〈 μa1. 〈 cocase { ap(z; a0) ⇒ 〈 1 | a0 〉 } | b 〉 | ap(y; a2) 〉 | a3 〉 } | ~μx. 〈 cocase { ap(z; a4) ⇒ 〈 3 | a4 〉 } | a5 〉 〉 | a6 〉
def criticalEta2(; b, a4) := 〈 μa3. 〈 μa1. 〈 cocase { ap(z; a0) ⇒ 〈 1 | a0 〉 } | b 〉 | ~μx. 〈 cocase { ap(z; a2) ⇒ 〈 3 | a2 〉 } | a3 〉 〉 | a4 〉
---------- Result of Focusing --------
def criticalEta1(; b, a6) := 〈 μa5. 〈 cocase { ap(y; a3) ⇒ 〈 μa2. 〈 μa1. 〈 cocase { ap(z; a0) ⇒ 〈 1 | a0 〉 } | b 〉 | ap(y; a2) 〉 | a3 〉 } | ~μx. 〈 cocase { ap(z; a4) ⇒ 〈 3 | a4 〉 } | a5 〉 〉 | a6 〉
def criticalEta2(; b, a4) := 〈 μa3. 〈 μa1. 〈 cocase { ap(z; a0) ⇒ 〈 1 | a0 〉 } | b 〉 | ~μx. 〈 cocase { ap(z; a2) ⇒ 〈 3 | a2 〉 } | a3 〉 〉 | a4 〉
---------- Result of Simplification --------
def criticalEta1(; b, a6) := 〈 cocase { ap(x0; a0) ⇒ 〈 μa1. 〈 cocase { ap(x0; a1) ⇒ 〈 1 | a1 〉 } | b 〉 | ap(x0; a0) 〉 } | ~μx0. 〈 cocase { ap(x0; a1) ⇒ 〈 3 | a1 〉 } | a6 〉 〉
def criticalEta2(; b, a4) := 〈 μa0. 〈 cocase { ap(x0; a0) ⇒ 〈 1 | a0 〉 } | b 〉 | ~μx0. 〈 cocase { ap(x0; a1) ⇒ 〈 3 | a1 〉 } | a4 〉 〉
```

Evaluating `def main := label b { criticalEta2(; b) };` demonstrates how in a call-by-value language, μ-abstractions are evaluated first:

```console
---------- Result of Evaluation --------
0: 〈 μa0. criticalEta2(; a0, a0) | ★ 〉
1: criticalEta2(; ★, ★)
2: 〈 μa0. 〈 cocase { ap(x0; a0) ⇒ 〈 1 | a0 〉 } | ★ 〉 | ~μx0. 〈 cocase { ap(x0; a1) ⇒ 〈 3 | a1 〉 } | ★ 〉 〉
3: 〈 cocase { ap(x0; a1) ⇒ 〈 1 | a1 〉 } | ★ 〉
```

Evaluating `def main := label b { criticalEta1(; b) };` gives a different result instead, since here the ~μ-abstraction is evaluated:

```console
---------- Result of Evaluation --------
0: 〈 μa0. criticalEta1(; a0, a0) | ★ 〉
1: criticalEta1(; ★, ★)
2: 〈 cocase { ap(x1; a1) ⇒ 〈 μa0. 〈 cocase { ap(x0; a0) ⇒ 〈 1 | a0 〉 } | ★ 〉 | ap(x1; a1) 〉 } | ~μx0. 〈 cocase { ap(x0; a1) ⇒ 〈 3 | a1 〉 } | ★ 〉 〉
3: 〈 cocase { ap(x1; a0) ⇒ 〈 3 | a0 〉 } | ★ 〉
```

The η-law for the codata type of functions hence does not hold in general with call-by-value.
If we instead used call-by-name, the ~μ-abstraction would be evaluated first for `crtiticalEta1`, too, making the η-law hold.

### Code

To see how the definitions and formulas in the paper are implemented in Haskell, we will give a quick overview of what can be found where

* `Core` syntax (introduced in section 2) is implemented in `src/Core/Syntax.hs`.
* `Fun` syntax (introduced in section 2) is implemented in `src/Fun/Syntax.hs`.
* A parser for `Fun` is implemented in `src/Fun/Parser.hs`.
* Typing rules for `Fun` (introduced in section 4.1 and appendix B) are implemented in `src/Fun/Types.hs`.
    The type inference algorithm follows the standard Hindley-Milner approach, but for simplicity does not implement let-generalization.
    Since types are preserved under translation, by theorem 4.6, we have not provided type inference for `Core`.
    We can be sure that if a term can be typed in `Fun`, it can also be typed in `Core` with the same type.
* Compilation rules (introduced in section 2) are implemented in `src/Compiler/hs`.
    The output of compilation can be seen under the `---------- Result of Compilation --------` heading when running the binary, and under `Core Representation > Compiled` in the web demo.
* Static Focusing (introduced in section 3.2, definition 3.2) is implemented in `src/Core/Focusing.hs`.
    The output of focusing can be seen under the `---------- Result of Focusing --------` heading when running the binary, and under `Core Representation > Focused` in the web demo.
* Simplification is implemented in `src/Core/Simplify.hs`.
    The results of simplification are included when running the binary under the `---------- Result of Evaluation --------` heding, and in the web demo under `Core Representation > Simplified`.
    We only simplify administrative redexes introduced during compilation and focusing.
* Evaluation rules for Core (introduced in section 2) are implemented in `src/Core/Eval.hs`.
    The output of evaluation can be seen under the `---------- Result of Evaluation --------` heading when running the binary, and under `Evaluation` in the web demo.
    Evaluation is only run for the `main` definition in a program, so any program without such a function will not have any evaluation results (neither in the binary nor the web demo).
    Furthermore, evaluation is only implemented for `Core` and not for `Fun`, since we treat `Fun` as a surface language compiled to `Core` only after which it is evaluated.
* Substitution during evaluation is implemented in `src/Core/Substitution.hs`.
    For simplicity, we α-rename all bindings using fresh names with respect to the performed substitution.
* Fresh covariables and variables that are generated during compilation, focusing (denoted by `fresh` section 2 and 3) and substitution are implemented in `src/Core/Substitution.hs`.
    This uses a type class `FreeV` to make sure no shadowing can occur when generating a variable.
