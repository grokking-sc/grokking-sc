# Functional Pearl: Grokking the Sequent Calculus

This is the artifact for the functional pearl "Grokking the Sequent Calculus", available as an artifact on [Zenodo](https://zenodo.org/doi/10.5281/zenodo.11491667)
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

In order to build the binary, simply run `cabal build exe:sequent-calculus` or use the Makefile: `make build-exe`.
To clean all built files for a rebuild, use the make target `make clean`. This will remove the `dist-newstyle` directory along with all generated javascript files.

### Web Demo
You can use the `build-webdemo.sh` script or the make target `make build-web` to build:

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

In order to run the compiled binary, either use `cabal run sequent-calculus FILENAME` or the build target `make run filepath=FILENAME` where `FILENAME` is the path to the source file to be run.
The file will then be parsed, typechecked, compiled, focused and evaluated, outputting the resulting `Core` program, before and after focusing, along with the trace of evaluating the `main` function.

```console 
> make run filepath=examples/Lists.sc
cabal run sequent-calculus examples/Lists.sc
Program typechecks!
---------- Result of Compilation --------
def map(f,l;a0) := 〈 μa0. 〈 l | case {Nil ⇒ 〈 Nil | a0 〉,Cons(x,xs;) ⇒ 〈 Cons(f,μa0. map(f,xs;a0);) | a0 〉} 〉 | a0 〉
def map(f,l;a0) := 〈 μa0. 〈 l | case {Nil ⇒ 〈 Nil | a0 〉,Cons(x,xs;) ⇒ 〈 Cons(f,μa0. map(f,xs;a0);) | a0 〉} 〉 | a0 〉
def multFast(x;a0) := 〈 μa. 〈 μa0. 〈 x | case {Nil ⇒ 〈 1 | a0 〉,Cons(y,ys;) ⇒ 〈 μa0. ifz(y;〈 μa0. 〈 0 | a 〉 | a0 〉,〈 μa0. *(y,μa0. multFast(ys;a0);a0) | a0 〉) | a0 〉} 〉 | a 〉 | a0 〉
def mult(x;a0) := 〈 μa0. 〈 x | case {Nil ⇒ 〈 1 | a0 〉,Cons(y,ys;) ⇒ 〈 μa0. *(y,μa0. mult(ys;a0);a0) | a0 〉} 〉 | a0 〉
def foldr(f,st,ls;a0) := 〈 μa0. 〈 ls | case {Nil ⇒ 〈 st | a0 〉,Cons(y,ys;) ⇒ 〈 μa0. foldr(f,μa0. 〈 μa0. 〈 f | ap(y;a0) 〉 | ap(st;a0) 〉,ys;a0) | a0 〉} 〉 | a0 〉
def len(ls;a0) := 〈 μa0. 〈 ls | case {Nil ⇒ 〈 0 | a0 〉,Cons(y,ys;) ⇒ 〈 μa0. +(1,μa0. len(ys;a0);a0) | a0 〉} 〉 | a0 〉
def sum(;a0) := 〈 cocase {ap(x;a0) ⇒ 〈 cocase {ap(y;a0) ⇒ 〈 μa0. +(x,y;a0) | a0 〉} | a0 〉} | a0 〉
def main(;a0) := 〈 μa0. len(Cons(1,Cons(2,Cons(3,Cons(4,Nil;););););a0) | a0 〉
---------- Result of Focusing --------
def map(f,l;a0) := 〈 μa0. 〈 l | case {Nil ⇒ 〈 Nil | a0 〉,Cons(x,xs;) ⇒ 〈 μa0. 〈 μa0. map(f,xs;a0) | ~μx0. 〈 Cons(f,x0;) | a0 〉 〉 | a0 〉} 〉 | a0 〉
def map(f,l;a0) := 〈 μa0. 〈 l | case {Nil ⇒ 〈 Nil | a0 〉,Cons(x,xs;) ⇒ 〈 μa0. 〈 μa0. map(f,xs;a0) | ~μx0. 〈 Cons(f,x0;) | a0 〉 〉 | a0 〉} 〉 | a0 〉
def multFast(x;a0) := 〈 μa. 〈 μa0. 〈 x | case {Nil ⇒ 〈 1 | a0 〉,Cons(y,ys;) ⇒ 〈 μa0. ifz(y;〈 μa0. 〈 0 | a 〉 | a0 〉,〈 μa0. 〈 μa0. multFast(ys;a0) | ~μx0. *(y,x0;a0) 〉 | a0 〉) | a0 〉} 〉 | a 〉 | a0 〉
def mult(x;a0) := 〈 μa0. 〈 x | case {Nil ⇒ 〈 1 | a0 〉,Cons(y,ys;) ⇒ 〈 μa0. 〈 μa0. mult(ys;a0) | ~μx0. *(y,x0;a0) 〉 | a0 〉} 〉 | a0 〉
def foldr(f,st,ls;a0) := 〈 μa0. 〈 ls | case {Nil ⇒ 〈 st | a0 〉,Cons(y,ys;) ⇒ 〈 μa0. 〈 μa0. 〈 μa0. 〈 f | ap(y;a0) 〉 | ap(st;a0) 〉 | ~μx0. foldr(μa0. 〈 μa0. 〈 f | ap(y;a0) 〉 | ap(st;a0) 〉,x0,μa0. 〈 μa0. 〈 f | ap(y;a0) 〉 | ap(st;a0) 〉;a0) 〉 | a0 〉} 〉 | a0 〉
def len(ls;a0) := 〈 μa0. 〈 ls | case {Nil ⇒ 〈 0 | a0 〉,Cons(y,ys;) ⇒ 〈 μa0. 〈 μa0. len(ys;a0) | ~μx0. +(1,x0;a0) 〉 | a0 〉} 〉 | a0 〉
def sum(;a0) := 〈 cocase {ap(x;a0) ⇒ 〈 cocase {ap(y;a0) ⇒ 〈 μa0. +(x,y;a0) | a0 〉} | a0 〉} | a0 〉
def main(;a0) := 〈 μa0. len(Cons(1,Cons(2,Cons(3,Cons(4,Nil;););););a0) | a0 〉
---------- Result of Evaluation --------
0: 〈 μa1. len(Cons(1,Cons(2,Cons(3,Cons(4,Nil;););););a1) | ★ 〉
1: len(Cons(1,Cons(2,Cons(3,Cons(4,Nil;););););★)
2: 〈 μa1. 〈 Cons(1,Cons(2,Cons(3,Cons(4,Nil;);););) | case {Nil ⇒ 〈 0 | a1 〉,Cons(x0,x2;) ⇒ 〈 μa1. 〈 μa1. len(x2;a1) | ~μx1. +(1,x1;a1) 〉 | a1 〉} 〉 | ★ 〉
3: 〈 Cons(1,Cons(2,Cons(3,Cons(4,Nil;);););) | case {Nil ⇒ 〈 0 | ★ 〉,Cons(x0,x1;) ⇒ 〈 μa2. 〈 μa0. len(x1;a0) | ~μx1. +(1,x1;a2) 〉 | ★ 〉} 〉
4: 〈 μa0. 〈 μa0. len(Cons(2,Cons(3,Cons(4,Nil;);););a0) | ~μx2. +(1,x2;a0) 〉 | ★ 〉
5: 〈 μa1. len(Cons(2,Cons(3,Cons(4,Nil;);););a1) | ~μx0. +(1,x0;★) 〉
6: len(Cons(2,Cons(3,Cons(4,Nil;);););~μx0. +(1,x0;★))
7: 〈 μa1. 〈 Cons(2,Cons(3,Cons(4,Nil;););) | case {Nil ⇒ 〈 0 | a1 〉,Cons(x0,x2;) ⇒ 〈 μa1. 〈 μa1. len(x2;a1) | ~μx1. +(1,x1;a1) 〉 | a1 〉} 〉 | ~μx0. +(1,x0;★) 〉
8: 〈 Cons(2,Cons(3,Cons(4,Nil;););) | case {Nil ⇒ 〈 0 | ~μx0. +(1,x0;★) 〉,Cons(x0,x1;) ⇒ 〈 μa2. 〈 μa0. len(x1;a0) | ~μx1. +(1,x1;a2) 〉 | ~μx0. +(1,x0;★) 〉} 〉
9: 〈 μa0. 〈 μa0. len(Cons(3,Cons(4,Nil;););a0) | ~μx2. +(1,x2;a0) 〉 | ~μx2. +(1,x2;★) 〉
10: 〈 μa1. len(Cons(3,Cons(4,Nil;););a1) | ~μx0. +(1,x0;~μx2. +(1,x2;★)) 〉
11: len(Cons(3,Cons(4,Nil;););~μx0. +(1,x0;~μx2. +(1,x2;★)))
12: 〈 μa1. 〈 Cons(3,Cons(4,Nil;);) | case {Nil ⇒ 〈 0 | a1 〉,Cons(x0,x2;) ⇒ 〈 μa1. 〈 μa1. len(x2;a1) | ~μx1. +(1,x1;a1) 〉 | a1 〉} 〉 | ~μx0. +(1,x0;~μx2. +(1,x2;★)) 〉
13: 〈 Cons(3,Cons(4,Nil;);) | case {Nil ⇒ 〈 0 | ~μx0. +(1,x0;~μx2. +(1,x2;★)) 〉,Cons(x0,x1;) ⇒ 〈 μa2. 〈 μa0. len(x1;a0) | ~μx1. +(1,x1;a2) 〉 | ~μx0. +(1,x0;~μx2. +(1,x2;★)) 〉} 〉
14: 〈 μa0. 〈 μa0. len(Cons(4,Nil;);a0) | ~μx2. +(1,x2;a0) 〉 | ~μx2. +(1,x2;~μx2. +(1,x2;★)) 〉
15: 〈 μa1. len(Cons(4,Nil;);a1) | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;★))) 〉
16: len(Cons(4,Nil;);~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;★))))
17: 〈 μa1. 〈 Cons(4,Nil;) | case {Nil ⇒ 〈 0 | a1 〉,Cons(x0,x2;) ⇒ 〈 μa1. 〈 μa1. len(x2;a1) | ~μx1. +(1,x1;a1) 〉 | a1 〉} 〉 | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;★))) 〉
18: 〈 Cons(4,Nil;) | case {Nil ⇒ 〈 0 | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;★))) 〉,Cons(x0,x1;) ⇒ 〈 μa2. 〈 μa0. len(x1;a0) | ~μx1. +(1,x1;a2) 〉 | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;★))) 〉} 〉
19: 〈 μa0. 〈 μa0. len(Nil;a0) | ~μx2. +(1,x2;a0) 〉 | ~μx2. +(1,x2;~μx2. +(1,x2;~μx2. +(1,x2;★))) 〉
20: 〈 μa1. len(Nil;a1) | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;~μx2. +(1,x2;★)))) 〉
21: len(Nil;~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;~μx2. +(1,x2;★)))))
22: 〈 μa1. 〈 Nil | case {Nil ⇒ 〈 0 | a1 〉,Cons(x0,x2;) ⇒ 〈 μa1. 〈 μa1. len(x2;a1) | ~μx1. +(1,x1;a1) 〉 | a1 〉} 〉 | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;~μx2. +(1,x2;★)))) 〉
23: 〈 Nil | case {Nil ⇒ 〈 0 | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;~μx2. +(1,x2;★)))) 〉,Cons(x0,x1;) ⇒ 〈 μa2. 〈 μa0. len(x1;a0) | ~μx1. +(1,x1;a2) 〉 | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;~μx2. +(1,x2;★)))) 〉} 〉
24: 〈 0 | ~μx1. +(1,x1;~μx0. +(1,x0;~μx0. +(1,x0;~μx0. +(1,x0;★)))) 〉
25: +(1,0;~μx2. +(1,x2;~μx0. +(1,x0;~μx0. +(1,x0;★))))
26: 〈 1 | ~μx2. +(1,x2;~μx0. +(1,x0;~μx0. +(1,x0;★))) 〉
27: +(1,1;~μx1. +(1,x1;~μx0. +(1,x0;★)))
28: 〈 2 | ~μx1. +(1,x1;~μx0. +(1,x0;★)) 〉
29: +(1,2;~μx2. +(1,x2;★))
30: 〈 3 | ~μx2. +(1,x2;★) 〉
31: +(1,3;★)
32: 〈 4 | ★ 〉
```

### Web Demo

The Web Demo can be run by opening `web-app/index.html` in any browser, after the `examples.js` and `all.js` files have been build (see above).
This will show a web interface that compiles a given input in the `Fun` language to `Core`, focuses and simplifies it and evaluates the `main` function if it is available, giving the results of every step.
Since the disk does not contain X nor a web-browser, running it from the VM is slightly more involved.

#### Method 1: Serving a HTTP Server 

In order to serve `index.html` using a simple http-server, follow the following steps

1. Start the VM as normal, but add the additional flag `-nic user,hostfwd=tcp:8000-:8000`.
    This will enable host forwarding and allow you to access web demo from the host system.
    For example, if using the ICFP provided `start.sh` script, the VM would usually be started with the `./start.sh` command on its own.
    When enabling host forwarding, this becomes `./start.sh -nic user,hostfwd=tcp:8000-:8000`
2. In the directory containing the artifact code (usually `~/grokking-sc`, run the following command
    ```
    python3 -m http.server --directory web-app/ 8000
    ```
    Please make sure the web-app has been built before and inside the `web-app` directory both `all.js` and `examples.js` are present.
    The command will then host the web-demo locally and can be accessed in a browser from the host system
3. In the host system, start a web browser and visit `127.0.0.1:8000`
    If everything worked, the web-demo should show up fully functional

For some people, there might be issues with the first step, and after starting the VM, no SSH connection can be established to the guest system.
In this case, we recommend the second method

#### Method 2: Mounting the File System in the Host

Instead of serving the web-demo using a http server, the guest system can also directly be mounted on the host system using `libguestfs`.
 
1. Install `libguestfs` on the host system
    * Ubuntu/Debian `sudo apt install libguestfs-tools`
    * Arch: `sudo pacman -S libguestfs`
    * Fedora: `sudo yum install libguesfs-tools`
    * Others; Download and build sources from [libguestfs.org](https://libguestfs.org/)
2. Mount the disk image using `guestmount` 
    Before running the mount command below, please ensure the web-demo was successfully built and both `examples.js` and `all.js` are available on the qemu disk
    ```
    guestmount -a $path_to_disk -m /dev/sda1 $path_to_mount 
    ```
    In this command, replace `$path_to_disk` by the (relative or absolute) path to the `disk.qcow` image and `$path_to_mount` by the directory into which you want to mount the file system
    For example, within a bash shell in the same directory as `disk.qcow`, in order to mount the disk to `/mnt` run the command `guestmount -a disk.qcow -m /dev/sda1 /mnt`
    Please make sure the user running this command has access to the directory in which the disk will be mounted.
3. Open the mounted directory (for example `/mnt`) in the host system.
    You should now see a root folder of a linux installation.
4. Navigate to `/home/artifact/grokking-sc/web-app/` in the mounted disk and open `index.html` in a web browser in the host system.
    Now the web demo should have opened in the browser
5. Once finished, unmount the guest system using `guestunmount $path_to_mount` where `$path_to_mount` is the same directory as above


## Paper Claims

* In the introduction, we use the example of the `mult` function to illustrate compilation to sequent calculus as well as the `label` and `goto` terms. 
    This example can be found in the file `examples/FastMultiplication.sc`. To run this example, use the command `make run filepath=examples/FastMultiplication.sc`. 
    In the output you can then see how it is compiled and ran
* In section 2, we introduce the syntax of both the surface language `Fun` and the sequent calculus language `Core`. 
    Our implementation of `Fun`-syntax can be found in the source code in `src/Fun/Syntax.hs` while the one for Core is defined in `src/Core/Syntax.hs`
    All the introduced compilation rules are implemented in the compiler `src/Compiler.hs`. When running any example (using `make run filepath=file`), the output will show how each example is compiled according to these rules.
    This is found under the line `---------- Result of Compilation --------`
* We have implemented a parser for the `Fun` language, which is ran whenever an example is directly ran or ran from the web demo. This does not influence any output, but it can be found in `src/Fun/Parse.hs`
* All examples found in section 2 of the paper can be found in the example `examples/paper_examples.sc`. Running this with `make run filepath=examples/paper_examples.sc` will show an overview of how all these are compiled and evaluated. Within the file, there are comments labeling each of the examples and where they can be found in the paper. 
* In section 3.2, definition 3.2 we introduce static focusing for the `Core` language. Running any example using `make run filepath=file` will include the results of focusing under the `---------- Result of Focusing --------` line in the output. The implementation of these rules can be found in `src/Core/Focusing.hs`.
* We give the evaluation rules for `Fun` in the paper, in order to compare them to the ones for `Core`. These rules are not implemented in the code, however, as we explicitly want to show evaluation for `Core` as explained in section 3
* Evaluation for `Core` is implemented in `src/Core/Eval.hs`. Additionally, whenever an example is run using `make run filepath=file`, the output of evaluation is output under the line `---------- Result of Evaluation --------`. In order for this to show up, the example needs to have a `main` function defined, otherwise no evaluation will take place. All examples included in the `examples` directory have a defined `main` function, which is the only term that will be evaluated. In order to see the evaluation for any of the other defined terms in these examples, the `main` function has to be adjusted to use this different term. 
  Please note that in order to keep the parser simple, all top-level definitions can only be called using `()` even when they have no defined arguments. For example, in `paper_examples.sc`, we have the definition `sec51` defining the example used in section 5.1. This is included in the `main` function as `sec51()` instead of simply `sec51`. When the brackets are omitted, this is interpreted as a variable and will not successfully compile.
* In addition to direct evaluation, we also include simplification, found in `src/Core/Simplify.hs`. This implements common rules for simplification which are analogous to the evaluation rules. This is only used when running the web demo (under `Simplified` in `Core Representation`), and is skipped when running the binary. The reason for this is simply a better representation of terms, so the evaluation trace can be read more easily. When using the compiled binary instead, the simplification is skipped, and the evaluation trace is shown directly from the compiled terms which will be longer, but contains all the evaluation details. 
* During compliation and evaluation, we need fresh (co-) variables and need to substitute them for other terms. This is implemented for `Core` in `src/Core/Substitution.hs`
* We introduce both typing rules for `Core` (section 4.2) and for `Fun` (section 4.1 and appendix B). In the actual code, only typing for `Fun` is implemented and can be found in `src/Fun/Types.hs` (along with the definition of the types introduced in definition 4.1). Since theorem `4.6` shows the compilation to `Core` preserves types we argue its enough to have typing for `Fun` which already ensures terms in `Core` are well-typed.
* In order to show evaluation contexts are first-class in `Core`, we use the example `2*3*4` in the core language. This example is also included in `examples/paper_examples`. Evaluating this, we can see how the covariables generated during compilation act as continuations for evaluation. 
* The duality between data and codata, explained in section 5.2 can be easily seen in the examples of tuples and lazy tuples. We have defined `swap` for tuples (in `examples/Tuples.sc`) and `swaplazy` for lazy pairs (in `examples/LazyPair.sc`), which demonstrate how both are compiled to analogous `case` and `cocase` terms. For convenience, these are also both included in `examples/paper_examples.sc`, where the compiled terms can be easily compared in the output.
* In section 5.3, we compare `let/cc` and `call/cc` which are not directly implemented in the code. We do however have `let` and `label` implemented, which work similarly, but don't have the exact duality we explain in the section. Two examples of `let` and `label` are included in `examples/paper_examples.sc`, where the compiled terms can be compared and show how `mu` and `mu-tilde` are related.
* The case-of-case-translation shown in section 5.4 can be seen in `examples/paper_examples.sc` in the example `casecase`. When this is evaluated (by including it in `main`) or simplified (in the web-demo), we can see how this translation is automatically done by the evaluation rules of `Core`. Since we have not included booleans in the language, this uses a simple example of lists, but this example easily generalizes to other data types (where booleans can be implemented with a simple algebraic data type with the constructors `True` and `False`).
* For section 5.5, since our implementation does not contain any mutable data structures, we illustrate the difference between CPS and the `Core` language using the example `tltltl` in `examples/paper_examples.sc`. The compilation of this term in `Core` shows how the three destructor calls are chained when using the sequent-calculus syntax.
* Eta-Laws using call-by-name and call-by-value, as described in section 5.6 can be seen in the example `criticalPair` in `examples/paper_examples.sc`. When compiling this term to `Core`, we obtain the term `μa0. 〈 1 | a 〉 | a 〉 | ~μx. 〈 x | a0 〉 〉` (see the output of `make run filepath=examples/paper_examples.sc`). This contains a critical pair of a `mu` and `mu-tilde` abstraction whose evaluation depends on the calling convention used. Since we are using a call-by-value language, the `mu` abstraction is evaluated first. This also means that when using different combinations of cases and cocases in this term, different eta-reductions can be performed. By changing this term and either directly evaluating it in the `main` function or viewing the simplified terms in the web-demo, we can see how data types can be directly eta-reduced while codata types cannot be.
