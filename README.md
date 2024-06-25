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

We have added an additional example `examples/paper_examples.sc` to show the specific claims in the paper. 
Each definition in this example has a comment annotating which part of the paper it is referring to.
In the following we give a summary of all of them and explain what they are doing.

Please note that in order to keep the parser simple, all top-level definitions can only be called using `()` even when they have no defined arguments. For example, in `paper_examples.sc`, we have the definition `sec51` defining the example used in section 5.1. This is included in the `main` function as `sec51()` instead of simply `sec51`. When the brackets are omitted, this is interpreted as a variable and will not successfully compile.

### Example 2.1
```
def ex211 := 2*3;
def ex212 := ifz(2,5,10);
```
These are the terms we introduce in order to show compilation of arithmetic expressions as well as their evaluation. 
Running the `paper_examples.sc` file gives the following outputs for these examples:
```
---------- Result of Compilation --------
def ex211(;a0) := 〈 μa0. *(2,3;a0) | a0 〉
def ex212(;a0) := 〈 μa0. ifz(2;〈 5 | a0 〉,〈 10 | a0 〉) | a0 〉
---------- Result of Focusing --------
def ex211(;a0) := 〈 μa0. *(2,3;a0) | a0 〉
def ex212(;a0) := 〈 μa0. ifz(2;〈 5 | a0 〉,〈 10 | a0 〉) | a0 〉
```
Changing the definition of the `main` function in the example to `def main := ex211();` (also included as a comment) to evaluate the first example then gives the evaluation:
```
---------- Result of Evaluation -------- 
0: 〈 μa1. ex211(;a1) | ★ 〉
1: ex211(;★)
2: 〈 μa1. *(2,3;a1) | ★ 〉
3: *(2,3;★)
4: 〈 6 | ★ 〉
```
Similarly, using `def main := ex212();` gives 
```
---------- Result of Evaluation --------
0: 〈 μa1. ex212(;a1) | ★ 〉
1: ex212(;★)
2: 〈 μa1. ifz(2;〈 5 | a1 〉,〈 10 | a1 〉) | ★ 〉
3: ifz(2;〈 5 | ★ 〉,〈 10 | ★ 〉)
4: 〈 10 | ★ 〉
```
These results match the ones included in the paper

### Example 2.2
```
def ex22 := let x = 2*2 in x*x;
```
Similar to the last example, this example is used in the paper to show compilation and evaluation of `let`-bindings. 
Running this example gives the output 
```
---------- Result of Compilation --------
def ex22(;a0) := 〈 μa0. 〈 μa0. *(2,2;a0) | ~μx. 〈 μa0. *(x,x;a0) | a0 〉 〉 | a0 〉
---------- Result of Focusing --------
def ex22(;a0) := 〈 μa0. 〈 μa0. *(2,2;a0) | ~μx. 〈 μa0. *(x,x;a0) | a0 〉 〉 | a0 〉
```
Then, evaluating this example using `def main := ex22();` gives 
```
---------- Result of Evaluation --------
0: 〈 μa1. ex22(;a1) | ★ 〉
1: ex22(;★)
2: 〈 μa1. 〈 μa1. *(2,2;a1) | ~μx1. 〈 μa1. *(x1,x1;a1) | a1 〉 〉 | ★ 〉
3: 〈 μa0. *(2,2;a0) | ~μx0. 〈 μa2. *(x0,x0;a2) | ★ 〉 〉
4: *(2,2;~μx0. 〈 μa2. *(x0,x0;a2) | ★ 〉)
5: 〈 4 | ~μx0. 〈 μa2. *(x0,x0;a2) | ★ 〉 〉
6: 〈 μa0. *(4,4;a0) | ★ 〉
7: *(4,4;★)
8: 〈 16 | ★ 〉
``` 

### Example 2.3 
```
def fac(n) := ifz(n,1,n*fac(n-1));
def ex23 := fac(1);
```
This example shows compilation and evaluation of toplevel definitions. 
The outut of these are 
```
---------- Result of Compilation --------
def fac(n;a0) := 〈 μa0. ifz(n;〈 1 | a0 〉,〈 μa0. *(n,μa0. fac(μa0. -(n,1;a0);a0);a0) | a0 〉) | a0 〉
def ex23(;a0) := 〈 μa0. fac(1;a0) | a0 〉
---------- Result of Focusing --------
def fac(n;a0) := 〈 μa0. ifz(n;〈 1 | a0 〉,〈 μa0. 〈 μa0. 〈 μa0. -(n,1;a0) | ~μx0. fac(x0;a0) 〉 | ~μx0. *(n,x0;a0) 〉 | a0 〉) | a0 〉
def ex23(;a0) := 〈 μa0. fac(1;a0) | a0 〉
```
When we evaluate `def main := ex23();`, we then get 
```
---------- Result of Evaluation --------
0: 〈 μa1. ex23(;a1) | ★ 〉
1: ex23(;★)
2: 〈 μa1. fac(1;a1) | ★ 〉
3: fac(1;★)
4: 〈 μa1. ifz(1;〈 1 | a1 〉,〈 μa1. 〈 μa1. 〈 μa1. -(1,1;a1) | ~μx0. fac(x0;a1) 〉 | ~μx0. *(1,x0;a1) 〉 | a1 〉) | ★ 〉
5: ifz(1;〈 1 | ★ 〉,〈 μa0. 〈 μa0. 〈 μa0. -(1,1;a0) | ~μx0. fac(x0;a0) 〉 | ~μx0. *(1,x0;a0) 〉 | ★ 〉)
6: 〈 μa0. 〈 μa0. 〈 μa0. -(1,1;a0) | ~μx0. fac(x0;a0) 〉 | ~μx0. *(1,x0;a0) 〉 | ★ 〉
7: 〈 μa1. 〈 μa1. -(1,1;a1) | ~μx0. fac(x0;a1) 〉 | ~μx1. *(1,x1;★) 〉
8: 〈 μa0. -(1,1;a0) | ~μx1. fac(x1;~μx1. *(1,x1;★)) 〉
9: -(1,1;~μx1. fac(x1;~μx1. *(1,x1;★)))
10: 〈 0 | ~μx1. fac(x1;~μx1. *(1,x1;★)) 〉
11: fac(0;~μx0. *(1,x0;★))
12: 〈 μa1. ifz(0;〈 1 | a1 〉,〈 μa1. 〈 μa1. 〈 μa1. -(0,1;a1) | ~μx0. fac(x0;a1) 〉 | ~μx0. *(0,x0;a1) 〉 | a1 〉) | ~μx0. *(1,x0;★) 〉
13: ifz(0;〈 1 | ~μx0. *(1,x0;★) 〉,〈 μa0. 〈 μa0. 〈 μa0. -(0,1;a0) | ~μx0. fac(x0;a0) 〉 | ~μx0. *(0,x0;a0) 〉 | ~μx0. *(1,x0;★) 〉)
14: 〈 1 | ~μx0. *(1,x0;★) 〉
15: *(1,1;★)
16: 〈 1 | ★ 〉
```

### Section 2.4
```
def sum(x) := case x of { Nil => 0, Cons(y,ys) => y + sum(ys) };
def repeat(x) := cocase { hd => x, tl => repeat(x) };
```
These examples serve as an introduction to data and codata types, before we introduce their representation in the `Core` language. 
Compiling and focusing these shows the compilation of data and codata types as introduced in  definition 2.5
```
---------- Result of Compilation --------
def sum(x;a0) := 〈 μa0. 〈 x | case {Nil ⇒ 〈 0 | a0 〉,Cons(y,ys;) ⇒ 〈 μa0. +(y,μa0. sum(ys;a0);a0) | a0 〉} 〉 | a0 〉
def repeat(x;a0) := 〈 cocase {hd(;a0) ⇒ 〈 x | a0 〉,tl(;a0) ⇒ 〈 μa0. repeat(x;a0) | a0 〉} | a0 〉
```
```
---------- Result of Focusing --------
def sum(x;a0) := 〈 μa0. 〈 x | case {Nil ⇒ 〈 0 | a0 〉,Cons(y,ys;) ⇒ 〈 μa0. 〈 μa0. sum(ys;a0) | ~μx0. +(y,x0;a0) 〉 | a0 〉} 〉 | a0 〉
def repeat(x;a0) := 〈 cocase {hd(;a0) ⇒ 〈 x | a0 〉,tl(;a0) ⇒ 〈 μa0. repeat(x;a0) | a0 〉} | a0 〉
```
These results also demonstrate the duality between data and codata types as explained in section 5.2. 
Compiled cocase and case terms are completely analogous in `Core`, since the bound `x` in the `case` is moved to the cut between `x` and the actual case-expression.

By evaluating this for an example list `[1,1,1]` using `def main := sum(Cons(1,Cons(1,Cons(1,Nil))));` shows the evaluation rules for data and codata types 
```
---------- Result of Evaluation --------
0: 〈 μa1. sum(Cons(1,Cons(1,Cons(1,Nil;);););a1) | ★ 〉
1: sum(Cons(1,Cons(1,Cons(1,Nil;);););★)
2: 〈 μa1. 〈 Cons(1,Cons(1,Cons(1,Nil;););) | case {Nil ⇒ 〈 0 | a1 〉,Cons(x2,x3;) ⇒ 〈 μa1. 〈 μa1. sum(x3;a1) | ~μx1. +(x2,x1;a1) 〉 | a1 〉} 〉 | ★ 〉
3: 〈 Cons(1,Cons(1,Cons(1,Nil;););) | case {Nil ⇒ 〈 0 | ★ 〉,Cons(x0,x1;) ⇒ 〈 μa2. 〈 μa0. sum(x1;a0) | ~μx2. +(x0,x2;a2) 〉 | ★ 〉} 〉
4: 〈 μa0. 〈 μa0. sum(Cons(1,Cons(1,Nil;););a0) | ~μx2. +(1,x2;a0) 〉 | ★ 〉
5: 〈 μa1. sum(Cons(1,Cons(1,Nil;););a1) | ~μx0. +(1,x0;★) 〉
6: sum(Cons(1,Cons(1,Nil;););~μx0. +(1,x0;★))
7: 〈 μa1. 〈 Cons(1,Cons(1,Nil;);) | case {Nil ⇒ 〈 0 | a1 〉,Cons(x2,x3;) ⇒ 〈 μa1. 〈 μa1. sum(x3;a1) | ~μx1. +(x2,x1;a1) 〉 | a1 〉} 〉 | ~μx0. +(1,x0;★) 〉
8: 〈 Cons(1,Cons(1,Nil;);) | case {Nil ⇒ 〈 0 | ~μx0. +(1,x0;★) 〉,Cons(x0,x1;) ⇒ 〈 μa2. 〈 μa0. sum(x1;a0) | ~μx2. +(x0,x2;a2) 〉 | ~μx0. +(1,x0;★) 〉} 〉
9: 〈 μa0. 〈 μa0. sum(Cons(1,Nil;);a0) | ~μx2. +(1,x2;a0) 〉 | ~μx2. +(1,x2;★) 〉
10: 〈 μa1. sum(Cons(1,Nil;);a1) | ~μx0. +(1,x0;~μx2. +(1,x2;★)) 〉
11: sum(Cons(1,Nil;);~μx0. +(1,x0;~μx2. +(1,x2;★)))
12: 〈 μa1. 〈 Cons(1,Nil;) | case {Nil ⇒ 〈 0 | a1 〉,Cons(x2,x3;) ⇒ 〈 μa1. 〈 μa1. sum(x3;a1) | ~μx1. +(x2,x1;a1) 〉 | a1 〉} 〉 | ~μx0. +(1,x0;~μx2. +(1,x2;★)) 〉
13: 〈 Cons(1,Nil;) | case {Nil ⇒ 〈 0 | ~μx0. +(1,x0;~μx2. +(1,x2;★)) 〉,Cons(x0,x1;) ⇒ 〈 μa2. 〈 μa0. sum(x1;a0) | ~μx2. +(x0,x2;a2) 〉 | ~μx0. +(1,x0;~μx2. +(1,x2;★)) 〉} 〉
14: 〈 μa0. 〈 μa0. sum(Nil;a0) | ~μx2. +(1,x2;a0) 〉 | ~μx2. +(1,x2;~μx2. +(1,x2;★)) 〉
15: 〈 μa1. sum(Nil;a1) | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;★))) 〉
16: sum(Nil;~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;★))))
17: 〈 μa1. 〈 Nil | case {Nil ⇒ 〈 0 | a1 〉,Cons(x2,x3;) ⇒ 〈 μa1. 〈 μa1. sum(x3;a1) | ~μx1. +(x2,x1;a1) 〉 | a1 〉} 〉 | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;★))) 〉
18: 〈 Nil | case {Nil ⇒ 〈 0 | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;★))) 〉,Cons(x0,x1;) ⇒ 〈 μa2. 〈 μa0. sum(x1;a0) | ~μx2. +(x0,x2;a2) 〉 | ~μx0. +(1,x0;~μx2. +(1,x2;~μx2. +(1,x2;★))) 〉} 〉
19: 〈 0 | ~μx1. +(1,x1;~μx0. +(1,x0;~μx0. +(1,x0;★))) 〉
20: +(1,0;~μx2. +(1,x2;~μx0. +(1,x0;★)))
21: 〈 1 | ~μx2. +(1,x2;~μx0. +(1,x0;★)) 〉
22: +(1,1;~μx1. +(1,x1;★))
23: 〈 2 | ~μx1. +(1,x1;★) 〉
24: +(1,2;★)
25: 〈 3 | ★ 〉
```
Alternatively using `def main := repeat(1);` gives evaluation output 
```
---------- Result of Evaluation --------
0: 〈 μa1. repeat(1;a1) | ★ 〉
1: repeat(1;★)
2: 〈 cocase {hd(;a1) ⇒ 〈 1 | a1 〉,tl(;a1) ⇒ 〈 μa1. repeat(1;a1) | a1 〉} | ★ 〉
```

### Example 2.4 
```
def swap(x) := case x of { Tup(y,z) => Tup(z,y) }; 
```
This example similarly shows evaluation and compilation for data types with the following results (using `def main := swap(Tup(1,2));`):
```
---------- Result of Compilation --------
def swap(x;a0) := 〈 μa0. 〈 x | case {Tup(y,z;) ⇒ 〈 Tup(z,y;) | a0 〉} 〉 | a0 〉
```

```
---------- Result of Focusing --------
def swap(x;a0) := 〈 μa0. 〈 x | case {Tup(y,z;) ⇒ 〈 Tup(z,y;) | a0 〉} 〉 | a0 〉
```

```
---------- Result of Evaluation --------
0: 〈 μa1. swap(Tup(1,2;);a1) | ★ 〉
1: swap(Tup(1,2;);★)
2: 〈 μa1. 〈 Tup(1,2;) | case {Tup(x2,x3;) ⇒ 〈 Tup(x3,x2;) | a1 〉} 〉 | ★ 〉
3: 〈 Tup(1,2;) | case {Tup(x0,x1;) ⇒ 〈 Tup(x1,x0;) | ★ 〉} 〉
4: 〈 Tup(2,1;) | ★ 〉
```

### Example 2.5
```
def swaplazy(x) := cocase { fst => x.snd, snd => x.fst };
```
This example shows the analogous of example 2.4 for codata types, as well as the difference between (data) pairs and (codata) lazy pairs .
```
---------- Result of Compilation --------
def swaplazy(x;a0) := 〈 cocase {fst(;a0) ⇒ 〈 μa0. 〈 x | snd(;a0) 〉 | a0 〉,snd(;a0) ⇒ 〈 μa0. 〈 x | fst(;a0) 〉 | a0 〉} | a0 〉
```
```
---------- Result of Focusing --------
def swaplazy(x;a0) := 〈 cocase {fst(;a0) ⇒ 〈 μa0. 〈 x | snd(;a0) 〉 | a0 〉,snd(;a0) ⇒ 〈 μa0. 〈 x | fst(;a0) 〉 | a0 〉} | a0 〉

```
Using `def main := swaplazy(cocase { fst => 1, snd => 2 });` shows the difference in evaluation:

```
---------- Result of Evaluation --------
0: 〈 μa1. swaplazy(cocase {fst(;a1) ⇒ 〈 1 | a1 〉,snd(;a1) ⇒ 〈 2 | a1 〉};a1) | ★ 〉
1: swaplazy(cocase {fst(;a0) ⇒ 〈 1 | a0 〉,snd(;a0) ⇒ 〈 2 | a0 〉};★)
2: 〈 cocase {fst(;a1) ⇒ 〈 μa1. 〈 cocase {fst(;a0) ⇒ 〈 1 | a0 〉,snd(;a0) ⇒ 〈 2 | a0 〉} | snd(;a1) 〉 | a1 〉,snd(;a1) ⇒ 〈 μa1. 〈 cocase {fst(;a0) ⇒ 〈 1 | a0 〉,snd(;a0) ⇒ 〈 2 | a0 〉} | fst(;a1) 〉 | a1 〉} | ★ 〉
```

### Example 2.6
```
def ex26 := (\x=>x*x) 2;
```
This example shows the compilation of lambda abstractions and function applications as well as how these two concepts are special cases of codata types (in particular, the codata type `Fun`), we we can see in the compilation and focusing output of this example

```
---------- Result of Compilation --------
def ex26(;a0) := 〈 μa0. 〈 cocase {ap(x;a0) ⇒ 〈 μa0. *(x,x;a0) | a0 〉} | ap(2;a0) 〉 | a0 〉
```
```
---------- Result of Focusing --------
def ex26(;a0) := 〈 μa0. 〈 cocase {ap(x;a0) ⇒ 〈 μa0. *(x,x;a0) | a0 〉} | ap(2;a0) 〉 | a0 〉
```
Evaluating this using `def main := ex26();` shows how the results are the same as directly evaluating the term directly using the rules of the `Fun` langauge
```
---------- Result of Evaluation --------
0: 〈 μa1. ex26(;a1) | ★ 〉
1: ex26(;★)
2: 〈 μa1. 〈 cocase {ap(x1;a1) ⇒ 〈 μa1. *(x1,x1;a1) | a1 〉} | ap(2;a1) 〉 | ★ 〉
3: 〈 cocase {ap(x0;a0) ⇒ 〈 μa0. *(x0,x0;a0) | a0 〉} | ap(2;★) 〉
4: 〈 μa1. *(2,2;a1) | ★ 〉
5: *(2,2;★)
6: 〈 4 | ★ 〉
```

### Example 2.7
```
def mult(l) := label a { mult2(l;a) };
def mult2(l;a) := case l of { Nil => 1, Cons(x,xs) => ifz(x,goto(0,a),x*mult2(xs;a))};
```
This example is identical to the one found in `examples/FastMultiplication.sc` (which is also used in the introduction) and shows how label and goto terms work when compiled to `Core`, as well as how they can be used to add shortcuts to evaluation. 
The compilation and focusing output is as follows:
```
---------- Result of Compilation --------
def mult(l;a0) := 〈 μa. 〈 μa0. mult2(l;a,a0) | a 〉 | a0 〉
def mult2(l;a0,a) := 〈 μa0. 〈 l | case {Nil ⇒ 〈 1 | a0 〉,Cons(x,xs;) ⇒ 〈 μa0. ifz(x;〈 μa0. 〈 0 | a 〉 | a0 〉,〈 μa0. *(x,μa0. mult2(xs;a,a0);a0) | a0 〉) | a0 〉} 〉 | a 〉
```

```
---------- Result of Focusing --------
def mult(l;a0) := 〈 μa. 〈 μa0. mult2(l;a,a0) | a 〉 | a0 〉
def mult2(l;a0,a) := 〈 μa0. 〈 l | case {Nil ⇒ 〈 1 | a0 〉,Cons(x,xs;) ⇒ 〈 μa0. ifz(x;〈 μa0. 〈 0 | a 〉 | a0 〉,〈 μa0. 〈 μa0. mult2(xs;a,a0) | ~μx0. *(x,x0;a0) 〉 | a0 〉) | a0 〉} 〉 | a 〉
```
When evaluating `mult` with the example list `[2,2,0,3]` using `def main := mult(Cons(2,Cons(2,Cons(0,Cons(3,Nil)))));` shows how evaluation directly stops once we reach the list element `0`

```
---------- Result of Evaluation --------
0: 〈 μa1. mult(Cons(2,Cons(2,Cons(0,Cons(3,Nil;););););a1) | ★ 〉
1: mult(Cons(2,Cons(2,Cons(0,Cons(3,Nil;););););★)
2: 〈 μa1. 〈 μa3. mult2(Cons(2,Cons(2,Cons(0,Cons(3,Nil;););););a1,a3) | a1 〉 | ★ 〉
3: 〈 μa0. mult2(Cons(2,Cons(2,Cons(0,Cons(3,Nil;););););★,a0) | ★ 〉
4: mult2(Cons(2,Cons(2,Cons(0,Cons(3,Nil;););););★,★)
5: 〈 μa1. 〈 Cons(2,Cons(2,Cons(0,Cons(3,Nil;);););) | case {Nil ⇒ 〈 1 | a1 〉,Cons(x2,x3;) ⇒ 〈 μa1. ifz(x2;〈 μa1. 〈 0 | ★ 〉 | a1 〉,〈 μa1. 〈 μa1. mult2(x3;★,a1) | ~μx1. *(x2,x1;a1) 〉 | a1 〉) | a1 〉} 〉 | ★ 〉
6: 〈 Cons(2,Cons(2,Cons(0,Cons(3,Nil;);););) | case {Nil ⇒ 〈 1 | ★ 〉,Cons(x0,x1;) ⇒ 〈 μa2. ifz(x0;〈 μa0. 〈 0 | ★ 〉 | a2 〉,〈 μa0. 〈 μa0. mult2(x1;★,a0) | ~μx2. *(x0,x2;a0) 〉 | a2 〉) | ★ 〉} 〉
7: 〈 μa0. ifz(2;〈 μa0. 〈 0 | ★ 〉 | a0 〉,〈 μa0. 〈 μa0. mult2(Cons(2,Cons(0,Cons(3,Nil;);););★,a0) | ~μx2. *(2,x2;a0) 〉 | a0 〉) | ★ 〉
8: ifz(2;〈 μa1. 〈 0 | ★ 〉 | ★ 〉,〈 μa1. 〈 μa1. mult2(Cons(2,Cons(0,Cons(3,Nil;);););★,a1) | ~μx1. *(2,x1;a1) 〉 | ★ 〉)
9: 〈 μa1. 〈 μa1. mult2(Cons(2,Cons(0,Cons(3,Nil;);););★,a1) | ~μx1. *(2,x1;a1) 〉 | ★ 〉
10: 〈 μa0. mult2(Cons(2,Cons(0,Cons(3,Nil;);););★,a0) | ~μx0. *(2,x0;★) 〉
11: mult2(Cons(2,Cons(0,Cons(3,Nil;);););★,~μx0. *(2,x0;★))
12: 〈 μa1. 〈 Cons(2,Cons(0,Cons(3,Nil;););) | case {Nil ⇒ 〈 1 | a1 〉,Cons(x2,x3;) ⇒ 〈 μa1. ifz(x2;〈 μa1. 〈 0 | ~μx0. *(2,x0;★) 〉 | a1 〉,〈 μa1. 〈 μa1. mult2(x3;~μx0. *(2,x0;★),a1) | ~μx1. *(x2,x1;a1) 〉 | a1 〉) | a1 〉} 〉 | ~μx0. *(2,x0;★) 〉
13: 〈 Cons(2,Cons(0,Cons(3,Nil;););) | case {Nil ⇒ 〈 1 | ~μx0. *(2,x0;★) 〉,Cons(x0,x1;) ⇒ 〈 μa2. ifz(x0;〈 μa0. 〈 0 | ~μx1. *(2,x1;★) 〉 | a2 〉,〈 μa0. 〈 μa0. mult2(x1;~μx1. *(2,x1;★),a0) | ~μx2. *(x0,x2;a0) 〉 | a2 〉) | ~μx0. *(2,x0;★) 〉} 〉
14: 〈 μa0. ifz(2;〈 μa0. 〈 0 | ~μx2. *(2,x2;★) 〉 | a0 〉,〈 μa0. 〈 μa0. mult2(Cons(0,Cons(3,Nil;););~μx2. *(2,x2;★),a0) | ~μx2. *(2,x2;a0) 〉 | a0 〉) | ~μx2. *(2,x2;★) 〉
15: ifz(2;〈 μa1. 〈 0 | ~μx1. *(2,x1;★) 〉 | ~μx2. *(2,x2;★) 〉,〈 μa1. 〈 μa1. mult2(Cons(0,Cons(3,Nil;););~μx1. *(2,x1;★),a1) | ~μx1. *(2,x1;a1) 〉 | ~μx2. *(2,x2;★) 〉)
16: 〈 μa1. 〈 μa1. mult2(Cons(0,Cons(3,Nil;););~μx1. *(2,x1;★),a1) | ~μx1. *(2,x1;a1) 〉 | ~μx2. *(2,x2;★) 〉
17: 〈 μa0. mult2(Cons(0,Cons(3,Nil;););~μx1. *(2,x1;★),a0) | ~μx0. *(2,x0;~μx2. *(2,x2;★)) 〉
18: mult2(Cons(0,Cons(3,Nil;););~μx0. *(2,x0;★),~μx0. *(2,x0;~μx2. *(2,x2;★)))
19: 〈 μa1. 〈 Cons(0,Cons(3,Nil;);) | case {Nil ⇒ 〈 1 | a1 〉,Cons(x2,x3;) ⇒ 〈 μa1. ifz(x2;〈 μa1. 〈 0 | ~μx0. *(2,x0;~μx2. *(2,x2;★)) 〉 | a1 〉,〈 μa1. 〈 μa1. mult2(x3;~μx0. *(2,x0;~μx2. *(2,x2;★)),a1) | ~μx1. *(x2,x1;a1) 〉 | a1 〉) | a1 〉} 〉 | ~μx0. *(2,x0;~μx2. *(2,x2;★)) 〉
20: 〈 Cons(0,Cons(3,Nil;);) | case {Nil ⇒ 〈 1 | ~μx0. *(2,x0;~μx2. *(2,x2;★)) 〉,Cons(x0,x1;) ⇒ 〈 μa2. ifz(x0;〈 μa0. 〈 0 | ~μx1. *(2,x1;~μx0. *(2,x0;★)) 〉 | a2 〉,〈 μa0. 〈 μa0. mult2(x1;~μx1. *(2,x1;~μx0. *(2,x0;★)),a0) | ~μx2. *(x0,x2;a0) 〉 | a2 〉) | ~μx0. *(2,x0;~μx2. *(2,x2;★)) 〉} 〉
21: 〈 μa0. ifz(0;〈 μa0. 〈 0 | ~μx2. *(2,x2;~μx2. *(2,x2;★)) 〉 | a0 〉,〈 μa0. 〈 μa0. mult2(Cons(3,Nil;);~μx2. *(2,x2;~μx2. *(2,x2;★)),a0) | ~μx2. *(0,x2;a0) 〉 | a0 〉) | ~μx2. *(2,x2;~μx2. *(2,x2;★)) 〉
22: ifz(0;〈 μa1. 〈 0 | ~μx1. *(2,x1;~μx0. *(2,x0;★)) 〉 | ~μx2. *(2,x2;~μx2. *(2,x2;★)) 〉,〈 μa1. 〈 μa1. mult2(Cons(3,Nil;);~μx1. *(2,x1;~μx0. *(2,x0;★)),a1) | ~μx1. *(0,x1;a1) 〉 | ~μx2. *(2,x2;~μx2. *(2,x2;★)) 〉)
23: 〈 μa1. 〈 0 | ~μx1. *(2,x1;~μx0. *(2,x0;★)) 〉 | ~μx2. *(2,x2;~μx2. *(2,x2;★)) 〉
24: 〈 0 | ~μx0. *(2,x0;~μx0. *(2,x0;★)) 〉
25: *(2,0;~μx1. *(2,x1;★))
26: 〈 0 | ~μx1. *(2,x1;★) 〉
27: *(2,0;★)
28: 〈 0 | ★ 〉
```

### Section 5.1
```
def sec51 := (2*3)*4;
```
This example included in section 5.1 shows how evaluation contexts are first-class in the `Core` language.
When we compile and focus this, we get the following: 

```
---------- Result of Compilation --------
def sec51(;a0) := 〈 μa0. *(μa0. *(2,3;a0),4;a0) | a0 〉
```
```
---------- Result of Focusing --------
def sec51(;a0) := 〈 μa0. 〈 μa0. *(2,3;a0) | ~μx0. *(x0,4;a0) 〉 | a0 〉
```
After compilation we can see that covariables are introduced and act as continuations of the compilation.
This can be seen even more clearly when evaluating `def main := sec51();`:
```
---------- Result of Evaluation --------
0: 〈 μa1. sec51(;a1) | ★ 〉
1: sec51(;★)
2: 〈 μa1. 〈 μa1. *(2,3;a1) | ~μx0. *(x0,4;a1) 〉 | ★ 〉
3: 〈 μa0. *(2,3;a0) | ~μx1. *(x1,4;★) 〉
4: *(2,3;~μx1. *(x1,4;★))
5: 〈 6 | ~μx1. *(x1,4;★) 〉
6: *(6,4;★)
7: 〈 24 | ★ 〉
```

### Section 5.3
This section considers `let/cc` and `call/cc` in the `Core`-language. Since these are not implemented in `Fun`, we instead use the examples of `let` and `label` to demonstrate how let-bindings are dual to control operators:
```
def letex := let x=2 in x*x;
def labelex := label a { goto(0,a) };
```
Compiling these shows how `let` is translated to mu-abstractions while `label` is translated to mu-tilde-abstractions:

```
---------- Result of Compilation --------
def letex(;a0) := 〈 μa0. 〈 2 | ~μx. 〈 μa0. *(x,x;a0) | a0 〉 〉 | a0 〉
def labelex(;a0) := 〈 μa. 〈 μa0. 〈 0 | a 〉 | a 〉 | a0 〉
```
```
---------- Result of Focusing --------
def letex(;a0) := 〈 μa0. 〈 2 | ~μx. 〈 μa0. *(x,x;a0) | a0 〉 〉 | a0 〉
def labelex(;a0) := 〈 μa. 〈 μa0. 〈 0 | a 〉 | a 〉 | a0 〉
```
Evaluating these two examples using `def main := letex();` and `def main := labelex();`, respectively, also shows the difference in evaluation between mu and mu-tilde (when using call-by-value, as we are throughout the paper)

```
---------- Result of Evaluation --------
0: 〈 μa1. letex(;a1) | ★ 〉
1: letex(;★)
2: 〈 μa1. 〈 2 | ~μx1. 〈 μa1. *(x1,x1;a1) | a1 〉 〉 | ★ 〉
3: 〈 2 | ~μx0. 〈 μa2. *(x0,x0;a2) | ★ 〉 〉
4: 〈 μa0. *(2,2;a0) | ★ 〉
5: *(2,2;★)
6: 〈 4 | ★ 〉
```
```
---------- Result of Evaluation --------
0: 〈 μa1. labelex(;a1) | ★ 〉
1: labelex(;★)
2: 〈 μa1. 〈 μa2. 〈 0 | a1 〉 | a1 〉 | ★ 〉
3: 〈 μa0. 〈 0 | ★ 〉 | ★ 〉
4: 〈 0 | ★ 〉
```

### Section 5.4
```
def casecase := case (case Nil of { Nil => Nil, Cons(x,xs) => xs}) of { Nil => Nil, Cons(y,ys) => ys };
```
This example shows the case-of-case translation explained in section 5.4. 
Since the `Fun` and `Core` languages do not include booleans (as opposed to the example we use in this section), we instead use lists as an example.
The translation is automatically done during compilation: 

```
---------- Result of Compilation --------
def casecase(;a0) := 〈 μa0. 〈 μa0. 〈 Nil | case {Nil ⇒ 〈 Nil | a0 〉,Cons(x,xs;) ⇒ 〈 xs | a0 〉} 〉 | case {Nil ⇒ 〈 Nil | a0 〉,Cons(y,ys;) ⇒ 〈 ys | a0 〉} 〉 | a0 〉
```
```
---------- Result of Focusing --------
def casecase(;a0) := 〈 μa0. 〈 μa0. 〈 Nil | case {Nil ⇒ 〈 Nil | a0 〉,Cons(x,xs;) ⇒ 〈 xs | a0 〉} 〉 | case {Nil ⇒ 〈 Nil | a0 〉,Cons(y,ys;) ⇒ 〈 ys | a0 〉} 〉 | a0 〉
```
From the compilation rules, one can also see how this generalizes to arbitrary data types (and cocases of codata types), and evaluating this example (`def main := casecase();`) shows how this translation does not change the result.
```
---------- Result of Evaluation --------
0: 〈 μa1. casecase(;a1) | ★ 〉
1: casecase(;★)
2: 〈 μa1. 〈 μa1. 〈 Nil | case {Nil ⇒ 〈 Nil | a1 〉,Cons(x0,x2;) ⇒ 〈 x2 | a1 〉} 〉 | case {Nil ⇒ 〈 Nil | a1 〉,Cons(x0,x2;) ⇒ 〈 x2 | a1 〉} 〉 | ★ 〉
3: 〈 μa0. 〈 Nil | case {Nil ⇒ 〈 Nil | a0 〉,Cons(x0,x2;) ⇒ 〈 x2 | a0 〉} 〉 | case {Nil ⇒ 〈 Nil | ★ 〉,Cons(x0,x1;) ⇒ 〈 x1 | ★ 〉} 〉
4: 〈 Nil | case {Nil ⇒ 〈 Nil | case {Nil ⇒ 〈 Nil | ★ 〉,Cons(x0,x1;) ⇒ 〈 x1 | ★ 〉} 〉,Cons(x0,x1;) ⇒ 〈 x1 | case {Nil ⇒ 〈 Nil | ★ 〉,Cons(x0,x1;) ⇒ 〈 x1 | ★ 〉} 〉} 〉
5: 〈 Nil | case {Nil ⇒ 〈 Nil | ★ 〉,Cons(x0,x2;) ⇒ 〈 x2 | ★ 〉} 〉
6: 〈 Nil | ★ 〉
```

### Section 5.5
As our languages also do not contain any mutable structures, we cannot directly use the example for direct and indirect consumers in section 5.5.
Instead, we use the example of the `tl` destructor of streams:
```
def tltltl := repeat(1).tl.tl.tl;
```
When compiling this to `Core`, we still have the chained destructors as in the surface language, which would be lost in a traditional CPS translation:
```
---------- Result of Compilation --------
def tltltl(;a0) := 〈 μa0. 〈 μa0. 〈 μa0. 〈 μa0. repeat(1;a0) | tl(;a0) 〉 | tl(;a0) 〉 | tl(;a0) 〉 | a0 〉
```
```
---------- Result of Focusing --------
def tltltl(;a0) := 〈 μa0. 〈 μa0. 〈 μa0. 〈 μa0. repeat(1;a0) | tl(;a0) 〉 | tl(;a0) 〉 | tl(;a0) 〉 | a0 〉
```
When evaluating this example (`def main := tltltl();`), we can also see that this property is preserved during evaluation.
```
---------- Result of Evaluation --------
0: 〈 μa1. tltltl(;a1) | ★ 〉
1: tltltl(;★)
2: 〈 μa1. 〈 μa1. 〈 μa1. 〈 μa1. repeat(1;a1) | tl(;a1) 〉 | tl(;a1) 〉 | tl(;a1) 〉 | ★ 〉
3: 〈 μa0. 〈 μa0. 〈 μa0. repeat(1;a0) | tl(;a0) 〉 | tl(;a0) 〉 | tl(;★) 〉
4: 〈 μa1. 〈 μa1. repeat(1;a1) | tl(;a1) 〉 | tl(;tl(;★)) 〉
5: 〈 μa0. repeat(1;a0) | tl(;tl(;tl(;★))) 〉
6: repeat(1;tl(;tl(;tl(;★))))
7: 〈 cocase {hd(;a1) ⇒ 〈 1 | a1 〉,tl(;a1) ⇒ 〈 μa1. repeat(1;a1) | a1 〉} | tl(;tl(;tl(;★))) 〉
8: 〈 μa0. repeat(1;a0) | tl(;tl(;★)) 〉
9: repeat(1;tl(;tl(;★)))
10: 〈 cocase {hd(;a1) ⇒ 〈 1 | a1 〉,tl(;a1) ⇒ 〈 μa1. repeat(1;a1) | a1 〉} | tl(;tl(;★)) 〉
11: 〈 μa0. repeat(1;a0) | tl(;★) 〉
12: repeat(1;tl(;★))
13: 〈 cocase {hd(;a1) ⇒ 〈 1 | a1 〉,tl(;a1) ⇒ 〈 μa1. repeat(1;a1) | a1 〉} | tl(;★) 〉
14: 〈 μa0. repeat(1;a0) | ★ 〉
15: repeat(1;★)
16: 〈 cocase {hd(;a1) ⇒ 〈 1 | a1 〉,tl(;a1) ⇒ 〈 μa1. repeat(1;a1) | a1 〉} | ★ 〉
```

### Section 5.6
```
def criticalPair := let x=label a { goto(1,a) } in x;
```
We use this example to demonstrate the differences between call-by-name and call-by-value as explained in section 5.6. 
After compilation, this contains a critical pair, i.e. a cut between a mu-abstraction and a mu-tilde abstraction, which demonstrates the difference between call-by-cvalue and call-by-name in the `Core` language:
```
---------- Result of Compilation --------
def criticalPair(;a0) := 〈 μa0. 〈 μa. 〈 μa0. 〈 1 | a 〉 | a 〉 | ~μx. 〈 x | a0 〉 〉 | a0 〉
```
```
---------- Result of Focusing --------
def criticalPair(;a0) := 〈 μa0. 〈 μa. 〈 μa0. 〈 1 | a 〉 | a 〉 | ~μx. 〈 x | a0 〉 〉 | a0 〉
```
Evaluation then shows how in a call-by-value alnguage, mu-abstractions are evaluated first 
```
---------- Result of Evaluation --------
0: 〈 μa1. criticalPair(;a1) | ★ 〉
1: criticalPair(;★)
2: 〈 μa1. 〈 μa1. 〈 μa2. 〈 1 | a1 〉 | a1 〉 | ~μx1. 〈 x1 | a1 〉 〉 | ★ 〉
3: 〈 μa0. 〈 μa2. 〈 1 | a0 〉 | a0 〉 | ~μx0. 〈 x0 | ★ 〉 〉
4: 〈 μa1. 〈 1 | ~μx0. 〈 x0 | ★ 〉 〉 | ~μx0. 〈 x0 | ★ 〉 〉
5: 〈 1 | ~μx1. 〈 x1 | ★ 〉 〉
6: 〈 1 | ★ 〉
```
If we instead used call-by-name, the mu-tilde abstraction would be evaluated first. 
This is also how eta-laws are applied in call-by-value languages and call-by-name languages, where call-by-value allows eta-reductions for data types and call-by-name for codata types.
This can be seen in the output of simplification for this example:
```
add output in web app
```
Note that this simplification is only included in the output of the web demo, and not in the output of the binary.

### Code 

To see how the definitions and formulas in the paper are implemented in Haskell, we will give a quick overview of what can be found where 

* `Core` syntax (introduced in section 2) is implemented in `src/Core/Syntax.hs`
* `Fun` syntax (introduced in section 2) is implemented in `src/Fun/Syntax.hs`
* Compilation rules (introduced in section 2) are implemented in `src/Compiler/hs`.
    The output of compilation can be see when running the binary under the `---------- Result of Compilation --------` heading, and under `Core Representation > Compiled` in the web demo 
* A parser for `Fun` is implemented in `src/Fun/Parser.hs`
* Static Focusing (introduced in section 3.2, definition 3.2) is implemented in `src/Core/Focusing.hs`
    The output of focusing can be seen when running the binary under the `---------- Result of Focusing --------` heading, and under `Core Represntation > Focused` in the web demo
* Evaluation rules for Core (introduced in section 2) are implemented in `src/Core/Eval.hs`
    The output of evaluation can be seen when running the binary under the `---------- Result of Evaluation --------` heading, and under `Evaluation` in the web demo.
    Evaluation is only run for the `main` definition in a program, so any program without such a function will not have any evaluation results (neither in the binary nor the web demo)
    Furthermore, evaluation is only implemented for `Core` and not for `Fun`, since we treat `Fun` as a surface language compiled to `Core` only after which it is evaluated.
* Simplification, including eta reductius (section 5.6) is implemented in `src/Core/Simplify.hs`.
    The results of simplification are not included when running the binary, but in the web demo under `Core Representation > Simplified`. 
    This simplification step is skipped when running the binary, as evaluation is equivalent on simplified and non-simplified terms, which can be seen by running the same example in the web demo and using the binary.
* Fresh covariables and variables that are generated during both compilation (denoted in section 2 by `fresh`) are implemented in `src/Core/Substitution.hs`
    This uses a type class `FreeV` to make sure no shadowing can occur when generating a variable
* Substitution during evaluation (for example for mu and mu-tilde) is implemented in `src/Core/Substitution.hs`
* Typing rules for `Fun` (introduced in section 4.1 and appendix B) are implemented in `src/Fun/Types.hs`
    Because by theorem 4.6, types are preserved under translation, we have not provided type inference for `Core`, since we can be sure if a term can be typed in `Fun` it can also be typed in `Core` with the same type.
