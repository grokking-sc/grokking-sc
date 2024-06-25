# Open 

* fix the errors Invalid hexadecimal escape sequence and all_examples is not defined in index.html (the drop-down list "Select example..." is empty)
* README "add the additional flag `-nic user,hostfwd=tcp:8000-:8000`" (hostaddr column before hostport missing, cf. qemu(1))
* README "this becomes `./start.sh -nic user,hostfwd=tcp:8000-:8000`" (hostaddr column before hostport missing, cf. qemu(1))
* add a concrete way of installing the required versions of GHC and Cabal to the README along the following lines:
    ```
	> # you might want to `export GHCUP_USE_XDG_DIRS=1` to use XDG-style directories
	> ghcup install ghc 9.4 --set
	> ghcup install cabal
	> EMSDK_SRCDIR=$(mktemp -dt emsdk.XXXXXXXXXX)
	> git clone https://github.com/emscripten-core/emsdk.git $EMSDK_SRCDIR
	> cd $EMSDK_SRCDIR
	> ./emsdk install 3.1.57
	> ./emsdk activate 3.1.57
	> source ./emsdk_env.sh
	> cd -
	> ghcup config add-release-channel https://raw.githubusercontent.com/haskell/ghcup-metadata/develop/ghcup-cross-0.0.8.yaml
	> emconfigure ghcup install ghc --set javascript-unknown-ghcjs-9.10.0.20240413
	> # you might need to pre- or append $HOME/.local/bin or $HOME/.ghcup/bin to PATH
	> command -v ghc cabal javascript-unknown-ghcjs-ghc
	> ghc --version
	> cabal --version
	> javascript-unknown-ghcjs-ghc --version
    ```
* add a summary of the surface syntax (note that, as reported by @A6, there seems to be an inconsistency between what is suggested by the paper and what is expected by the parser for the goto argument list syntax: goto(T; a) vs. goto(T, a)) to the README along the following lines:
    ```
	P ::= def f(x,..; a,..) := T;..
	T ::= x
	    | n
	    | (T)
	    | T * T | T + T | T - T
	    | ifz(T, T, T)
	    | let x = T in T
	    | f(T,..; a,..)
	    | K(T,..)    | case T of { K(x,..) => T,.. }
	    | T.D(T,..)  | cocase { D(x,..) => T,.. }
	    | \x => T | T T
	    | label a { T } | goto(T; a)
	K ::= Nil | Cons | Tup
	D ::= hd | tl | fst | snd | ap
    ```
* where in the covariable argument list the "additional [continuation] parameter α [is added] to every top-level definition when [they are translated]" in the case compileDef (Fun.Def nm prodargs (Just cv) bd rt) of compileDef :: Fun.Def a -> Core.Def a in src/Compiler.hs (it does not seem to be added at the end as the comment right above the function definition suggests)
* the recursion in the case focus s@(Fun nm pargs cargs) of instance Focus Statement in src/Core/Focusing.hs (it does not seem to happen as the comment right above the case suggests, also newArgs seems to replace all producer arguments by the first non-value producer argument)
* the case focus dest@(Destructor dt pargs cargs) of instance Focus Consumer in src/Core/Focusing.hs (it does not seem to be correct nor match the corresponding rule in Definition 3.2 in the paper)
* add a function-application-with-multiple-non-value-arguments example along the lines of def main := (\x => \y => y) (1 + 2) (3 + 4); and make sure that it gets focused correctly
* superfluous label–goto in def multFast(x) := label a { case x of {Nil=>1, Cons(y, ys) => ifz(y,goto(0,a),y * multFast(ys))}}; in Lists.sc
*  It was not completely obvious what to do and what to install, but this was expected since I am not an expert of the Haskell ecosystem. At the end, however, I was able to compile everything from scratch.
* Syntax of goto
    The first one is actually only a minor problem. While the syntax of goto in the paper is goto(t;a), the compiler needs a comma instead of the semicolon, otherwise compilation stops (with a quite misleading error message). For example, the program `def main := label a { 3 + goto(0;a) };`gives the error
	```
	<interactive>:1:25:
	  |
	1 | def main := label a { 3 + goto(0;a) };
	  |                         ^
	unexpected '+'
	expecting '}'
	```
* Problems with co-variables in the formal parameters of a function Now consider, the following program
	```
	def f1(v) := label a0 {f2(v;a0)};
	def f2(v;a0) := ifz(v, goto(0,a0), v + f2(v-1;a0));
	def main := f1(1);
	```
    The program does compile, but its result is 1. I think this is wrong, because when f2 is called recursively with v=0, it should jump to the label a in the function f1 with return value 0: all pending computations should be discharged.
    I think the culprit is the way goto is compiled. The artifact compiles the f2 function as follows:
    ```
    def f2(v;a1,a0) := 〈 μa1. ifz(v;〈 μa1. 〈 0 | a0 〉 | a1 〉,〈 μa0. +(v,μa0. f2(μa0. -(v,1;a0);a0,a0);a0) | a1 〉) | a0 〉
    ```
    But I think the correct compiled code is the following:
    ```
	def f2(v;a0,a) := 
	   〈 μa1. 
	      ifz(v;
	          〈 μa2. 〈 0 | a0 〉 | a1 〉,
	          〈 μa3. +(v,μa4. f2(μa5. -(v,1;a5);a0,a4); a3) | a1 〉
	       ) 
	  | a 〉
    ```
    The twos are not alpha-convertible. It seems there are two points where the artifact is wrong:
    the goto should be compiled to μa2.〈 0 | a0 〉where a0 is the second formal parameter of f2. However, in the artifact's output, a0 is the third argument of f2, which is the standard return consumer.
    in the recursive call to f2, the second argument should be equal to the second formal parameter in the definition of f2, like in my hand-made compiled code. However, in the artifact's compiled code, the consumer arguments of f2 are equal.
* Problems in the focusing algorithm: Consider the file examples/ArithmeticExpressions.sc, in particular the function monus
    ```
    def monus(n,m) := ifz(m,n,ifz(n,0,monus(n-1,m-1)));
    ```
    It is compiled by the artifact as
    ```
    def monus(n,m;a0) := 〈 μa0. ifz(m;〈 n | a0 〉,〈 μa0. ifz(n;〈 0 | a0 〉,〈 μa0. monus(μa0. -(n,1;a0),μa0. -(m,1;a0);a0) | a0 〉) | a0 〉) | a0 〉
    ```
    which seems correct (although calling a0 all bound variables makes checking the code very hard). The focused code returned by the artifact is
    ```
    def monus(n,m;a0) := 〈 μa0. ifz(m;〈 n | a0 〉,〈 μa0. ifz(n;〈 0 | a0 〉,〈 μa0. 〈 μa0. -(n,1;a0) | ~μx0. monus(x0,μa0. -(n,1;a0);a0) 〉 | a0 〉) | a0 〉) | a0 〉
    ```
    and this is clearly wrong, for two reasons:
    * (m,1;a0) in the original compiled code becomes -(n,1;a0) in the focused one;
    * focusing is not complete: the code still contains monus(x0,μa0. -(n,1;a0);a0) which is not correctly focused. Actually, execution of monus(10,5) does not terminate with a value.
* Make Core programs more readable
    Core programs would be much more readable if bound variables were renamed to be all different one from another. In this moment, it is exactly the opposite: the compiler tries to reuse the same variables over and over. However, this makes very difficult for an human being to read the generated code, since it is hard to match each variable occurrence with the corresponding binder.
* Make easier to test the web-based demo
    It was particularly difficult for me to test the web-based demo, since I fall in the category of people for whom adding
    ```
    -nic user,hostfwd=tcp:8000-:8000
    ```
    to ```start.sh`` makes it impossible to connect to the VM.
    Unfortunately, also the approach of mounting the guest file system in the host was not completely trivial. I am using Ubuntu 24.04, and I had to change the permission of the host Linux kernel file with the command:
    ```
    chmod 644 /boot/vmlinuz-$(uname -r)
    ```
    before I was able to use the guestmount command.
    Therefore I would suggest to:
    * add hostfwd=tcp:8000-:8000 directly to the netdev option in the start.sh file (which should be included in the artifact anyway), since this should work in any system;
    * suggest to run the web-based demo directly from the source code in the grokking-sc-AEC2.tar.gz archive, since it already contains a compiled version of the web GUI.
* In examples/List.sc there is a weird implementation of multFast function, different from the one in examples/FastMultiplication.sc. It uses goto in a way not particularly interesting, since this goto only exits from the current execution frame, like a simple 0 expression would, and not from all the recursive calls of multFast as the implementation in examples/FastMultiplication.sc (and in the paper).

# Done 

* I could not find the username and password to use anywhere, so I had to guess them (username: artifact, password: password). 
        fixed, password is included in the README for the disk tarball (`README_artifact.md`)
* Also, instructions on the README.md file are not completely reliable. They say to use "make run -filepath=FILENAME", but the command is actually "make run filepath=FILENAME" without the minus sign. 
        fixed
* examples FastMultiplication, Lists and Stream do not compile.
        fixed
* add a README to the Zenodo record (next to the two archives) that summarizes the contents of the record and of each of the two archives that it contains
        done
* remove the .git, .git.old, .github, .gitignore, grokking-src.zip and dist-newstyle directories and files from the source archive
        fixed, all files are removed by the `clean_repo.sh` script
* fix the typo "simply run `cabal build sequent calculus`" in the README
        fixed, now says `cabal build exe:sequent-calculus`
* fix the typo "in the `web-demo` directory" in the README
        fixed, now all directory references use `web-app`
* add instructions on how to serve the web demo from the virtual machine to the README
        fixed, added instructions for serving using http server (with port tunneling) and mounting the disk archive (still different comment below though)
* remove the .git, .git.old, .github, .gitignore and \ (!) directories and files from the grokking-sc directory in the disk image
        fixed, same as above
* make sure that (both the binary and) the web demo are already built in the disk image (as suggested by @A4, including instructions on how to rebuild the binary and the web demo is helpful)
        fixed, added `clean` target to allow rebuilding
* The README suggested to run cabal build sequent calculus, however I had to run cabal build exe:sequent-calculus.
        fixed
* Since the disk image contains the compiled files, running cabal build exe:sequent-calculus or make build-exe initially does nothing. Please include instructions with README to delete all built files.
        fixed, added `clean` target
* Include instructions on how to run the executable on one of the example files.
        fixed, added more explanation to `make run`
* include the startup scripts start.sh and start.bat in the disk archive
        fixed
* README "The command will thn host the web-demo locally"
        fixed
* README "Ubunutu/Debian"
        fixed
* README "run the copmmand `guestmount -a disk.qcow -m /dev/sda1 /mnt`"
        fixed
* README "All examples included in the `examples` directoty"
        fixed
* README "During many steps of compliation and evaluation"
        fixed
* README "where theiy compiled terms can be compared"
        fixed
* README "Evaluating this [paragraph ends unexpectedly]"
        fixed
* add a concrete example of the cabal run sequent-calculus FILENAME and make run filepath=FILENAME command lines to the README
    fixed, run for lists included in README
* missing .snd in def swapLazy(x) := cocase { fst=>x, snd=>x.fst}; in LazyPair.sc
        fixed
* duplicate def map(f, l) := case l of {Nil=>Nil, Cons(x, xs) => Cons(f, map(f, xs))}; in Lists.sc
        fixed
* Include a link to the Zenodo archive of the base image in the README. This will allow users to find the base image easily.
        fixed
* The disk image contains a file ''. I suggest removing this file. There are other superfluous files (e.g., .git) that can be removed.
        fixed with the `clean_repo.sh`
* In examples/LazyPair.sc: the swapLazy function has a typo. It should be def swapLazy(x) := cocase { fst=>x.snd, snd=>x.fst}; but it is missing .snd after x.
        fixed
* In examples/List.sc there are two copies of the map function;
        fixed
* include the list of claims the paper claims to be supported by the artifact alongside instructions on how to verify each of them in the README, not only to help us review but also others navigate your artifact efficiently 
        fixed, added `paper_examples.sc` in examples
* the formatting of the section "Paper Claims" in the README can be improved
        fixed
