# Sequent Calculus

## How to build the web-demo

In order to build the webdemo, you have to install a crosscompiler of GHC.
This is still a bit experimental, but you can use the instructions here: https://www.haskell.org/ghcup/guide/#cross-support
Be sure to first install an emscripten toolchain.

Once you have verified that you can run the crosscompiler, for example by checking its version:

```console
> javascript-unknown-ghcjs-ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.10.0.20240413

```

You can use the `build-webdemo.sh` script to build:

```console
> ./build-webdemo.sh
Building executable 'web-demo' for sequent-calculus-0.1.0.0..
[1 of 1] Compiling Main             ( web-app/Main.hs, /Users/david/GitRepos/introduction-to-sequent-calculus/code/dist-newstyle/build/javascript-ghcjs/ghc-9.6.2/sequent-calculus-0.1.0.0/x/web-demo/build/web-demo/web-demo-tmp/Main.o )
[2 of 2] Linking /Users/david/GitRepos/introduction-to-sequent-calculus/code/dist-newstyle/build/javascript-ghcjs/ghc-9.6.2/sequent-calculus-0.1.0.0/x/web-demo/build/web-demo/web-demo.jsexe
```

This also tells you where in the `dist-newstyle` directory you can find the generated JS code.

Once the build has finished successfully, the final comipled javascript code will be copied to the `web-app` directory and can be run by opening `index.html` in a web-browser.

## Packing 

All the source code found in this directory can be packed into a zip folder using the `pack.sh` bash script.
This will also include the compiled `all.js` script, created by the `build-webdemo.sh`. When that build script is run a second time, the `all.js` file will be overwriten by the new version.
