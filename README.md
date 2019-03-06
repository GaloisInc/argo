# Protocol

## Build Instructions

### Build tools

Requirements:

* cabal-2.4.1.0
* GHC-8.6.4

Any easy to way get GHC and cabal-install installed is to use ghcup;
however any other method will be fine.

https://github.com/haskell/ghcup/blob/master/README.md

```
$ ghcup install 8.6.4
$ ghcup set 8.6.4 # optional
$ ghcup install-cabal
```

### Building the project

You might have to manually specify the GHC version when configuring this
project. This can be achieved with `v2-configure` as seen below.

This project has multiple targets. To build them all you can specify `all`.

```
$ cabal v2-configure -w ghc-8.6.4
$ cabal v2-build all
```

## Usage

Requirements:

* emacs

Running the demonstration emacs mode uses the following steps. Note that
these commands can be sensitive to the current working directory in emacs.

1. Launch emacs
2. Open `proto-test.el`
3. Evaluate the buffer: `M-x eval-buffer` or on spacemacs: `, e b`
4. Enter `Command:` to run proto server `cabal v2-exec proto`

Invoking methods:

Currently it is necessary to load a file first before using any other methods.

1. `M-x proto-test-cryptol-load-file`
2. `M-x proto-test-cryptol-eval`
2. `M-x proto-test-cryptol-change-directory`

Terminating the demo:

1. `M-x proto-test-quit`
