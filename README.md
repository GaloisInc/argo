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

### Emacs

There is a little test rig written in Emacs Lisp to automate the
production of commands and log responses. Note that these commands can
be sensitive to the current working directory in emacs.

There are two ways to use it: over stdio, or over a socket. The
initial setup for both is the same:
1. Launch emacs
2. Open `proto-test.el`
3. Evaluate the buffer: `M-x eval-buffer` or on spacemacs: `, e b`

To use the stdio version:
1. `M-x proto-test-start`
2. At the prompt for `Command:`, run proto server `cabal v2-exec cryptol-remote-api`

To use the socket version:
1. At a shell, run `cabal v2-exec cryptol-remote-api -- --socket 10006` (or pick your favorite port instead of 10006)
2. In Emacs, `M-x proto-test-start-socket`. When prompted, enter `10006` or your choice of port.

Invoking methods:

Currently it is necessary to load a file first before using any other
methods, because that brings the Cryptol prelude into scope. These
Elisp wrappers will prompt you for appropriate input.

1. `M-x proto-test-cryptol-load-file`
2. `M-x proto-test-cryptol-eval`
3. `M-x proto-test-cryptol-change-directory`
4. `M-x proto-test-cryptol-call`
5. `M-x proto-test-cryptol-focused-module`
6. `M-x proto-test-cryptol-check-type`
7. `M-x proto-test-cryptol-cyptol-satisfy`

Terminating the demo:

1. `M-x proto-test-quit`
