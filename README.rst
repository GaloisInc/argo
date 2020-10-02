Remote Bindings for SAW and Cryptol
===================================

Build Instructions
------------------

Build tools
~~~~~~~~~~~

Requirements:

* cabal-install 2.4.1.0 or 3.0.0
* GHC-8.6.4 or GHC-8.8.3
* Python 3.7

Any easy to way get GHC and cabal-install installed is to use `ghcup`_;
however any other method will be fine::

    $ ghcup install 8.6.4
    $ ghcup set 8.6.4 # optional
    $ ghcup install-cabal

.. _ghcup: https://gitlab.haskell.org/haskell/ghcup

Building the project
~~~~~~~~~~~~~~~~~~~~

To clone the project, you'll need to initialize its submodules as well::

    $ git clone https://github.com/galoisinc/argo
    $ cd argo/
    $ git submodule init
    $ pushd . && cd deps/saw-script/ && git submodule update --init && cd deps/abcBridge/ && git submodule update --init && popd

Most of the submodules are loaded recursively from the saw-script
submodule, because that repository already contains the result of
doing the work to synchronize different versions. Recursively
initializing all submodules will result in too many unneccessary
checkouts. However, if you encounter mysterious build errors
mentioning `abcBridge`, it is likely because it is missing its
submodules.

You might have to manually specify the GHC version when configuring this
project. This can be achieved with ``v2-configure`` as seen below.

This project has multiple targets. To build them all you can specify ``all``::

    $ cabal v2-configure -w ghc-8.6.4
    $ cabal v2-build all

Development
-----------

Documentation
-------------

The protocol and the Python bindings are described in Sphinx-buildable
ReStructuredText format in the [docs](docs/) subdirectory. Use ``make html``
in that directory to build readable HTML output.

Python
~~~~~~

The real goal of this system is to support Python bindings to SAW. The
Python bindings are in the ``python`` subdirectory. Right now, the
Cryptol support is more advanced, but the SAW support is under
development. The bindings are tested only with Python 3.7.

To install the Python bindings, we recommend the use of a "virtual
environment" that isolates collections of Python packages that are
used for different projects. To create a virtual environment, use the
command::

    python3 -m venv virtenv

The preferred mode of use for virtual environments is to *activate*
them, which modifies various environment variables to cause the
current shell's view of the Python implementations, tools, and
libraries to match the environment. For instance, ``PATH`` is modified
to prioritize the virtual environment's Python version, and that
Python is pointed at the specific collection of libraries that are
available. Under a broadly Bourne-compatible shell like ``bash`` or
``zsh``, source the appropriate file in the environment::

   . virtenv/bin/activate

to activate the environment. Deactivate it using the shell alias
``deactivate`` that is defined by ``activate``. For other shells or
operating systems, please consult the documentation for ``venv``. If
you prefer not to activate the environment, it is also possible to run
the environment's version of Python tooling by invoking the scripts in
its ``bin`` directory.

In the virtual environment, run the following command to install the
library's dependencies::

    pip install -r python/requirements.txt

Next, install the library itself::

    pip install -e python/

The ``-e`` flag to ``pip install`` causes it to use the current files
in the repository as the library's source rather than copying them to
a central location in the virtual environment. This means that they
can be edited in-place and tested immediately, with no reinstallation
step. If you'd prefer to just install them, then omit the ``-e`` flag.

Working on the Python bindings
==============================

To run the ``mypy`` type checker, enter the virtual environment and then run::

    mypy argo saw cryptol

from the ``python`` subdirectory.

Testing the Python bindings
===========================

To test out the Python bindings, load the test file in a Python
REPL. We recommend ``ipython3``, because it provides easy access to
docstrings and tab completion. Here's an example command line and
Python session to give you an idea of what's currently implemented,
with commentary in the form of Python comments::

    $ ipython3 -i cryptol-api_test.py
    Python 3.7.2 (default, Jan 16 2019, 19:49:22)
    Type 'copyright', 'credits' or 'license' for more information
    IPython 6.4.0 -- An enhanced Interactive Python. Type '?' for help.

    # A CryptolConnection represents a connection to the Cryptol server
    # that has a particular state. The state is created by issuing commands
    # such as loading files. The test file creates a connection, changes the
    # working directory, and loads a file called Foo.cry.
    In [1]: c?
    Type:        CryptolConnection
    String form: <cryptol.CryptolConnection object at 0x7fa16fbf6780>
    File:        ~/Projects/proto/proto/python/cryptol/__init__.py
    Docstring:   <no docstring>

    In [2]: c.protocol_state()
    Out[2]:
    [['change directory', {'directory': '/home/dtc/Projects/proto/proto/python'}],
     ['load module', {'file': 'Foo.cry'}]]

    # A CryptolContext is a wrapper around a connection that allows easy access to
    # the names in scope at that particular state. Constructing a CryptolContext
    # takes a lightweight snapshot of the state, so it will still work even if the
    # connection is later used for other commands.
    In [3]: ctx = CryptolContext(c)


    # The Connection is a low-level interface, without easy access to Cryptol. It is
    # not particularly suitable to interactive experimentation, but may be a good target
    # for applications looking to script Cryptol.
    In [4]: c.add?
    Object `c.add` not found.

    # The Context has methods corresponding to each name in scope, though there is not
    # yet an easy way to call infix operators. Note that the Cryptol type is shown as
    # part of the Python docstring.
    In [5]: ctx.add?
    Signature:   ctx.add(*args)
    Type:        CryptolFunctionHandle
    String form: <cryptol.CryptolFunctionHandle object at 0x7fa16e878c50>
    File:        ~/Projects/proto/proto/python/cryptol/__init__.py
    Docstring:   Cryptol type: {a} (fin a) => [a] -> [a] -> [a]

    # There are heuristic rules for converting Python data to the associated Cryptol
    # data, taking the Cryptol type into account:
    In [6]: ctx.add(bytes.fromhex('ff'), bytes.fromhex('01'))
    Out[6]: b'\x00'

    # Additionally, the current state of a connection can be used to construct a Python
    # module from which Cryptol names can be imported directly:

    In [7]: cryptol.add_cryptol_module('Foo', c)

    In [8]: from Foo import *

    # Because b'\2' is enough to solve the type variable a in add's type, the integer 2
    # can be used as a bitvector. There is not yet a way to supply a explicitly.
    In [9]: add(b'\2', 2)
    Out[9]: b'\x04'

    # Cryptol documentation is also carried over to Python, whether through a Context or
    # through a module.
    In [10]: ctx.carry?
    Signature:   ctx.carry(*args)
    Type:        CryptolFunctionHandle
    String form: <cryptol.CryptolFunctionHandle object at 0x7fa16e87b748>
    File:        ~/Projects/proto/proto/python/cryptol/__init__.py
    Docstring:
    Cryptol type: {n} (fin n) => [n] -> [n] -> Bit
    Unsigned carry.  Returns true if the unsigned addition of the given
    bitvector arguments would result in an unsigned overflow.

    In [11]: carry?
    Signature:   carry(*args)
    Type:        CryptolFunctionHandle
    String form: <cryptol.CryptolFunctionHandle object at 0x7fa16e7bb6a0>
    File:        ~/Projects/proto/proto/python/cryptol/__init__.py
    Docstring:
    Cryptol type: {n} (fin n) => [n] -> [n] -> Bit
    Unsigned carry.  Returns true if the unsigned addition of the given
    bitvector arguments would result in an unsigned overflow.



Emacs
~~~~~

There is a little test rig written in Emacs Lisp to automate the
production of commands and log responses. Emacs was chosen because it
makes it easy to run a subprocess and communicate with it over a pipe
or socket --- don't expect fancy editor support for Cryptol or much
ease of use from the integration. Note that these commands can be
sensitive to the current working directory in Emacs.

There are two ways to use it: over stdio, or over a socket. The
initial setup for both is the same:

1. Launch emacs

2. Open ``proto-test.el``

3. Evaluate the buffer: ``M-x eval-buffer`` or on Spacemacs: ``, e b``

To use the stdio version:

1. ``M-x proto-test-start``

2. At the prompt for ``Command:``, run the server with ``cabal v2-exec -v0
   saw-remote-api`` or ``cabal v2-exec -v0 cryptol-remote-api``.

If this leaves a confusing error message in Emacs, the output was
probably corrupted by ``cabal-install`` stating that nothing needs
building. Run ``cabal v2-build all`` to make sure that all builds are
up-to-date, and try again.


To use the socket version:

1. At a shell, run ``cabal v2-exec cryptol-remote-api -- --port 10006``
   (or pick your favorite port instead of 10006)

2. In Emacs, ``M-x proto-test-start-socket``. When prompted, enter
   ``10006`` or your choice of port.

Invoking methods:

Currently it is necessary to load a file first before using any other
methods, because that brings the Cryptol prelude into scope. These
Elisp wrappers will prompt you for appropriate input.

1. ``M-x proto-test-cryptol-load-file``
2. ``M-x proto-test-cryptol-eval``
3. ``M-x proto-test-cryptol-change-directory``
4. ``M-x proto-test-cryptol-call``
5. ``M-x proto-test-cryptol-focused-module``
6. ``M-x proto-test-cryptol-check-type``
7. ``M-x proto-test-cryptol-cyptol-satisfy``

Terminating the demo:

1. ``M-x proto-test-quit``



(If the above description is stale, please file an issue or let
``dtc@galois.com`` know)
