Argo: A JSON-RPC Server Library
===============================

This repository contains ``argo``, a library designed for adding
JSON-RPC support to existing applications. Features include:

* Explicit management of application state, enabling low-cost
  rollbacks to earlier states and multiple independent clients.

* Support for socket, ``stdio``, and HTTP transports, all in one server.

* Caching of application states, including filesystem state.

* Python bindings

Users
-----

This library is currently used to provide JSON-RPC interfaces to
`Cryptol <https://github.com/GaloisInc/cryptol/tree/master/cryptol-remote-api>`_
and `SAW <https://github.com/GaloisInc/saw-script/tree/master/saw-remote-api>`_.


Build Instructions
------------------

This repository contains:

* The ``argo`` library, in Haskell

* Python bindings for ``argo``, as well as for the Cryptol and SAW clients

* Sphinx documentation for the above

* A sample server, in the ``file-echo-api`` directory. This server
  demonstrates the use of ``argo`` and is useful for testing.

* An extension to ``tasty`` for invoking scripts written in a language
  other than Haskell (in this case, Python). This is in the
  ``tasty-script-exitcode`` directory.

Argo is primarily intended to be used as a dependency of another
server. To build and test it on its own, use::

    cabal v2-build
    cabal v2-test argo file-echo-api

Build tools
~~~~~~~~~~~

Requirements:

* cabal-install 2.4.1.0 or newer
* GHC-8.6.5 or GHC-8.8.4
* Python 3.7 or higher

Any easy to way get GHC and cabal-install installed is to use `ghcup`_;
however any other method will be fine::

    $ ghcup install 8.6.5
    $ ghcup set 8.6.5 # optional
    $ ghcup install-cabal

.. _ghcup: https://gitlab.haskell.org/haskell/ghcup


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
development. The bindings are tested only with Python 3.7 and newer.

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

Actually using the application-specific bindings requires the
appropriate server (please refer to the links at the beginning of this
document).

