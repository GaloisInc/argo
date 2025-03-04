Overview
========

This package contains servers that wrap the functionality of Cryptol and
SAW within a JSON-RPC API, along with a Python client library for
convenient interaction with that API.

Environment Setup
=================

The package contains two directories that external tools need to be able
to find: `bin` and `python`. The former contains the server executables,
and the latter contains Python code to make it convenient to interact
with the servers.

To make these accessible, the easiest approach is to set the `PATH`
environment variable to include the `bin` directory, wherever you unpack
it, and set the `PYTHONPATH` environment variable to point to

Dependencies
============

The server executables are standalone and dynamically link to only a few
core system libraries. The Python code requires Python 3.7 or newer, and
the following extra packages:

- BitVector==3.4.9
- mypy==0.730

You can install these either globally or within a Python `virtualenv`
containter using

    `pip install -r python/requirements.txt`.

If you set `PYTHONPATH` as described above, the Python interpreter will
be able to find the client library code. Alternatively, you can install
the code for the Python client libraries, also either globally or within
a `virtualenv`, using

    `pip install -e python/`

Running Examples
================

Once you have installed the necessary dependencies and set up your
environment variables as described above, the scripts in the `examples`
directory should be directly runnable. The `.py` files in that directory
are executable and have an interpreter directive to tell the shell to
run them with `python3`.

When the verification scripts run, they will print a URL which will show
you a graphical indication of verification status as it progresses.  For
this to work, the Graphviz `dot` program must be in your `PATH`.
