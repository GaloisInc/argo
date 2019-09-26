Running the Python bindings
---------------------------

First, initialize a virtual environment. This is roughly akin to Cabal's prior sandbox feature. Use this command::

    python3 -m venv env

Next, install the appropriate dependencies in the virtual environment::

    ./env/bin/pip install -r requirements.txt

The ``bin`` directory in the virtual environment contains versions of Python, pip, and mypy that point at the appropriate tools. They can be executed directly from the ``bin`` directory, if you'd like. To place these on your path, you can alternatively enter the environment::

    . ./env/bin/activate

(or the platform- and shell-specific version if you're not using ``bash`` or ``zsh`` on a Unix-like system)

To return to an ordinary user's perspective, use the ``deactivate`` command, which is defined as a shell alias by the ``activate`` script.

