================
SAW Verification
================

Note: The SAW API is still in flux and is not yet fully documented.

As with the Cryptol methods, server state is propagated as described in the
protocol overview.

Methods implemented against the SAW API may throw :ref:`a variety of SAW-related
errors <saw-server-errors>`, with codes in the range of ``10000``-``19999``.
Additionally, SAW verification relies on Cryptol, and some SAW methods may throw
:ref:`Cryptol server errors <cryptol-server-errors>` when appropriate.

Cryptol Module Management
=========================

Loading Modules
---------------

:Method name:
  ``SAW/Cryptol/load module``
:Parameters:
  - ``module name``: The name of the Cryptol module to be loaded.

Loading Files
-------------

:Method name:
  ``SAW/Cryptol/load file``
:Parameters:
  - ``file``: The name of the Cryptol source file to be loaded.

Saving Terms
------------

:Method name:
  ``SAW/Crpytol/save term``
:Parameters:
  - ``name``: The name to bind the value of ``expression`` to on the server.
  - ``expression``: The Cryptol expression to bind the value of ``name`` to on the server.

JVM Verification
================

Loading Classes
---------------

:Method name:
  ``SAW/JVM/load class``
:Parameters:
  - ``name``: The name to bind the loaded class to on the server.
  - ``class``: The name of the class to load and bind the value of ``name`` to on the server.

Verifying
---------

:Method name:
  ``SAW/JVM/verify``
:Parameters:
  - ``module``: The name of the (previously loaded) *class* containing the function/method to verify.
  - ``function``: The name of the function/method to verify.
  - ``lemmas``: A list containing the names of previously proved lemmas to be used in compositional verification.
  - ``check sat``: A Boolean value indicating whether or not to perform path satisfiability checking.
  - ``contract``: The specification to perform verification against.
  - ``script``: The proof script to use for verification.
  - ``lemma name``: The name to bind the result of verification to on the server.

Assuming
--------

:Method name:
  ``SAW/JVM/assume``
:Parameters:
  - ``module``: The name of the (previously loaded) *class* containing the function/method to assume verified.
  - ``function``: The name of the function/method to assume verified.
  - ``contract``: The specification to assume for the given function/method.
  - ``lemma name``: The name to bind the result of verification to on the server.

LLVM Verification
=================

Loading Modules
---------------

:Method name:
  ``SAW/LLVM/load module``
:Parameters:
  - ``name``: The name to bind the loaded bitcode file to on the server.
  - ``bitcode file``: The path to the bitcode file to load and bind to ``name`` on the server.

Verifying (General)
-------------------

:Method name:
  ``SAW/LLVM/verify``
:Parameters:
  - ``module``: The name of the (previously loaded) module containing the function to verify.
  - ``function``: The name of the function to verify.
  - ``lemmas``: A list containing the names of previously proved lemmas to be used in compositional verification.
  - ``check sat``: A Boolean value indicating whether or not to perform path satisfiability checking.
  - ``contract``: The specification to perform verification against.
  - ``script``: The proof script to use for verification.
  - ``lemma name``: The name to bind the result of verification to on the server.

Verifying (x86)
---------------

:Method name:
  ``SAW/LLVM/verify x86``
:Parameters:
  - ``module``: The name of the (previously loaded) module containing the function to verify.
  - ``object file``: The path to the x86 object file containing the function to verify.
  - ``function``: The name of the function to verify.
  - ``globals``: A list containing the global allocations needed for the verification task.
  - ``lemmas``: A list containing the names of previously proved lemmas to be used in compositional verification.
  - ``check sat``: A Boolean value indicating whether or not to perform path satisfiability checking.
  - ``contract``: The specification to perform verification against.
  - ``script``: The proof script to use for verification.
  - ``lemma name``: The name to bind the result of verification to on the server.

Assuming
--------

:Method name:
  ``SAW/LLVM/assume``
:Parameters:
  - ``module``: The name of the (previously loaded) *class* containing the function/method to assume verified.
  - ``function``: The name of the function/method to assume verified.
  - ``contract``: The specification to assume for the given function/method.
  - ``lemma name``: The name to bind the result of verification to on the server.

Proof Management
================
