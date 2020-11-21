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

Making Simpsets
---------------

:Method name:
  ``SAW/make simpset``
:Parameters:
  - ``elements``: A list of names bound to terms to add to the simpset.
  - ``result``: The name to bind the simpset to on the server.

Running Proof Scripts
---------------------

:Method name:
  ``SAW/prove``
:Parameters:
  - ``script``: The proof script to run.
  - ``term``: The term to run the proof script against.
:Return fields:
  - ``status``: A string (either ``valid`` or ``invalid``) indicating whether the proof went through successfully or not.

Setting Options
---------------

:Method name:
  ``SAW/set option``
:Parameters:
  - ``option``: The name of the option to set. This is one of:

    * ``lax arithmetic``
    * ``SMT array memory model``
    * ``What4 hash consing``

  - ``value``: A Boolean value indicating whether to enable/disable the feature named by ``option``.

Specifications
==============

SAW verification relies on the provision of specifications to verify against. In the API,
these specifications are represented by a JSON object with the following fields:

``pre vars``
  A list of symbolic variables introduced in the initial state section of the specification. These variables
  are represented by a JSON object containing three fields:

  - ``server name``: The name of the variable on the server.
  - ``name``: The "display name" of the variable, used in debugging output.
  - ``type``: The LLVM or JVM type of this variable.

``pre conds``
  A list of the specification's preconditions, as Cryptol expressions.

``pre allocated``
  A list of allocations in the initial state section of the specification. In preconditions,
  allocations specify that the function being verified expects a pointer to the allocated memory
  to exist. An allocation is a JSON object containing four fields, one of which is optional:

  - ``server name``: The name by which the allocation is referred to on the server.
  - ``type``: The LLVM or JVM type of the data for which space is being allocated.
  - ``mutable``: A Boolean value indicating whether the allocated memory is mutable or not.
  - ``alignment``: An integer value indicating where the start of the allocated memory should
    be aligned. This value must be a power of two, and the allocated memory may be aligned at
    any multiple of it. The field *must* be ``null`` in JVM specifications, and *may* be ``null``
    in LLVM specifications.

``pre points to``
  A list of 'points-to' relationships in the initial state section of the specification. These
  relationships are captured in a JSON object containing two fields:

  - ``pointer``: A Crucible Setup value representing the pointer.
  - ``points to``: A Crucible Setup value representing the referent of ``pointer``.

``argument vals``
A list of Crucible Setup values representing the arguments to the function being verified.

``post vars``
A list of variables in the final state section of the specification. While in many cases this
list will be empty, it is sometimes useful to specify that functions return arbitrary values.
These variables are represented in the same way as those under ``pre vars`` above.

``post conds``
A list of the specification's postconditions, as Cryptol expressions.

``post allocated``
A list of allocations in the final state section of the specification. In postconditions,
allocations specify that the function being verified allocated memory. An allocation is
represented in the same was as under ``pre conds`` above.

``post points tos``
A list of 'points-to' relationships in the final state section of the specification. These
relationships are represented in the same was as under ``pre points to`` above.


``return val``
An optional Crucible Setup value specifying the expected return value of the function being verified.
