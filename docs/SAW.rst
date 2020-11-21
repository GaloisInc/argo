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
  - ``contract``: The :ref:`specification<specifications>` to perform verification against.
  - ``script``: The :ref:`proof script<proof-scripts>` to use for verification.
  - ``lemma name``: The name to bind the result of verification to on the server.

Assuming
--------

:Method name:
  ``SAW/JVM/assume``
:Parameters:
  - ``module``: The name of the (previously loaded) *class* containing the function/method to assume verified.
  - ``function``: The name of the function/method to assume verified.
  - ``contract``: The :ref:`specification<specifications>` to assume for the given function/method.
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
  - ``contract``: The :ref:`specification<specifications>` to perform verification against.
  - ``script``: The :ref:`proof script<proof-scripts>` to use for verification.
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
  - ``contract``: The :ref:`specification<specifications>` to perform verification against.
  - ``script``: The :ref:`proof script<proof-scripts>` to use for verification.
  - ``lemma name``: The name to bind the result of verification to on the server.

Assuming
--------

:Method name:
  ``SAW/LLVM/assume``
:Parameters:
  - ``module``: The name of the (previously loaded) *class* containing the function/method to assume verified.
  - ``function``: The name of the function/method to assume verified.
  - ``contract``: The :ref:`specification<specifications>` to assume for the given function/method.
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
  - ``script``: The :ref:`proof script<proof-scripts>` to run.
  - ``term``: The name of a term bound on the server to run the proof script against.
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

.. _specifications:

Specifications
==============

SAW verification relies on the provision of specifications to verify against. In the API,
these specifications are represented by a JSON object with the following fields:

``pre vars``
  A list of symbolic variables introduced in the initial state section of the specification. These variables
  are represented by a JSON object containing three fields:

.. _contract-vars:

  - ``server name``: The name of the variable on the server.
  - ``name``: The "display name" of the variable, used in debugging output.
  - ``type``: The LLVM or JVM type of this variable.

``pre conds``
  A list of the specification's preconditions, as Cryptol expressions.

``pre allocated``
  A list of allocations in the initial state section of the specification. In preconditions,
  allocations specify that the function being verified expects a pointer to the allocated memory
  to exist. An allocation is a JSON object containing four fields, one of which is optional:

.. _allocation:

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

.. _points-to:

  - ``pointer``: A :ref:`Crucible Setup value<setup-values>` representing the pointer.
  - ``points to``: A :ref:`Crucible Setup value<setup-values>` representing the referent of ``pointer``.

``argument vals``
  A list of :ref:`Crucible Setup values<setup-values>` representing the arguments to the function being verified.

``post vars``
  A list of variables in the final state section of the specification. While in many cases this
  list will be empty, it is sometimes useful to specify that functions return arbitrary values.
  These variables are represented in the same way as :ref:`above<contract-vars>`.

``post conds``
  A list of the specification's postconditions, as Cryptol expressions.

``post allocated``
  A list of allocations in the final state section of the specification. In postconditions,
  allocations specify that the function being verified allocated memory. An allocation is
  represented in the same was as :ref:`above<allocation>`.

``post points tos``
  A list of 'points-to' relationships in the final state section of the specification. These
  relationships are represented in the same was as :ref:`above<points-to>`.


``return val``
  An optional :ref:`Crucible Setup value<setup-values>` specifying the expected return value of the function being verified.

.. _proof-scripts:

Proof Scripts
=============

SAW allows one to direct a verification task using a proof script, which is simply a sequence of proof
tactics to apply. Very commonly, the proof script provided in a verification task is simply an instruction
to use an external SAT/SMT solver such as ABS, Yices, or Z3.

A proof script is represented as a JSON object with a single field:

``tactics``
  A list of proof tactics to apply to the context/goal. A proof tactic is represented as a JSON object
  containing a tag named ``tactic``, with any further fields determined by this tag. These tag values can be:

  ``use prover``
    Apply an external prover to the goal. There is an additional field ``prover``, which is a JSON object
    with a field ``name`` specifying what prover to use (one of ``abc``, ``cvc4``, ``rme``, ``yices``, or ``z3``),
    and a field ``uninterpreted functions`` when ``name`` is one of ``cvc4``, ``yices``, or ``z3``. This
    field is a list of names of functions taken as uninterpreted/abstract.

  ``unfold``
    Unfold terms in the context/goal. There is an additional field ``names``, a list of the names bound on
    the server to unfold.

  ``beta reduce goal``
    Perform a single beta reduction on the proof goal.

  ``evaluate goal``
    Fully evaluate the proof goal. There is an additional field ``uninterpreted functions``, a list of names
    of functions taken as uninterpreted/abstract.

  ``simplify``
    Simplify the context/goal. There is an additional field ``rules``, a name bound to a simpset on the server.

  ``assume unsat``
    Assume the goal is unsatisfiable, which in the current implementation of SAW should be interpreted as
    assuming the property being checked to be true. This is likely to change in the future.

  ``trivial``
    States that the goal should be trivially true (either the constant ``True`` or a function that immediately
    returns ``True``. This tactic fails if that is not the case.

.. _setup-values:

Crucible Setup Values
=====================
