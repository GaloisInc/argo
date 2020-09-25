Python Bindings
===============

General-Purpose
---------------

Module ``argo.connection``
~~~~~~~~~~~~~~~~~~~~~~~~~~

This module contains utilities that are useful for connecting to servers that implement the Argo protocols. To interact with a server, a process and a connection are necessary. The variants of processes are used to manage the different transport layers that are available, such as sockets or pipes, as well as server process lifecycles. They are:

  :class:`argo.connection.DynamicSocketProcess`
    This is used to start a subprocess and communicate with it over a socket on a port chosen by the server itself.

  :class:`argo.connection.RemoteSocketProcess`
    This is used to connect to a socket on a pre-existing server process, potentially on another machine

  :class:`argo.connection.StdIOProcess`
    This is used to manage a server subprocess, communicating with it over Unix pipes.

The other necessary component is a :class:`argo.connection.ServerConnection`. While a process manages the underlying transport layer, the connection object tracks the logical state of the connection itself, encoding protocol details that are invariant with respect to the transport method. This includes mapping request IDs to their responses and JSON (de)serialization.

.. automodule:: argo.connection
   :members:
   :special-members:


Module ``argo.interaction``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. automodule:: argo.interaction
   :members:
   :special-members:

Module ``argo.netstring``
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. automodule:: argo.netstring
   :members:
   :special-members:


SAW
---

Cryptol
-------


Module ``cryptol``
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. automodule:: cryptol

.. autofunction:: cryptol.connect


.. autoclass:: cryptol.CryptolConnection
   :members:
   :special-members:



Module ``cryptol.cryptoltypes``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. automodule:: cryptol.cryptoltypes
   :members:
   :special-members:
