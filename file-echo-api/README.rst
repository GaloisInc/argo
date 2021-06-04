file-echo-api
=============

Fundamental Protocol
--------------------

This application is a `JSON-RPC <https://www.jsonrpc.org/specification>`_ server. Additionally, it maintains a persistent cache of application states and explicitly indicates the state in which each command is to be carried out.

Transport
~~~~~~~~~

The server supports three transport methods:


``stdio``
  in which the server communicates over ``stdin`` and ``stdout``
  
  

Socket
  in which the server communicates over ``stdin`` and ``stdout``
  
  

HTTP
  in which the server communicates over HTTP
  
  
In both ``stdio`` and socket mode, messages are delimited using `netstrings. <http://cr.yp.to/proto/netstrings.txt>`_


Application State
~~~~~~~~~~~~~~~~~

According to the JSON-RPC specification, the ``params`` field in a message object must be an array or object. In this protocol, it is always an object. While each message may specify its own arguments, every message has a parameter field named ``state``.

When the first message is sent from the client to the server, the ``state`` parameter should be initialized to the JSON null value ``null``. Replies from the server may contain a new state that should be used in subsequent requests, so that state changes executed by the request are visible. Prior versions of this protocol represented the initial state as the empty array ``[]``, but this is now deprecated and will be removed.

In particular, per JSON-RPC, non-error replies are always a JSON object that contains a ``result`` field. The result field always contains an ``answer`` field and a ``state`` field, as well as ``stdout`` and ``stderr``.


``answer``
  The value returned as a response to the request (the precise contents depend on which request was sent)
  
  

``state``
  The state, to be sent in subsequent requests. If the server did not modify its state in response to the command, then this state may be the same as the one sent by the client.
  
  

``stdout`` and ``stderr``
  These fields contain the contents of the Unix ``stdout`` and ``stderr`` file descriptors. They are intended as a stopgap measure for clients who are still in the process of obtaining structured information from the libraries on which they depend, so that information is not completely lost to users. However, the server may or may not cache this information and resend it. Applications are encouraged to used structured data and send it deliberately as the answer.
  
  
The precise structure of states is considered an implementation detail that could change at any time. Please treat them as opaque tokens that may be saved and re-used within a given server process, but not created by the client directly.



A sample server that demonstrates filesystem caching.

Datatypes
---------

.. _Ignorable:
Ignorable data
~~~~~~~~~~~~~~

Data to be ignored can take one of three forms:


``true``
  The first ignorable value
  
  

``false``
  The second ignorable value
  
  

``null``
  The ultimate ignorable value, neither true nor false
  
  
Nothing else may be ignored.



Methods
-------

load (command)
~~~~~~~~~~~~~~

Load a file from disk into memory.

Parameter fields
++++++++++++++++


``file path``
  The file to read into memory.
  
  

Return fields
+++++++++++++

No return fields



clear (command)
~~~~~~~~~~~~~~~

Forget the loaded file.

Parameter fields
++++++++++++++++

No parameters


Return fields
+++++++++++++

No return fields



prepend (command)
~~~~~~~~~~~~~~~~~

Append a string to the left of the current contents.

Parameter fields
++++++++++++++++


``content``
  The string to append to the left of the current file content on the server.
  
  

Return fields
+++++++++++++

No return fields



drop (command)
~~~~~~~~~~~~~~

Drop from the left of the current contents.

Parameter fields
++++++++++++++++


``count``
  The number of characters to drop from the left of the current file content on the server.
  
  

Return fields
+++++++++++++

No return fields



implode (query)
~~~~~~~~~~~~~~~

Throw an error immediately.

Parameter fields
++++++++++++++++

No parameters


Return fields
+++++++++++++

No return fields



show (query)
~~~~~~~~~~~~

Show a substring of the file.

Parameter fields
++++++++++++++++


``start``
  Start index (inclusive). If not provided, the substring is from the beginning of the file.
  
  

``end``
  End index (exclusive). If not provided, the remainder of the file is returned.
  
  

Return fields
+++++++++++++


``value``
  The substring ranging from ``start`` to ``end``.
  
  


ignore (query)
~~~~~~~~~~~~~~

Ignore an :ref:`ignorable value <Ignorable>`.

Parameter fields
++++++++++++++++


``to be ignored``
  The value to be ignored goes here.
  
  

Return fields
+++++++++++++

No return fields



destroy state (notification)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Destroy a state in the server.

Parameter fields
++++++++++++++++


``state to destroy``
  The state to destroy in the server (so it can be released from memory).
  
  

Return fields
+++++++++++++

No return fields



destroy all states (notification)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Destroy all states in the server.

Parameter fields
++++++++++++++++

No parameters


Return fields
+++++++++++++

No return fields






