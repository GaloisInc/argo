``file-echo-api``
=================

A simple example usage of Argo: a JSON-RPC "file echo server" which can load files on disk and send their contents back to the client.

Commands
--------------------------

* ``load``

  * Loads a file into the echo server's memory.  
  * Parameters

    * ``file path : String``
  
      * File path describing which file to load.

* ``clear``
  
  * Clears any loaded file from the server's memory.
  * No parameters

* ``implode``
  
  * Causes the server to raise an internal error (for testing purposes).
  * No parameters

Queries
-------------------------

* ``show``
  
  * Returns the contents of the last loaded file.

  * No required parameters
  * Optional parameters

    * ``start : Integer``
  
      * Character index in loaded file to begin showing from, default value ``0``.
  
    * ``end : Integer``
  
      * Character index to show up until (but not including), default value is the character length of the currently loaded file minus the ``start`` parameter's value.


Files
-----

* ``src/FileEchoServer.hs`` implements the internals of the server.
* ``file-echo-api/Main.hs`` defines an Argo executable leveraging the definitions from ``FileEchoServer.hs``.
* ``test/Test.hs`` a Haskell test runner which executes the python script ``file-echo-tests.py``.
* ``test-scripts/file-echo-tests.py`` is a python script which leverages the ``argo`` python library to test the file echo server.
