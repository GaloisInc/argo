# file-echo-api

A simple example usage of Argo: a JSON-RPC "file echo server" which can load files on disk and send their contents back to the client.

## file-echo-api commands

+ `load`, loads a file into the echo server's memory
  - Parameters
    * `file path` : file path as a string, describing which file to load

+ `clear`, clears any loaded file from the server's memory
  - No parameters

## file-echo-api queries

+ `show`, returns the contents of the last loaded file
  - No Required Parameters
  - Optional Parameters
    * `start` : character index in loaded file to begin showing from, default value `0`
    * `end` : character index to show up until (but not including), default value is the character length of the currently loaded file minus the `start` parameter


# Files

+ `src/FileEchoServer.hs` implements the internals of the server
+ `file-echo-api/Main.hs` defines an Argo executable leveraging the definitions from `FileEchoServer.hs`
+ `test/Test.hs` a Haskell test runner which executes the python script `file-echo-tests.py`
+ `test-scripts/file-echo-tests.py` is a python script which leverages the `argo` python library to test the file echo server
