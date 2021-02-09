#!/bin/bash

set -x

cabal run file-echo-api --verbose=0 -- socket --port 50005 &
cabal run file-echo-api --verbose=0 -- http / --port 8080 &
