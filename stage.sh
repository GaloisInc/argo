#!/bin/bash

set -e

D=dist/cryptol-remote-api
BIN=$(stack path --local-install-root)/bin

mkdir -p ${D}
mkdir -p ${D}/bin
mkdir -p ${D}/doc
mkdir -p ${D}/python

cp ${BIN}/cryptol-remote-api ${D}/bin
cp docs/*.rst ${D}/doc
cp -r python/cryptol* ${D}/python
cp -r python/Foo.cry ${D}/python

cd dist
tar -czvf cryptol-remote-api.tar.gz cryptol-remote-api
