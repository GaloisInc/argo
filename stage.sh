#!/bin/bash

set -e

CDIR=dist/cryptol-saw-remote-api
BIN=$(stack path --local-install-root)/bin
DATE=$(date "+%Y-%m-%d")

mkdir -p ${CDIR}
mkdir -p ${CDIR}/bin
mkdir -p ${CDIR}/doc
mkdir -p ${CDIR}/python

cp ${BIN}/cryptol-remote-api ${CDIR}/bin
cp ${BIN}/saw-remote-api ${CDIR}/bin
cp docs/*.rst ${CDIR}/doc
cp -r python ${CDIR}

cd dist
tar -czvf cryptol-saw-remote-api-${DATE}.tar.gz cryptol-saw-remote-api
