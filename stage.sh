#!/bin/bash

set -e

CDIR=dist/cryptol-saw-remote-api
DATE=$(date "+%Y-%m-%d")
CRA=$(cabal v2-exec which cryptol-remote-api)
SRA=$(cabal v2-exec which saw-remote-api)

mkdir -p ${CDIR}
mkdir -p ${CDIR}/bin
mkdir -p ${CDIR}/doc
mkdir -p ${CDIR}/python
mkdir -p ${CDIR}/examples

cp ${CRA} ${CDIR}/bin
cp ${SRA} ${CDIR}/bin
cp docs/*.rst ${CDIR}/doc
cp -r python ${CDIR}
cp -r examples ${CDIR}
rm -rf ${CDIR}/python/.stack-work
cp README-dist.rst ${CDIR}/README.rst

cd dist
tar -czvf cryptol-saw-remote-api-${DATE}.tar.gz cryptol-saw-remote-api
