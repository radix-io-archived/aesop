#!/bin/sh

REF=master

PREFIX=$1
if test -z $PREFIX; then
   echo "Need destination prefix!" > /dev/stderr
   exit 1
fi

TMPDIR=$(mktemp -d)
git clone -b ${REF} git://git.mcs.anl.gov/c-utils $TMPDIR
pushd $TMPDIR
./prepare
./configure --disable-shared --prefix=${PREFIX} && make && make install || exit 1
popd


