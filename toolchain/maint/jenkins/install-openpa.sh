#!/bin/sh

VERSION=1.0.4
FILE=openpa-${VERSION}.tar.gz

PREFIX=$1
if test -z $PREFIX; then
   echo "Need destination prefix!" > /dev/stderr
   exit 1
fi

TMPDIR=$(mktemp -d)
pushd $TMPDIR
wget -c http://trac.mcs.anl.gov/projects/openpa/raw-attachment/wiki/Downloads/${FILE}
tar -xvzf $FILE
cd openpa-${VERSION}
./configure --prefix=${PREFIX} && make && make install || exit 1
popd


