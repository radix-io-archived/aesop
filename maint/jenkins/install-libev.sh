#!/bin/sh

CACHEDIR=/tmp/aesop-download-cache
PREFIX=$1

if test -z $PREFIX; then
   echo "Need destination prefix!" > /dev/stderr
   exit 1
fi

if ! test -d $CACHEDIR ; then
   mkdir -p ${CACHEDIR}
fi

TMPDIR=$(mktemp --tmpdir -d)
pushd $TMPDIR

# be nice to download host
wget -c -O ${CACHEDIR}/libev-4.15.tar.gz http://dist.schmorp.de/libev/libev-4.15.tar.gz
tar -xzf ${CACHEDIR}/libev-4.15.tar.gz
cd libev-4.15
./configure --prefix=$PREFIX && make && make install
popd




