#!/bin/sh

echo "Some info:"
date
ls
set
echo "============"

# get root
SRC_ROOT=$PWD

BUILD_ROOT=$PWD/build
INSTALL_ROOT=$PWD/install

if ! test -d ${BUILD_ROOT} ; then
mkdir $BUILD_ROOT || exit 2
fi
if ! test -d ${INSTALL_ROOT} ; then
mkdir $INSTALL_ROOT || exit 2
fi


# Generate build system files
./prepare || exit 2

# check if we have cabal
CABAL=$(which cabal)
if test -z $CABAL; then
   ./maint/hs/setup-cabal-local || exit 3
   export PATH=$PATH:$HOME/.cabal/bin
   CABAL=$(which cabal)
   if test -z $CABAL; then
      echo "Error installing cabal!" > /dev/stderr
      exit 3
   fi
fi

# Get HS packages
./maint/hs/setup-hs-local || exit 4

# Get modified Language.C
./maint/hs/setup-aesop || exit 5

# Install OpenPA
OPENPA=${BUILD_ROOT}/openpa
./maint/jenkins/install-openpa.sh $OPENPA || exit 6
CONFIGURE_OPTS="${CONFIGURE_OPTS} --with-openpa=${OPENPA}"

# Configure

CONFIGURE_OPTS="${CONFIGURE_OPTS} --prefix=${INSTALL_ROOT}"

BUILD_DIR=${SRC_ROOT}    # no-VPATH
BUILD_DIR=${BUILD_ROOT}  # VPATH

cd ${BUILD_ROOT}
${SRC_ROOT}/configure ${CONFIGURE_OPTS}

# make
make

