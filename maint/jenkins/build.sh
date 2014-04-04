#!/bin/sh

set

# get root
SRC_ROOT=$PWD

BUILD_ROOT=$PWD/build
INSTALL_ROOT=$PWD/install

CONFIGURE_OPTS=

PROPS=$BUILD_ROOT/dependencies.sh

#==============================================================

if ! test -d ${BUILD_ROOT} ; then
mkdir $BUILD_ROOT || exit 2
fi
if ! test -d ${INSTALL_ROOT} ; then
mkdir $INSTALL_ROOT || exit 2
fi

#==============================================================
#==== AESOP Dependencies ======================================
#==============================================================

./maint/jenkins/prepare_environment.sh $PROPS

source $PROPS

CONFIGURE_OPTS="${CONFIGURE_OPTS} --with-libev=${AESOP_LIBEV}"
CONFIGURE_OPTS="${CONFIGURE_OPTS} --with-openpa=${AESOP_OPENPA}"
CONFIGURE_OPTS="${CONFIGURE_OPTS} --with-c-utils=${AESOP_CUTILS}"

export LD_LIBRARY_PATH=${AESOP_CUTILS}/lib:${AESOP_LIBEV}/lib:${AESOP_OPENPA}/lib:$LD_LIBRARY_PATH


#===============================================================
# Build AESOP
#===============================================================

# Configure Aesop
CONFIGURE_OPTS="${CONFIGURE_OPTS} --prefix=${INSTALL_ROOT}"

# Generate build system files
./prepare || exit 2
BUILD_DIR=${SRC_ROOT}    # no-VPATH
BUILD_DIR=${BUILD_ROOT}  # VPATH

cd ${BUILD_ROOT}
${SRC_ROOT}/configure ${CONFIGURE_OPTS}

# make
make || exit 3

# check
make check || exit 4

# Archive bin build for dependencies
tar -cvjf bindist.tar.bz2 install || exit 5

