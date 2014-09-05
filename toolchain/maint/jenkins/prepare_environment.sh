#!/bin/sh

echo "========================================================="
echo "=== Preparing aesop build environment ==================="
echo "========================================================="

set

PROPS=$1

echo "# Aesop build environment properties $(date)" > $PROPS

echo "==========================================="
echo "========== Installing libev ==============="
echo "==========================================="
AESOP_LIBEV=$(mktemp --tmpdir -d aesop-ev-XXXXXX)
maint/jenkins/install-libev.sh ${AESOP_LIBEV} || exit 1
echo "AESOP_LIBEV=\"$AESOP_LIBEV\"" >> $PROPS


echo "==========================================="
echo "========== Installing c-utils ============="
echo "==========================================="
AESOP_CUTILS=$(mktemp --tmpdir -d aesop-cu-XXXXXX)
maint/jenkins/install-c-utils.sh ${AESOP_CUTILS} || exit 1
echo "AESOP_CUTILS=\"$AESOP_CUTILS\"" >> $PROPS


echo "==========================================="
echo "========== Installing openpa -============="
echo "==========================================="
AESOP_OPENPA=$(mktemp --tmpdir -d aesop-openpa-XXXXXX)
maint/jenkins/install-openpa.sh ${AESOP_OPENPA} || exit 1
echo "AESOP_OPENPA=\"$AESOP_OPENPA\"" >> $PROPS

echo "==========================================="
echo "========== Installing haskell ============="
echo "==========================================="
./maint/jenkins/prepare_haskell_deps.sh || exit 1



