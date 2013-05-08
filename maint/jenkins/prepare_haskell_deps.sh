#!/bin/sh


#------------------------------------------------------------
# check if we have cabal & Haskell Libs
#------------------------------------------------------------

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


