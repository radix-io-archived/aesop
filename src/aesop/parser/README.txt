
How to install the necessary Haskell libraries for Aesop:

1) Install cabal

a) Install the easy way:
apt-get install cabal-install

or:
b) Set up a local cabal installation:
./maint/hs/setup-cabal-local

2) Set up the required Haskell libraries
./maint/hs/setup-hs-local

3) Set up Aesop:
./maint/hs/setup-aesop

On a given workstation OS installation, steps 1) and 2) are applied
only once.  Step 3) is applied for each working directory.

On systems without Haskell binary packages (ghc), you'll need to do:

* Download/Build/Install ghc from http://www.haskell.org/ghc/
* Download/Build/Install MTL from http://hackage.haskell.org/cgi-bin/hackage-scripts/package/mtl
* Download/Build/Install happy from http://www.haskell.org/happy/
* Download/Build/Install alex from http://www.haskell.org/alex/

* Then run: setup-aesop

NOTE: Cabal is the recommended method.
