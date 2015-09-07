# hs-ifstat

Something like `ifstat(1)` but different.  Measures traffic on a network interface.  So very, *very* underdone.

## Building
```shell
git clone https://github.com/nathaningle/hs-ifstat.git
cd hs-ifstat
cabal sandbox init
cabal install --dependencies-only
cabal build
```
The `ifstat` binary should then be in `dist/build/ifstat`.  You will probably need root permissions to execute it.
