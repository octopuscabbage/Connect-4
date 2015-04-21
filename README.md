#What is this?

A connect four bot for the AIMania ACM AI Contest. Uses standard minMax 2 player algorithm.

#Using this program

You need to build this with cabal

```
sudo apt-get install cabal
```

then probably make a sandbox

```
cabal sandbox init
```

then install the dependencies

```
cabal install
```

then compile and run

```
cabal run
```

#Documentation

When documenting functions please use haddock style documentation

for example, write

```
-- |This function does this
```
instead of just

```
-- This function does this
```
