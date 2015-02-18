#What is this?

A machine learning connect four bot for the ACM connect four tournament


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
