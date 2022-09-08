# haskell

Container image for Haskell

## Usage

```bash
W=/workspace
# Create container
COMPILE="stack ghc -- -O -j2 +RTS -A128m -n2m -RTS --make -v0 -outputdir /tmp -isrc:test test/Main.hs -o tests"
C=$(docker container create --rm -w $W $IMAGE sh -c "$COMPILE && exec ./tests")

# Copy files from the examples directory
docker container cp ./examples/passing/. $C:$W

# Start
docker container start --attach $C
```

## Building

```bash
docker build -t ghcr.io/codewars/haskell:latest .
```
