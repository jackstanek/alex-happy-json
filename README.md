# Alex/Happy JSON Parser

This repo contains a simple, minimalistic Haskell JSON parser written using
Alex and Happy, mostly as an exercise to learn how these tools work together.
The parser utilizes the monadic interfaces to Alex and Happy.

## Compiling and running

This project can be built easily with `cabal`:

```text
$ cabal v2-update && cabal v2-run
```

This will launch a REPL which will parse JSON expressions.
