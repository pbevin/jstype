# Javascript Type Inferencer

## Setup

```bash
cabal sandbox init
cabal configure --enable-tests
cabal install --only-dependencies --enable-tests
```

## Getting a REPL

```
$ cabal run repl
```

## Tests

Tests are written using Hspec.

### Single Run

```bash
cabal test
```

### Automatic Tests

For this you need Ruby and the `guard-shell` gem installed.

```bash
guard
```

Now edit and save a test or a source file and the tests will run automatically.
