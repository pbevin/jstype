#!/bin/sh

cabal build repl >&2 && test262/tools/packaging/test262.py --tests=test262 --command="dist/build/repl/repl" "$@"
