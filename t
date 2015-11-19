#!/bin/sh

stack install jstype:jstype-repl >&2 && test262/tools/packaging/test262.py --tests=test262 --command="$HOME/.local/bin/jstype-repl" --unmarked_default=non_strict "$@"
