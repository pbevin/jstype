#!/usr/bin/env bash

cabal exec runhaskell -- -isrc -itest Main.hs "$@"
