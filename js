#!/usr/bin/env bash

cabal exec runhaskell -- -isrc -irepl Main "$@"
