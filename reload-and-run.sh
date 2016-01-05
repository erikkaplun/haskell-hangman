#!/usr/bin/env sh

files=$(find app src -type f -and -name "*.hs")

ghc_cmd="stack ghc -- --make $files"

$ghc_cmd && app/Main $@
