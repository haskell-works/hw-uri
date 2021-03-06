#!/usr/bin/env bash

cabal_cache_exe="$1"
archive="$2"

for x in "$archive"/v1/6d91da2ce3/ghc-8.6.4/*; do
  echo "====== $(basename $x) ======"
  rm -rf ~/.cabal/store/ghc-8.6.4
  mkdir -p "$archive"-2
  cp "$archive"/v1/6d91da2ce3/ghc-8.6.4/* "$archive"-2/v1/6d91da2ce3/ghc-8.6.4/
  rm "$archive"-2/v1/6d91da2ce3/ghc-8.6.4/$(basename $x)
  "$cabal_cache_exe" sync-from-archive --threads 16 --archive-uri "$archive"-2 --region Sydney > /dev/null 2> /dev/null
  ./project.sh build
done
