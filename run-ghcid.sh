#! /usr/bin/env bash

ghcid \
  --command "ghci -i../design-system bin/run-tests.hs" \
  --test ":main --color always"
