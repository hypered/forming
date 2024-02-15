#! /usr/bin/env bash

runghc -i../design -i../syntactical -isrc/ -itests/ \
  -XImportQualifiedPost \
  -XNoImplicitPrelude \
  -XOverloadedStrings \
  -XRecordWildCards \
  -XTypeApplications \
  -XTypeOperators \
  -Wall \
  tests/run-tests.hs --hide-successes
