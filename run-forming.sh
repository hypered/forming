#! /usr/bin/env bash

export FORMING_SITE_DIR=../design
runghc \
  -i../design \
  -i../syntactical \
  -isrc \
  -XImportQualifiedPost \
  -XNoImplicitPrelude \
  -XOverloadedStrings \
  -XRecordWildCards \
  -XTypeApplications \
  -XTypeOperators \
  -Wall \
  bin/forming.hs \
  "$@"
