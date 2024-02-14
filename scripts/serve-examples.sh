#! /usr/bin/env bash

echo Visit http://127.0.0.1:8000/examples/.
echo

export FORMING_SITE_DIR=_site
export FORMING_SITE_DIR=../design
runghc \
  -i../design/src \
  -i../syntactical \
  -isrc/ \
  -XImportQualifiedPost \
  -XNoImplicitPrelude \
  -XOverloadedStrings \
  -XRecordWildCards \
  -XTypeApplications \
  -XTypeOperators \
  -Wall \
  bin/forming-examples.hs serve
