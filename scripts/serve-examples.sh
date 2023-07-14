#! /usr/bin/env bash

echo Visit http://127.0.0.1:8000/noteed/.
echo

export FORMING_SITE_DIR=../design
runghc -i../design/src -i../syntactical \
  -XOverloadedStrings \
  -XTypeApplications \
  bin/forming-examples.hs serve
