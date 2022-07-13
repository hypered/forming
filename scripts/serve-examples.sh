#! /usr/bin/env bash

echo Visit http://127.0.0.1:8000/noteed/.
echo

export FORMING_SITE_DIR=../design-system
runghc -i../design-system bin/forming-examples.hs --serve
