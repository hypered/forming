#! /usr/bin/env bash

export FORMING_SITE_DIR=../design
runghc -i../design -i../syntactical bin/forming.hs "$@"
