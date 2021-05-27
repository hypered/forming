#! /usr/bin/env bash

export FORMING_SITE_DIR=../design-system
runghc -i../design-system -i../syntactical bin/forming.hs "$@"
