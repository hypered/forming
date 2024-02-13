#! /usr/bin/env bash

runghc -i../design -i../syntactical -isrc/ bin/add.hs "$@"
