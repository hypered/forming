#! /usr/bin/env bash

ghc --make -i../design-system/ bin/forming-examples.hs
ghc --make -i../design-system/ -i../syntactical/ bin/forming.hs
