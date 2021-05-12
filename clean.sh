#! /usr/bin/env bash

rm -rf .hpc coverage tix bin/forming-examples bin/run-tests
find . -type f -name '*.tix' -delete
find . -type f -name '*.hi' -delete
find . -type f -name '*.o' -delete
find . -type f -name '*.dyn_hi' -delete
find . -type f -name '*.dyn_o' -delete
find ../design-system/ -type f -name '*.hi' -delete
find ../design-system/ -type f -name '*.o' -delete
find ../design-system/ -type f -name '*.dyn_hi' -delete
find ../design-system/ -type f -name '*.dyn_o' -delete
