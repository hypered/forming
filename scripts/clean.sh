#! /usr/bin/env bash

rm -rf .hpc coverage tix bin/forming bin/forming-examples bin/run-tests bin/add
find . -type f -name '*.tix' -delete
find . -type f -name '*.hi' -delete
find . -type f -name '*.o' -delete
find . -type f -name '*.dyn_hi' -delete
find . -type f -name '*.dyn_o' -delete
find ../design/ -type f -name '*.hi' -delete
find ../design/ -type f -name '*.o' -delete
find ../design/ -type f -name '*.dyn_hi' -delete
find ../design/ -type f -name '*.dyn_o' -delete
