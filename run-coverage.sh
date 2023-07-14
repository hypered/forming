#! /usr/bin/env bash

# Run our executable in various ways, collecting coverage information. The
# result is saved as HTML pages in the coverage/ directory.
# (In other words, this is not related to tests, but to invokation examples.)

# Compile the programs, with HPC enabled. This generates a .hpc/ directory
# containing .mix files. If the .hi and .o files are not deleted first,
# the corresponding .mix file will be missing, causing errors later when
# invoking `hpc report` or `hpc markup`.
echo "Building bin/forming-examples..."
./scripts/clean.sh
ghc --make -fhpc -i../design/ -i../syntactical/ bin/forming-examples.hs

# With the HPCTIXDIR environment variable set, an HPC-instrumented process
# will use its own .tix file. This is necessary here as Intake is run
# concurrently with itself: a server and multiple clients.
# `hpc combine` can then be used to aggregate the .tix files in a single
# new .tix file, used as usual with `hpc report`, `hpc markup` or
# `covered markup`.
# That directory is automatically created by the instrumented executable.
export HPCTIXDIR=tix

# Run the code in various ways.
# TODO I can't run run-tests because it seems it is a different Main and it
# clashes with forming-examples' Main. Anyway test coverage and example
# coverage are two different things.
echo "Running bin/forming-examples..."
bin/forming-examples > /dev/null 2>&1
bin/forming-examples --help > /dev/null 2>&1
bin/forming-examples trivial-1 > /dev/null 2>&1
bin/forming-examples trivial-a > /dev/null 2>&1
bin/forming-examples trivial-a --help > /dev/null 2>&1
bin/forming-examples trivial-a --html > /dev/null 2>&1
bin/forming-examples trivial-a --list > /dev/null 2>&1
bin/forming-examples trivial-a --unset > /dev/null 2>&1
bin/forming-examples trivial-a --unset a > /dev/null 2>&1
bin/forming-examples trivial-a a > /dev/null 2>&1
bin/forming-examples trivial-a --set > /dev/null 2>&1
bin/forming-examples trivial-a --set a > /dev/null 2>&1
bin/forming-examples trivial-a --set a True > /dev/null 2>&1
bin/forming-examples trivial-a --set a False > /dev/null 2>&1
bin/forming-examples trivial-a --set a 1 > /dev/null 2>&1
bin/forming-examples trivial-a --set a "x" > /dev/null 2>&1
bin/forming-examples trivial-a --json '{' > /dev/null 2>&1
bin/forming-examples trivial-a --json '1' > /dev/null 2>&1
bin/forming-examples trivial-a --json '{"a": true}' > /dev/null 2>&1
bin/forming-examples trivial-a --json '{"a": 1}' > /dev/null 2>&1
bin/forming-examples trivial-a --json '{"a": "x"}' > /dev/null 2>&1
bin/forming-examples annotate-a > /dev/null 2>&1
bin/forming-examples annotate-a --set a 1 > /dev/null 2>&1
bin/forming-examples annotate-enum > /dev/null 2>&1
bin/forming-examples add > /dev/null 2>&1
bin/forming-examples greater-than-10 > /dev/null 2>&1
bin/forming-examples add-greater-than-10 > /dev/null 2>&1
bin/forming-examples assert-greater-than-10 > /dev/null 2>&1
bin/forming-examples has-a-cat > /dev/null 2>&1
bin/forming-examples compensation > /dev/null 2>&1

# Combine all the .tix files in a single forming.tix file.
TIX=$(ls tix/*.tix | head -n 1)
cp $TIX forming.tix
TIXS=$(ls tix/*.tix | tail -n +2)
for i in $TIXS ; do
  hpc combine --union $i forming.tix --output new.tix
  mv new.tix forming.tix
done

mkdir -p coverage
echo "HPC Report:"
hpc report forming.tix --hpcdir .hpc
echo "Writing HPC HTML report to coverage/..."
hpc markup forming.tix --hpcdir .hpc --destdir coverage

# TODO Use covered.
#covered markup --hpcdir dist/hpc/mix/forming-0.0.0/ forming
rm forming.tix
rm -rf tix

# TODO Use haddock.
#cabal haddock --internal --hyperlink-source
