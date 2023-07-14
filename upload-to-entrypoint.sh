#! /usr/bin/env bash

set -e

# Generate files into generated/
runghc -i../design/src bin/add.hs --html

# Copy file to entrypoint
mkdir -p ../entrypoint/as-is/forming/generated/
mkdir -p ../entrypoint/as-is/forming/static/
rsync -aP generated/ ../entrypoint/as-is/forming/generated/
rsync -aP ../design/static/ ../entrypoint/as-is/forming/static/

# Let browser-sync know about new files
find ../entrypoint/as-is/forming/generated/ -exec chmod u+w {} \; -exec touch {} \;
