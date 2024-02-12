#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# We use a ghci.conf file instead of loading directly bin/forming.hs. This lets
# us add additional modules for convenience when trying expressions inside
# GHCi.

ghc --interactive \
  -i../design/src/ \
  -i../syntactical/ \
  -ibin/ \
  -isrc/ \
  -itests/ \
  -XImportQualifiedPost \
  -XOverloadedStrings \
  -XRecordWildCards \
  -XTypeApplications \
  -XTypeOperators \
  -Wall \
  -ghci-script scripts/ghci.conf

#  -XNoImplicitPrelude \
