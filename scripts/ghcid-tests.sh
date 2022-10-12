#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghcid \
  --command "ghci -i../design-system -i../syntactical bin/run-tests.hs" \
  --test ":main --color always"