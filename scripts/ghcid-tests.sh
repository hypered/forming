#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghcid \
  --command "ghci -XOverloadedStrings -XTypeApplications -i../design/src -i../syntactical bin/run-tests.hs" \
  --test ":main --color always"
