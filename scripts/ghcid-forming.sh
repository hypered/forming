#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghcid \
  --command "ghci -i../design-system -i../syntactical bin/forming.hs"
