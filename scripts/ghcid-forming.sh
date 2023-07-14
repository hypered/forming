#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghcid \
  --command "ghci -i../design -i../syntactical bin/forming.hs"
