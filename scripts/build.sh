#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc --make -i../design-system/ -i../syntactical/ bin/add.hs
ghc --make -i../design-system/ -i../syntactical/ bin/forming-examples.hs
ghc --make -i../design-system/ -i../syntactical/ bin/forming.hs
