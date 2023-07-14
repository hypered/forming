#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc --make -XOverloadedStrings -XTypeApplications -i../design/src/ -i../syntactical/ bin/add.hs
ghc --make -XOverloadedStrings -XTypeApplications -i../design/src/ -i../syntactical/ bin/forming-examples.hs
ghc --make -XOverloadedStrings -XTypeApplications -i../design/src/ -i../syntactical/ bin/forming.hs
