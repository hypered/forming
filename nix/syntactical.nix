# This file can be removed when syntactical provides its own nix/overlay.nix
# file.

self: super:
let

  lib = super.lib;

  inherit (import ./sources.nix) syntactical; 

  stmContainersOverrides = selfh: superh: {
    syntactical =
      selfh.callCabal2nix "syntactical" syntactical { };
  };

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) stmContainersOverrides;
  });
}
