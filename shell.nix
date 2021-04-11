{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [
      pkgs.ghcid
      (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
        hpkgs.aeson
        hpkgs.blaze-html
        hpkgs.lens
        hpkgs.snap
        hpkgs.snap-core
        hpkgs.snap-server
      ]))
    ];
}
