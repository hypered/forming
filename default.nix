let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };

in rec
  {
    # Build with nix-build -A <attr>
    binaries = nixpkgs.haskellPackages.forming;
    # binaries + haddock are also available as binaries.all.
    haddock = nixpkgs.haskellPackages.forming.doc;

    struct = (import "${sources.hypered-design}").struct;

    # A shell to try out our binaries
    # Run with nix-shell default.nix -A shell
    shell = nixpkgs.mkShell {
      buildInputs = [
        binaries
      ];
      shellHook = ''
        source <(forming --bash-completion-script `which forming`)
      '';
    };
  }
