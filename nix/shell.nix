let
  pkgs = import <nixpkgs> {};
  codex = import ./default.nix;
  z3 = pkgs.callPackage ./z3.nix {};
in
  pkgs.mkShell {
    inputsFrom = [ codex ];
    nativeBuildInputs = [ pkgs.ocaml-ng.ocamlPackages_4_14.merlin pkgs.dune_3 z3 ];
  }

