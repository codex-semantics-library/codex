let
  pkgs = import <nixpkgs> {};
  cp = pkgs.ocaml-ng.ocamlPackages_4_14.callPackage;
  /*cp = pkgs.ocaml-ng.ocamlPackages_4_07.callPackage;*/
  cudd = cp ./cudd.nix {};
  grain_dypgen = cp ./grain_dypgen.nix {};
  binsec = cp ./binsec.nix { inherit grain_dypgen; };
  framac = cp ./frama-c.nix { inherit cudd; };
  codex = cp ./codex.nix { inherit cudd binsec framac ; };
in
  codex
