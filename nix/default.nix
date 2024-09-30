let
  pkgs = import <nixpkgs> {};
  cp = pkgs.ocaml-ng.ocamlPackages_4_14.callPackage;
  cudd = cp ./cudd.nix {};
  grain_dypgen = cp ./grain_dypgen.nix {};
  diff2junit = cp ./diff2junit.nix {};  
  binsec = cp ./binsec.nix { inherit grain_dypgen; };
  framac = cp ./frama-c.nix { inherit cudd; };
  codex = cp ./codex.nix { inherit cudd binsec framac diff2junit; };
in
  codex
