{ ocaml, bheap, ocamlgraph, camlp-streams, nix-gitignore, stdenv, gmp, findlib, binsec, framac, zarith, cudd, yaml, menhirLib
, yojson, apron, menhir, ppx_import, ppx_deriving, ppx_deriving_yojson,
    dune_3, dune-configurator, dune-site, zmq, sexplib, ppx_sexp_conv, ppx_inline_test, qcheck }:
let
  pkgs = import <nixpkgs> {};
  ocaml-lmdb = pkgs.ocaml-ng.ocamlPackages_4_14.callPackage ./ocaml-lmdb.nix {};
#  lmdb-ocaml = with pkgs; with stdenv; import ./lmdb-ocaml.nix { inherit stdenv ocaml lib buildDunePackage lmdb fetchzip; };
in
stdenv.mkDerivation {
  pname = "codex";
  version = "1.0.0";
  src = nix-gitignore.gitignoreSourcePure
    [
      ../.gitignore
      "../nix"
    ] ../.;
  buildInputs = [
    gmp # for zarith
    ocaml
    findlib
    ocamlgraph
    zarith
    bheap
    cudd
    camlp-streams
    yaml
    framac
    binsec
    yojson
    apron
    menhir
    menhirLib
    ppx_import
    ppx_deriving
    ppx_deriving_yojson
    ppx_inline_test
    dune_3
    dune-configurator
    dune-site
    qcheck
    zmq
# For odvtk
    ocaml-lmdb
    sexplib
    ppx_sexp_conv
  ];
}
