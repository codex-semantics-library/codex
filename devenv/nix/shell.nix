{ pkgs ? import <nixpkgs> { } }:

let

  myocamlpackage = pkgs.ocaml-ng.ocamlPackages_5_2;
  # myocamlpackage = mmyocamlpackage.overrideScope (self: super: {
  #   ppxlib = super.buildDunePackage rec {
  #     pname = "ppxlib";
  #     version = "0.33.0";
  #     src = pkgs.fetchurl {
  #       url =
  #         "https://github.com/ocaml-ppx/ppxlib/releases/download/0.33.0/ppxlib-0.33.0.tbz";
  #       hash = "sha256-/6RO9VHyO3XiHb1pijAxBDE4Gq8UC5/kuBwucKLSxjo=";
  #     };
  #     duneVersion = "3";
  #     buildInputs = with self; [
  #       base
  #       ocaml-compiler-libs
  #       janeStreet.ocaml-compiler-libs
  #       ppx_derivers
  #       stdio
  #       stdlib-shims
  #     ];
  #   };
  # });
  #
  # I have differences with the version of z3 in pkgs.z3
  myz3 = myocamlpackage.callPackage ./z3.nix { };
  # Import local cudd package as a nix derivation
  # TODO: some of these packages could be obtained using opam2nix.
  diff2junit = myocamlpackage.callPackage ./diff2junit.nix {
    ocamlPackages = myocamlpackage;
  };

  # (Oct. 2025) the mdx package conflicts with other ocamlpackages, like yaml,
  # which causes errors saying that logs has several META files.
  # To solve it, we build our own version of mdx.
  mdx =
    myocamlpackage.callPackage ./mdx.nix { ocamlPackages = myocamlpackage; pkgs = pkgs; };
  cudd =
    myocamlpackage.callPackage ./cudd.nix { ocamlPackages = myocamlpackage; pkgs = pkgs; };
  pacomb =
    myocamlpackage.callPackage ./pacomb.nix { ocamlPackages = myocamlpackage; pkgs = pkgs;};
  patricia-tree = myocamlpackage.callPackage ./patricia-tree.nix {
    ocamlPackages = myocamlpackage; pkgs = pkgs;
  };
  frama-c = myocamlpackage.callPackage ./frama-c.nix {
    ocamlPackages = myocamlpackage; pkgs = pkgs;
  };
  binsec =
    myocamlpackage.callPackage ./binsec.nix { ocamlPackages = myocamlpackage; pkgs = pkgs; };

  # Your main package, assuming you have a dune-project and dune files here
  # TODO: Could be obtained from opam2nix, to ensure that it is is compatible.
  codex = myocamlpackage.buildDunePackage {
    pname = "codex";
    version = "1.0rc2";
    src = ../.;
    dontDetectOcamlConflicts = true;

    nativeBuildInputs = [ pkgs.git myocamlpackage.dune_3 ];

    buildInputs = [
      cudd
      pacomb
      patricia-tree
      myocamlpackage.ppxlib
      myocamlpackage.ppx_deriving
      myocamlpackage.stdlib-shims
      myocamlpackage.camlp-streams
      myocamlpackage.ppx_inline_test
      myocamlpackage.bheap
      myocamlpackage.zarith
      myocamlpackage.fmt
      myocamlpackage.base64
      myocamlpackage.ocaml
      myocamlpackage.dune_3
      myocamlpackage.qcheck-core
      myocamlpackage.ocaml-embed-file
      myocamlpackage.js_of_ocaml
      myocamlpackage.js_of_ocaml-ppx
      myocamlpackage.vdom
      myocamlpackage.odoc
#     frama-c
#     binsec
    ] ++ frama-c.buildInputs ++ binsec.buildInputs;
  };

in pkgs.mkShell {

  packages = [ pkgs.curl pkgs.cacert frama-c binsec diff2junit myz3 pkgs.headache mdx];

  nativeBuildInputs = [
    # 32-bit toolchain - Used to compile the example.c and run the binary analysis
    # in doc/types-tutorial
    # pkgs.gcc_multi
    # pkgs.glibc_multi
    # pkgs.pkgsi686Linux.gcc
    # pkgs.pkgsi686Linux.binutils
    pkgs.cproto
  ];

  # ensure 32-bit compiler front-end is used for code requiring -m32
  CC = "i686-unknown-linux-gnu-gcc";
  LD = "i686-unknown-linux-gnu-ld";

  inputsFrom = [ codex ];

  # optional environment setup, e.g.:
  #   shellHook = ''
  #     echo "Welcome to the Codex dev shell!"
  #     export OCAMLRUNPARAM="b"
  #   '';
}
