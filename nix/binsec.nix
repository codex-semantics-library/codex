{ lib
, nix-gitignore
, gmp
, mmap
, ocamlgraph
, zarith
, menhir
, toml
/* , llvm */
, dune-site
, qtest
, ounit
, qcheck
, seq
 /* , unisim_archisec */
, buildDunePackage
, bubblewrap
, glibc
, curses
, pkgsi686Linux
, python3
, bitwuzla
, boolector
, z3
, gdb
, file
, grain_dypgen
}:
buildDunePackage {
  pname = "binsec";
  version = "dev";

  duneVersion = "3";


  src = fetchGit {
    url = "https://gitlab-ci-token:${builtins.getEnv "CI_JOB_TOKEN"}@git.frama-c.com/binary/binsec";
    /*  version 0.8.1. */
    rev = "8fb27e39e3a025b15407a1fd73d56655227d5ed1";
  };

  nativeBuildInputs = [
    dune-site
    menhir
    grain_dypgen
  ];

  buildInputs = [
    gmp # for zarith
    ocamlgraph
    zarith
    dune-site
    /* llvm */
    menhir
    grain_dypgen
    toml
    /* unisim_archisec */
    curses
    mmap
  ];

  checkInputs = [
    qtest
    ounit
    qcheck
    seq
    bubblewrap
    python3
    z3
    bitwuzla
    boolector
    gdb
    file
  ];

  doCheck = true;
}
