{ pkgs ? import <nixpkgs> { }, ocamlPackages }:
with pkgs;
with ocamlPackages;
buildDunePackage {
  pname = "binsec";
  version = "0.10.0";
  duneVersion = "3";

  src = fetchgit {
    url = "https://gitlab-ci-token:${
        builtins.getEnv "CI_JOB_TOKEN"
      }@git.frama-c.com/binary/binsec";
    # version 0.10.0.
    rev = "2792ee4e9508255f7afd17c2cbd20303e939b409";
    hash = "sha256-eGysV2vTRnpV0muE/yANf1+PZqz4OUhoiQPoaXG5nC8=";
  };

  nativeBuildInputs = [ dune-site menhir dypgen ];

  buildInputs = [
    gmp # for zarith
    ocamlgraph
    zarith
    dune-site
    # llvm
    menhir
    dypgen
    toml
    # unisim_archisec
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

  doCheck = false;
}
