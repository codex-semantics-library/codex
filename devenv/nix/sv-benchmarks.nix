{ pkgs ? import <nixpkgs> {} }:
# {stdenv, lib, fetchzip, fetchgit}:
pkgs.stdenv.mkDerivation {
  pname = "sv-benchmarks";
  version = "svcomp24-final";

  src = pkgs.fetchgit {
    url = "https://gitlab-ci-token:${builtins.getEnv "CI_JOB_TOKEN"}@git.frama-c.com/codex/sv-benchmarks";
    hash = "sha256-Vsq5XL2W4Jb+0AcHcDH4wEmKhjf/+wrUEcbu05mWJa8=";
  };

  installPhase = ''
    echo "SVBENCHMARK INSTALL PHASE"
    mkdir -p $out
    cp -r $src/* $out
  '';
}
