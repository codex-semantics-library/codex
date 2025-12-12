{ pkgs ? import <nixpkgs> { }, ocamlPackages }:
with pkgs;
with ocamlPackages;
buildDunePackage{
  pname = "diff2junit";
  version = "dev";
  duneVersion = "3";
  src = pkgs.fetchgit {
    url = "https://gitlab-ci-token:${
        builtins.getEnv "CI_JOB_TOKEN"
      }@git.frama-c.com/codex/diff2junit";
    hash = "sha256-4JzQahF+Y/d42k5hcoaLGIN6HVvlFcAqwR5II6KOqKY=";
  };

  nativeBuildInputs = [ ];
  buildInputs = [ ];
  checkInputs = [ ];
  doCheck = true;
}
