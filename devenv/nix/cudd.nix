{ pkgs ? import <nixpkgs> { }, ocamlPackages }:
with ocamlPackages;
with pkgs;
buildDunePackage {
  pname = "cudd";
  version = "${builtins.getEnv "CUDDML_REV"}";
  duneVersion = "3";
  # WE use fetchGit and not fetchgit here, because it does not require a hash. 
  src = builtins.fetchGit {
    url = "https://git.frama-c.com/pub/codex/cudd.ml";
    rev = "${builtins.getEnv "CUDDML_REV"}";
  };

  nativeBuildInputs = [ curl cacert ];
  buildInputs = [ ];
  checkInputs = [ ];
  doCheck = true;
}
