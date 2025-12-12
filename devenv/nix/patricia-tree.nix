{ pkgs ? import <nixpkgs> { }, ocamlPackages }:
with pkgs;
with ocamlPackages;

buildDunePackage rec {
  pname = "patricia-tree";
  version = "${builtins.getEnv "PATRICIA_TREE_REV"}";

  # WE use fetchGit and not fetchgit here, because it does not require a hash.
  src = builtins.fetchGit {
    url = "https://github.com/codex-semantics-library/patricia-tree.git";
    rev = "${builtins.getEnv "PATRICIA_TREE_REV"}";
  };

  duneVersion = "3";
  doCheck = false;
  checkInputs = [ ];
  buildInputs = [ zarith qcheck ppx_inline_test mdx ];
}
