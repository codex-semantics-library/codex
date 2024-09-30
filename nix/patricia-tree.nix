{ stdenv, ocaml, lib, buildDunePackage, findlib, fetchzip, fetchgit, zarith, qcheck, ppx_inline_test, mdx }:

buildDunePackage rec  {
  pname = "patricia-tree";
  version = "0.10";

  src = fetchGit {
    url = "https://github.com/codex-semantics-library/patricia-tree.git";
    rev = "${builtins.getEnv "PATRICIA_TREE_REV"}";
#   "ab57e7e6c722c34219a8e97390b51bea85653d0f";    
  };

  duneVersion = "3";
  doCheck = false;
  checkInputs = [];
  buildInputs = [ zarith qcheck ppx_inline_test mdx ];
}
