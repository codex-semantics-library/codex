{ pkgs ? import <nixpkgs> { }, ocamlPackages }:
with pkgs;
with ocamlPackages;

buildDunePackage rec {
  pname = "pacomb";
  version = "1.3";
  src = pkgs.fetchzip {
    url = "https://github.com/craff/pacomb/archive/refs/tags/1.3.tar.gz";
    hash = "sha256-7y5/57FVLBLPH951LBESEXW/WnvgsZiWc6XG954jTGc=";
  };

  duneVersion = "3";
  doCheck = true;
  checkInputs = [ stdlib-shims ];
  buildInputs = [ ppxlib stdlib-shims ];

}
