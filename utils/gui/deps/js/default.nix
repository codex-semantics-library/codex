{ pkgs ? import <nixpkgs> { } }:
let
  graphvizJs = pkgs.fetchurl {
    url = "https://cdn.jsdelivr.net/npm/@hpcc-js/wasm@2.22.4/dist/graphviz.umd.js";
    sha256 = "sha256-JeT1R2S8FhCSAcL0zsJjx7ai+bL1X3AjHcgEniwm33c=";
  };
  tailwind4 = pkgs.fetchurl {
    url = "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4.1.5";
    sha256 = "sha256-HgE6gWIf2+0W908sYIjhUDYkUnsIz0mJN2nHqXY3QD8=";
  };

in pkgs.stdenv.mkDerivation {
  pname = "codex-js-deps";
  version = "1.0.0";

  src = ./.;

  buildInputs = [ pkgs.nodejs pkgs.nodePackages.rollup pkgs.curl pkgs.cacert ];

  buildPhase = ''
    export HOME=$TMPDIR
    npm install
    npx rollup -c
    cp ${graphvizJs} graphviz.umd.js
    cp ${tailwind4} tailwind4.1.5.css 
  '';

  installPhase = ''
    mkdir -p $out
    cp bundle-output.js $out/
    cp graphviz.umd.js $out/
    cp *.css $out/
  '';
}
