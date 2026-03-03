{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

pkgs.mkShell {
  name = "codex-dev-shell";
  dontDetectOcamlConflicts = true;
  nativeBuildInputs = with pkgs.ocamlPackages; [
    dune_3
    findlib
    # tests
    mdx
    mdx.bin
    qcheck-core
    # dev dependencies
    merlin
    ocaml
    ocamlformat
    ocp-browser
    ocp-index
    ocb
    odoc
  ];
  buildInputs = with pkgs.ocamlPackages; [
    js_of_ocaml
    js_of_ocaml-ppx
    ppx_deriving
    ppx_inline_test
  ];
  propagatedBuildInputs = with pkgs.ocamlPackages; [
    base64
    bheap
    camlp-streams
    cudd
    fmt
    pacomb
    patricia-tree
    vdom
    zarith
  ];

  shellHook = ''
    mkdir -p utils/gui/deps/js

    fetch_if_missing () {
      if [ ! -f "$1" ]; then
        echo "Fetching $1"
        curl -L "$2" -o "$1"
      fi
    }

    fetch_if_missing \
      utils/gui/deps/tailwind4.1.5.css \
      https://github.com/codex-semantics-library/codex/releases/download/1.0-rc4/tailwind4.1.5.css

    fetch_if_missing \
      utils/gui/deps/graphviz.umd.js \
      https://github.com/codex-semantics-library/codex/releases/download/1.0-rc4/graphviz.umd.js

    fetch_if_missing \
      utils/gui/deps/js/bundle-output.js \
      https://github.com/codex-semantics-library/codex/releases/download/1.0-rc4/bundle-output.js
  '';
}
