# Nix
{ lib
, stdenvNoCC # for E-ACSL
, fetchurl
, makeWrapper
, nix-gitignore
, wrapGAppsHook
, writeText
# Generic
, findlib
# Frama-C build
, apron
, camlzip
, cudd
, camomile
, dune_3
, dune-configurator
, dune-site
, fpath
, gcc9
, graphviz
, lablgtk3
, lablgtk3-sourceview3
, ltl2ba
, menhir
, menhirLib
#, mlmpfr
, ocaml
, ocamlgraph
, ocamlgraph_gtk
, ocp-indent
, ppx_deriving
, ppx_deriving_yaml
, ppx_deriving_yojson
, ppx_import
, yojson
, which
, why3
, yaml
, zarith
, zmq
# Frama-C extra (other targets do not reconfigure)
, dos2unix
, doxygen
, python3
, python3Packages
, yq
, release_mode ? false
}:

# We do not use buildDunePackage because Frama-C still uses a Makefile to build
# some files and prepare some information before starting dune.
stdenvNoCC.mkDerivation rec {
  pname = "frama-c";
  version = "27.1";

  src = fetchGit { 
     url = "git@git.frama-c.com:frama-c/frama-c";
     rev = "2b89cfd81a917d4bc5c44ad3ee5f3fe463c42d82";
     };

  nativeBuildInputs = [
    which
    wrapGAppsHook
  ];

  buildInputs = [
    apron
    camlzip
    cudd
    camomile
    dune_3
    dune-configurator
    dune-site
    findlib
    fpath
    gcc9
    graphviz
    lablgtk3
    lablgtk3-sourceview3
    ltl2ba
    menhir
    menhirLib
#    mlmpfr
    ocaml
    ocamlgraph
    ocamlgraph_gtk
    ocp-indent
    ppx_deriving
    ppx_deriving_yaml
    ppx_deriving_yojson
    ppx_import
    yojson
    which
    why3
    yaml
    zarith
    zmq
    # For other CI targets
    dos2unix
    doxygen
    python3
    python3Packages.pyaml
    yq
  ];

  outputs = [ "out" "build_dir" ];

  preConfigure = ''
    rm -Rf src/plugins/wp src/plugins/impact src/plugins/slicing src/plugins/e-acsl src/plugins/instantiate src/plugins/scope src/plugins/sparecode src/plugins/aorai src/plugins/dive src/plugins/reduc src/plugins/inout src/plugins/pdg src/plugins/from src/plugins/gui src/plugins/studia src/plugins/users src/plugins/users src/plugins/markdown-report src/plugins/nonterm src/plugins/server src/plugins/rte src/plugins/report src/plugins/qed
    patchShebangs src/plugins/eva/gen-api.sh
    chmod +x src/plugins/eva/gen-api.sh
    dune build @frama-c-configure
  '';

  # Do not use default parallel building, but allow 2 cores for Frama-C build
  enableParallelBuilding = false;
  dune_opt = if release_mode then "--release" else "" ;
  patches = [ ./patch-frama-c.patch ];

  buildPhase = ''
    dune build -j2 --display short --error-reporting=twice $release_mode @install
    make tools/ptests/ptests.exe
    make tools/ptests/wtests.exe
  '';

  installFlags = [
    "PREFIX=$(out)"
  ];

  # Simpler for our test target
  # We export the build directory to avoid rebuilding Frama-C without having to
  # manage complex dependencies.
  postInstall = ''
    mkdir -p $build_dir
    tar -cf $build_dir/dir.tar .
  '';

  # Nix moves these directories after they have been installed by Dune and
  # compress manuals ...
  # Note: Required so that tests of external plugins can be executed. Don't know
  # why tests fail without them.
  postFixup = ''
    cp -r $out/share/doc $out/doc
    cp -r $out/share/man $out/man
    gzip -d $out/man/man1/*
  '';

  # Allow loading of external Frama-C plugins
  setupHook = writeText "setupHook.sh" ''
    has_dirs() {
      for f do
        [ -d "$f" ] && return
      done
      false
    }

    addFramaCPath () {
      if test -d "''$1/lib/frama-c/plugins"; then
        export FRAMAC_PLUGIN="''${FRAMAC_PLUGIN-}''${FRAMAC_PLUGIN:+:}''$1/lib/frama-c/plugins"
        export OCAMLPATH="''${OCAMLPATH-}''${OCAMLPATH:+:}''$1/lib/frama-c/plugins"
      fi

      if has_dirs ''$1/lib/frama-c-*; then
        export OCAMLPATH="''${OCAMLPATH-}''${OCAMLPATH:+:}''$1/lib"
        export DUNE_DIR_LOCATIONS="''${DUNE_DIR_LOCATIONS-}''${DUNE_DIR_LOCATIONS:+:}frama-c:lib:''$1/lib/frama-c"
      fi

      if test -d "''$1/share/frama-c/"; then
        export FRAMAC_EXTRA_SHARE="''${FRAMAC_EXTRA_SHARE-}''${FRAMAC_EXTRA_SHARE:+:}''$1/share/frama-c"
      fi

    }

    addEnvHooks "$targetOffset" addFramaCPath
  '';

  meta = {
    description = "An extensible and collaborative platform dedicated to source-code analysis of C software";
    homepage = "http://frama-c.com/";
    license = lib.licenses.lgpl21;
    platforms = lib.platforms.unix;
  };
}
