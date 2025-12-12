{ pkgs ? import <nixpkgs> { }, release_mode ? false, ocamlPackages }:
with pkgs;
with ocamlPackages;
# We do not use buildDunePackage because Frama-C still uses a Makefile to build
# some files and prepare some information before starting dune.
pkgs.stdenvNoCC.mkDerivation {
  pname = "frama-c";
  version = "31.0";

  src = builtins.fetchGit {
    url = "git@git.frama-c.com:pub/frama-c";
    ref = "refs/tags/31.0";
  };

  nativeBuildInputs = [ which wrapGAppsHook ];

  buildInputs = [
    apron
    bisect_ppx
    camlzip
    camomile
    dune_3
    dune-configurator
    dune-site
    findlib
    fpath
    gcc14
    graphviz
    lablgtk3
    lablgtk3-sourceview3
    ltl2ba
    menhir
    menhirLib
    # mlmpfr
    ocaml
    ocamlgraph
    ocp-indent
    ppx_deriving
    ppx_deriving_yaml
    ppx_deriving_yojson
    ppx_inline_test
    unionFind
    yojson
    which
    why3
    yaml
    zarith
    zmq
    # For other CI targets
    # python310
  ];

  outputs = [ "out" "build_dir" ];

  postPatch = ''
    patchShebangs .
  '';

  preConfigure = ''
    dune build @frama-c-configure
  '';

  # Do not use default parallel building, but allow 2 cores for Frama-C build
  enableParallelBuilding = false;
  dune_opt = if release_mode then "--release" else "";

  buildPhase = ''
    dune build -j2 $dune_opt @install

    make tools/ptests/ptests.exe
    make tools/ptests/wtests.exe
  '';

  installFlags = [ "PREFIX=$(out)" ];

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
      if test -d "$1/lib/frama-c/plugins"; then
        export FRAMAC_PLUGIN="''${FRAMAC_PLUGIN-}''${FRAMAC_PLUGIN:+:}$1/lib/frama-c/plugins"
        export OCAMLPATH="''${OCAMLPATH-}''${OCAMLPATH:+:}$1/lib/frama-c/plugins"
      fi

      if has_dirs $1/lib/frama-c-*; then
        export OCAMLPATH="''${OCAMLPATH-}''${OCAMLPATH:+:}$1/lib"
        export DUNE_DIR_LOCATIONS="''${DUNE_DIR_LOCATIONS-}''${DUNE_DIR_LOCATIONS:+:}frama-c:lib:$1/lib/frama-c"
      fi

    }

    addEnvHooks "$targetOffset" addFramaCPath
  '';

  meta = {
    description =
      "An extensible and collaborative platform dedicated to source-code analysis of C software";
    homepage = "http://frama-c.com/";
    license = lib.licenses.lgpl21;
    platforms = lib.platforms.unix;
  };
}
