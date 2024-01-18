{ pkgs, ocaml, findlib }:
pkgs.stdenv.mkDerivation rec {
  pname = "grain_dypgen";
  version = "0.2";

  src = pkgs.fetchurl {
    url = "https://github.com/grain-lang/dypgen/archive/0.2.tar.gz";
    sha512 = "b01044243d76550194ea1a9e93eb53f525ae9e88ef0a1d69c5c00ef7adf3791d88df72f8ccb47fe0966c54fc214ff8e54f3cdf34c7627ea10b3a1a64f2e683a0";
  };

  buildInputs = [ ocaml findlib ];

  installPhase = ''
    make install DYPGENLIBDIR=$out/lib/ocaml/${ocaml.version}/site-lib/ BINDIR=$out/bin/ MANDIR=$out/man/man1
  '';
}
