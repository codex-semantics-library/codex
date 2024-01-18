{ stdenv, ocaml, lib, buildDunePackage, lmdb, fetchzip, dune-configurator, bigstringaf, pkg-config }:

buildDunePackage rec  {
  pname = "lmdb";
  version = "1.0";
  src = fetchzip {
     url = "https://github.com/Drup/ocaml-lmdb/archive/1.0.tar.gz";
     sha256 = "1h461i4n9mpdzpsplzmfjnr2vqjm2yk028h3m1rjifk92gpqrf1m";
  };

 useDune2 = true;
 nativeBuildInputs = [ pkg-config ];
 propagatedBuildInputs = [ lmdb dune-configurator bigstringaf ];
 strictDeps = true;
 createFindlibDestdir = true; 

 meta = {
    description = "OCaml interface to the LMDB library";
    homepage = https://github.com/Drup/ocaml-lmdb/;
    license = lib.licenses.mit;
    inherit (ocaml.meta) platforms;
  };


}
