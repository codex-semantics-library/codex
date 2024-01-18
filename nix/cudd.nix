{ stdenv, lib, fetchzip, ocaml, findlib, camlidl, m4 }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-mlcuddidl";
  version = "3.0.6";
  src = fetchzip {
    url = "http://nberth.space/pool/mlcuddidl/mlcuddidl-${version}.tar.gz";
    sha256 = "09hbgw18c6mfx4zy3rydkfqnj2v55rccz3i6anccn82fq1nsx5lz";
  };

  nativeBuildInputs = [ ocaml findlib m4 ];
  buildInputs = [ camlidl ];

  createFindlibDestdir = true;

  meta = {
    description = "OCaml interface to the CUDD BDD library";
    homepage = https://www.inrialpes.fr/pop-art/people/bjeannet/mlxxxidl-forge/mlcuddidl/;
    license = lib.licenses.lgpl21;
    inherit (ocaml.meta) platforms;
  };
}
