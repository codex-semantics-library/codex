{ stdenv, lib, fetchzip, ocaml, findlib, camlidl, m4 }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-mlcuddidl";
  version = "3.0.8";
  src = fetchzip {
#    url = "http://nberth.space/pool/mlcuddidl/mlcuddidl-${version}.tar.gz";
     url = "https://framagit.org/nberth/mlcuddidl/-/archive/3.0.8/mlcuddidl-3.0.8.tar.gz";
#    sha256 = "09hbgw18c6mfx4zy3rydkfqnj2v55rccz3i6anccn82fq1nsx5lz";
#    sha512 = "b039fd1162bef7460bc7cf378a000d35730d5327ec4648dfe7eef88c148fb6812764bf32a1801353a09f59c9bcd6ea386f77e1896e48b53163d4c081fed5a80c";
#     sha256 = "dcdad49917ef1b01c109fe4fb947d7cc48c8c63c930f6092a86ecc0857cff8cd";
     hash="sha256-2tyZ1O8XARsJwU/+R7nM18hIPMYPk5JgbqgIzM9Xzfg=";
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
