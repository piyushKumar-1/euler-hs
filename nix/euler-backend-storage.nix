{ mkDerivation, aeson, base, beam-core, beam-mysql, beam-postgres
, beam-sqlite, containers, hpack, http-api-data, http-types, stdenv
, text, time, fetchgit
}:
mkDerivation {
  pname = "euler-backend-storage";
  version = "20.2.27.1";
  src = fetchgit {
    url    = "git@bitbucket.org:juspay/euler-db.git";
    rev    = "332d216c19982353270a5204cde115511f2d2e7d";
    sha256 = "0qwdn4q5k8rs6b5dmg27xf7c6i0qcs88cg6ca4f61nms42k8fn9p";
  };
  libraryHaskellDepends = [
    aeson base beam-core beam-mysql beam-postgres beam-sqlite
    containers http-api-data http-types text time
  ];
  postUnpack = "sourceRoot+=/lib/euler-backend-storage";
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  description = "DB types for euler-backend";
  license = "OtherLicense";
}
