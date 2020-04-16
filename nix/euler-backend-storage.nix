{ mkDerivation, aeson, base, beam-core, beam-mysql, beam-postgres
, beam-sqlite, containers, hpack, http-api-data, http-types, stdenv
, text, time, fetchgit
}:
mkDerivation {
  pname = "euler-backend-storage";
  version = "20.2.27.1";
  src = fetchgit {
    url    = "git@bitbucket.org:juspay/euler-db.git";
    rev    = "9ab73ee697d1c2c208bccfd03ca6cb25df0ac89f";
    sha256 = "0cx87xh5962p15x40xif4jd640l7bvdlz7ikisry4zbq3vpx02i2";
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