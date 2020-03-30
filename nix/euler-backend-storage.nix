{ mkDerivation, aeson, base, beam-core, beam-mysql, beam-postgres
, beam-sqlite, containers, hpack, http-api-data, http-types, stdenv
, text, time, fetchgit
}:
mkDerivation {
  pname = "euler-backend-storage";
  version = "20.2.27.1";
  src = fetchgit {
    url    = "git@bitbucket.org:juspay/euler-db.git";
    rev    = "3536abcc89e4cfda31710f502360e312eabeeb46";
    sha256 = "0q6az6bb0wd9z6asgiib2az9zdhskrcwm5kcddsrlqms5r1h6l7m";
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