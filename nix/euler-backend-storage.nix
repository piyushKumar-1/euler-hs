{ mkDerivation, aeson, base, beam-core, beam-mysql, beam-postgres
, beam-sqlite, containers, hpack, http-api-data, http-types, stdenv
, text, time, fetchgit
}:
mkDerivation {
  pname = "euler-backend-storage";
  version = "20.2.27.1";
  src = fetchgit {
    url    = "git@bitbucket.org:juspay/euler-db.git";
    rev    = "8be8da36c76b73db7ccd3d18b5cc65655b77405e";
    sha256 = "1x32r2dwbc4dbph46nhivqdfxm529pf39zhd1bryfkz4ar4nf6mb";
  };
  libraryHaskellDepends = [
    aeson base beam-core beam-mysql beam-postgres beam-sqlite
    containers http-api-data http-types text time
  ];
  postUnpack = "sourceRoot+=/lib/euler-backend-storage";
 # libraryToolDepends = [ hpack ];
 # prePatch = "hpack";
  description = "DB types for euler-backend";
  license = "OtherLicense";
  doHaddock = false;
  isLibrary = true;
}
