{ mkDerivation, aeson, base, beam-core, beam-mysql, beam-postgres
, beam-sqlite, containers, hpack, http-api-data, http-types, stdenv
, text, time, fetchgit
}:
mkDerivation {
  pname = "euler-backend-storage";
  version = "20.2.27.1";
  src = fetchgit {
    url    = "git@bitbucket.org:juspay/euler-db.git";
    rev    = "9df750f2ff0c00371e33281c1b985b1174649d77";
    sha256 = "1x726rrd7dgvwcsqllsmm353ks47p4lq0p6h1iq8ryplqrpp0v6m";
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