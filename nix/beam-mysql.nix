{ mkDerivation, aeson, attoparsec, base, beam-core, bytestring
, case-insensitive, free, hashable, mtl, mysql, network-uri
, scientific, text, time
, stdenv, fetchgit
}:
mkDerivation {
  pname = "beam-mysql";
  version = "0.2.0.0";
  src = fetchgit {
    url    = "https://github.com/graninas/beam-mysql";
    rev    = "6794a25b24062678b0e61013f0e38494d4defcf0";
    sha256 = "007wpcmv13g2nfwirjv3hq2qbgjqjplm4hwcw396as9x0zjaxiax";
  };
  libraryHaskellDepends = [
    aeson attoparsec base beam-core bytestring case-insensitive free
    hashable mtl mysql network-uri scientific text time
  ];
  description = "Connection layer between beam and MySQL/MariaDB";
  license = stdenv.lib.licenses.mit;
}
