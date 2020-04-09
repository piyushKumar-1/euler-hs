{ mkDerivation, base, bytestring, HUnit, stdenv, fetchgit }:
mkDerivation {
  pname = "crc16";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/C37H40O9/crc16.git";
    sha256 = "1nskyzqqgbkh1785iwcv7ksb4cc6yx6jrgwa6x6bnd4nx9rqylby";
    rev = "9d9eee27293e09de9662ab307476914fd7f58353";
    fetchSubmodules = false;
  };
  libraryHaskellDepends = [ base bytestring HUnit ];
  testHaskellDepends = [ base bytestring HUnit ];
  description = "Calculate the crc16-ccitt";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
