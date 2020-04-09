{ mkDerivation, array, async, base, bytestring, bytestring-lexing
, containers, crc16, deepseq, doctest, errors, fetchgit, HTTP
, HUnit, mtl, network, network-uri, resource-pool, say, scanner
, stdenv, stm, test-framework, test-framework-hunit, text, time
, tls, unordered-containers, vector
}:
mkDerivation {
  pname = "hedis";
  version = "0.12.8";
  src = fetchgit {
    url = "https://github.com/juspay/hedis.git";
    sha256 = "0px92p656l8dkrq1mb9vg1dsc1lkfkfx5sklnfbmvibn21m5zsqv";
    rev = "ce6de8f158e87414cf11bcba4e95d54a9c3185f3";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array async base bytestring bytestring-lexing containers crc16
    deepseq errors HTTP mtl network network-uri resource-pool say
    scanner stm text time tls unordered-containers vector
  ];
  testHaskellDepends = [
    async base bytestring doctest HUnit mtl stm test-framework
    test-framework-hunit text time
  ];
  benchmarkHaskellDepends = [ base mtl time ];
  homepage = "https://github.com/informatikr/hedis";
  description = "Client library for the Redis datastore: supports full command set, pipelining";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}