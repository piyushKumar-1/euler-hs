{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, stdenv, tasty, tasty-hunit, text, time, unordered-containers
}:
mkDerivation {
  pname = "amazonka-kms";
  version = "1.6.1";
  sha256 = "6d333ec392d1f47c850449e78a1071f2265b46f699f3c58ad9e30bd99c956285";
  libraryHaskellDepends = [ amazonka-core base ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring tasty tasty-hunit text
    time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Key Management Service SDK";
  license = stdenv.lib.licenses.mpl20;
}
