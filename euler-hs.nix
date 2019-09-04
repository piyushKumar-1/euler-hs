{ mkDerivation, base, aeson, bytestring, containers, errors, extra, hashable, mtl
, network, optparse-applicative, text, transformers, unordered-containers, lens
, http-client, messagepack, cereal, time, newtype, monad-loops, http-types
, attoparsec, scientific, stm, named, tagged, time-units, aeson-pretty, cryptonite
, base16-bytestring, base58-bytestring, base64-bytestring, process, vector, random
, free, async, haskeline, cryptohash-sha256, bytestring-conversion, fmt
, lens-aeson, uuid, safe-exceptions
, configureFlags ? []
, enableSharedExecutables ? true
, enableSharedLibraries ? true
, pname ? null
}:
assert pname != null;
mkDerivation rec {
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  inherit pname enableSharedExecutables enableSharedLibraries configureFlags;
  executableHaskellDepends = [
    base aeson bytestring containers errors extra hashable mtl
     network optparse-applicative text transformers unordered-containers lens
     http-client messagepack cereal time newtype monad-loops http-types
     attoparsec scientific stm named tagged time-units aeson-pretty cryptonite
     base16-bytestring base58-bytestring base64-bytestring process vector random
     free async haskeline cryptohash-sha256 bytestring-conversion fmt
     lens-aeson uuid safe-exceptions
  ];
  description = "Data processing";
  license = "unknown";

  buildTarget = pname;
  installPhase = ''
    mkdir -p $out/bin
    cp dist/build/${pname}/${pname} $out/bin
  '';
  doHaddock = false;
}
