{ mkDerivation, base, bytestring, containers, deepseq, doctest
, gauge, ghc-prim, Glob, hashable, hedgehog, microlens
, microlens-mtl, mtl, safe-exceptions, stm, tasty, tasty-hedgehog
, text, transformers, unordered-containers, utf8-string, vector
, stdenv
}:
mkDerivation {
  pname = "universum";
  version = "1.6.0";
  sha256 = "12gz4hpwmykb73dplbd8j628f54ipk0pygrswy0k1k7j68awnjl6";
  libraryHaskellDepends = [
    base bytestring containers deepseq ghc-prim hashable microlens
    microlens-mtl mtl safe-exceptions stm text transformers
    unordered-containers utf8-string vector
  ];
  testHaskellDepends = [
    base bytestring doctest Glob hedgehog tasty tasty-hedgehog text
    utf8-string
  ];
  benchmarkHaskellDepends = [
    base containers gauge text unordered-containers
  ];
  description = "Custom prelude used in Serokell";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
}
