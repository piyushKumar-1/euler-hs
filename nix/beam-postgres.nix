{ mkDerivation, aeson, attoparsec, base, beam-core, beam-migrate
, bytestring, case-insensitive, conduit, directory, filepath, free
, hashable, haskell-src-exts, hedgehog, lifted-base, monad-control
, mtl, network-uri, postgresql-libpq, postgresql-simple, process
, scientific, tagged, tasty, tasty-hunit, temporary, text, time
, unordered-containers, uuid, uuid-types, vector
, stdenv
}:
mkDerivation {
  pname = "beam-postgres";
  version = "0.4.0.0";
  sha256 = "0dxnp6zsyy30vrlv15iw4qwyzwawg468zqqsjnzk9h3g9k9xzj3v";
  libraryHaskellDepends = [
    aeson attoparsec base beam-core beam-migrate bytestring
    case-insensitive conduit free hashable haskell-src-exts lifted-base
    monad-control mtl network-uri postgresql-libpq postgresql-simple
    scientific tagged text time unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    base beam-core beam-migrate bytestring directory filepath hedgehog
    postgresql-simple process tasty tasty-hunit temporary text uuid
  ];
  description = "Connection layer between beam and postgres";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
  doCheck = false; # Tests are failing because of duplicate instances
}
