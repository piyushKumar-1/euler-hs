# Copyright (c) 2003-2019 Eelco Dolstra and the Nixpkgs/NixOS contributors
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
