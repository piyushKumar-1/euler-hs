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
