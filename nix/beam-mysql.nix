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
    rev    = "e5a667fe6396441c7dcf1c7755c69345f00d0eee";
    sha256 = "1crf5rsnpjm9gsxqk96y2g5wvx79rp78ivbk3qy46h9703q1zw1l";
  };
  libraryHaskellDepends = [
    aeson attoparsec base beam-core bytestring case-insensitive free
    hashable mtl mysql network-uri scientific text time
  ];
  description = "Connection layer between beam and MySQL/MariaDB";
  license = stdenv.lib.licenses.mit;
}
