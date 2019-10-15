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

{ mkDerivation, base, containers, directory, filepath
, generic-deriving, hspec, hspec-expectations-pretty-diff, lens
, mtl, text, transformers, wl-pprint-text
, stdenv, fetchgit
}:
mkDerivation {
  pname = "purescript-bridge";
  version = "0.13.0.0";
  src = fetchgit {
    url = "https://github.com/shmish111/purescript-bridge.git";
    rev = "0042602f8a195b1fe185138f9ccca02020b8dd62";
    sha256 = "1vl88g41f4vvgw9iyn7zd7i52qshpnk02z0y6czg4zy7wk3q12gz";
  };
  libraryHaskellDepends = [
    base containers directory filepath generic-deriving lens mtl text
    transformers wl-pprint-text
  ];
  testHaskellDepends = [
    base containers hspec hspec-expectations-pretty-diff text wl-pprint-text
  ];
  description = "Generate PureScript data types from Haskell data types";
  license = stdenv.lib.licenses.bsd3;
}
