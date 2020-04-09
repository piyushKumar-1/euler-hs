{ mkDerivation, base, bytestring, http-media, servant, stdenv
, xmlbf, xmlbf-xeno
}:
mkDerivation {
  pname = "servant-xml";
  version = "1.0.1.4";
  sha256 = "5dd006f1d303ffdb670ceea96bfd5eef07d11a75d2a3de47e9ca32c917e1ff4b";
  libraryHaskellDepends = [
    base bytestring http-media servant xmlbf xmlbf-xeno
  ];
  homepage = "https://github.com/fosskers/servant-xml";
  description = "Servant support for the XML Content-Type";
  license = stdenv.lib.licenses.bsd3;
}
