{ mkDerivation, base, blaze-react-core, bytestring, http-media
, servant, stdenv, text
}:
mkDerivation {
  pname = "servant-blaze-react";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-react-core bytestring http-media servant text
  ];
  description = "Servant integration for blaze-react";
  license = stdenv.lib.licenses.mit;
}
