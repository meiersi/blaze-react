{ mkDerivation, base, blaze-react-core, blaze-react-dev-mode-api
, bytestring, lens, mtl, servant-blaze-react, servant-server
, stdenv, stm, text, time, wai, warp
}:
mkDerivation {
  pname = "blaze-react-dev-mode-server";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-react-core blaze-react-dev-mode-api bytestring lens mtl
    servant-blaze-react servant-server stm text time wai warp
  ];
  description = "Development mode server for blaze-react";
  license = stdenv.lib.licenses.mit;
}
