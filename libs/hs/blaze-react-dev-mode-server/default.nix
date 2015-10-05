{ mkDerivation, aeson, async, base, blaze-react-core
, blaze-react-dev-mode-shared, bytestring, directory, file-embed
, filepath, lens, managed, mtl, random, servant-blaze-react
, servant-server, stdenv, stm, text, time, wai, wai-app-static
, warp
}:
mkDerivation {
  pname = "blaze-react-dev-mode-server";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base blaze-react-core blaze-react-dev-mode-shared
    bytestring directory file-embed filepath lens managed mtl random
    servant-blaze-react servant-server stm text time wai wai-app-static
    warp
  ];
  description = "Development mode server for blaze-react";
  license = stdenv.lib.licenses.mit;
}
