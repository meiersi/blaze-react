{ mkDerivation, aeson, base, blaze-react-core, bytestring, either
, lens, mtl, servant, servant-docs, stdenv, stm, text, time
, transformers
}:
mkDerivation {
  pname = "blaze-react-dev-mode-shared";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base blaze-react-core bytestring either lens mtl servant
    servant-docs stm text time transformers
  ];
  description = "Code shared between the dev-mode client app and the server library";
  license = stdenv.lib.licenses.mit;
}
