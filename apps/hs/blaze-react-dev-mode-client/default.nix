{ mkDerivation, base, blaze-react-core, blaze-react-dev-mode-api
, stdenv
}:
mkDerivation {
  pname = "blaze-react-dev-mode-client";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-react-core blaze-react-dev-mode-api
  ];
  description = "Development-mode client for blaze-react";
  license = stdenv.lib.licenses.mit;
}
