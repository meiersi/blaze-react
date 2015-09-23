{ mkDerivation, base, blaze-react-core, bytestring, either
, ghcjs-base, ghcjs-prim, hashable, lens, mtl, pretty-show
, profunctors, QuickCheck, stdenv, testing-feat, text, time
, transformers, unordered-containers, void
, ghcjs-servant-client, blaze-react-dev-mode-shared
}:
mkDerivation {
  pname = "blaze-react-spa";
  version = "0.2.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-react-core bytestring either ghcjs-base ghcjs-prim
    hashable lens mtl pretty-show profunctors QuickCheck testing-feat
    text time transformers unordered-containers void
    ghcjs-servant-client blaze-react-dev-mode-shared
  ];
  description = "Run blaze-react apps as single-page apps using ReactJS";
  license = stdenv.lib.licenses.mit;
}
