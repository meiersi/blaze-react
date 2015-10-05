{ mkDerivation, base, blaze-react-core, blaze-react-dev-mode-shared
, bytestring, either, ghcjs-base, ghcjs-prim, ghcjs-servant-client
, hashable, lens, mtl, pretty-show, profunctors, QuickCheck
, servant, stdenv, testing-feat, text, time, transformers
, unordered-containers, void
}:
mkDerivation {
  pname = "blaze-react-spa";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base blaze-react-core bytestring either ghcjs-base ghcjs-prim
    hashable lens mtl pretty-show profunctors QuickCheck testing-feat
    text time transformers unordered-containers void
  ];
  executableHaskellDepends = [
    base blaze-react-core blaze-react-dev-mode-shared either ghcjs-base
    ghcjs-servant-client servant text
  ];
  description = "Run blaze-react apps as single-page apps using ReactJS";
  license = stdenv.lib.licenses.mit;
}
