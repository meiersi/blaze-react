{ mkDerivation, aeson, base, bytestring, either, hashable, lens
, mtl, profunctors, QuickCheck, stdenv, text, time, transformers
, unordered-containers, vector, void
}:
mkDerivation {
  pname = "blaze-react-core";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring either hashable lens mtl profunctors
    QuickCheck text time transformers unordered-containers vector void
  ];
  description = "Pure and core part of blaze-react";
  license = stdenv.lib.licenses.mit;
}
