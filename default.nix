# Convenient re-exports of the build functions for the individual cabal
# packages constituting blaze-react.
{ nixpkgs ? (import <nixpkgs> {}) }:
let
  # Individual packages
  #####################

  # NOTE (SM): we might want to move that into the repo itself in the future.
  # For now I'm cobbling it together here.
  ghcjs-servant-client =
    { mkDerivation, aeson, attoparsec, base, bytestring
    , case-insensitive, deepseq, either, exceptions
    , ghcjs-base, hspec, http-media, http-types, HUnit, network
    , network-uri, QuickCheck, safe, servant, servant-client
    , servant-server, split, stdenv, string-conversions, text
    , transformers, wai, warp
    }:
    mkDerivation {
      pname = "ghcjs-servant-client";
      version = "0.4.2";
      src = nixpkgs.fetchFromGitHub {
        owner  = "meiersi";
        repo   = "ghcjs-servant-client";
        sha256 = "0iwsxw6qn4icd13kc6msm8g2zhj1dh9kfv8dr64vk171pl9mqqlr";
        rev    = "2ef35c92c7b6402f07d38d0d0f5ec42cad25f883";
      };
      libraryHaskellDepends = [
        aeson attoparsec base bytestring case-insensitive either exceptions
        ghcjs-base http-media http-types network-uri safe servant split
        string-conversions text transformers
      ];
      testHaskellDepends = [
        aeson base bytestring deepseq either hspec http-media http-types
        HUnit network QuickCheck servant servant-client servant-server text
        wai warp
      ];
      homepage = "http://haskell-servant.github.io/";
      description = "automatical derivation of querying functions for servant webservices";
      license = stdenv.lib.licenses.bsd3;
    };


  libs = {
    inherit ghcjs-servant-client;
    servant-blaze-react         = import libs/hs/servant-blaze-react;

    blaze-react-core            = import libs/hs/blaze-react-core;
    blaze-react-dev-mode-shared = import libs/hs/blaze-react-dev-mode-shared;
    blaze-react-dev-mode-server = import libs/hs/blaze-react-dev-mode-server;
    blaze-react-dev-mode-client = import libs/hs/blaze-react-dev-mode-client;
    blaze-react-spa             = import libs/hs/blaze-react-spa;

    # TODO (SM): make this build
    blaze-react-examples        = import libs/hs/blaze-react-examples;
  };

  apps = {
    # TODO (SM): make this build
    blaze-react-demo            = import apps/hs/blaze-react-example;
  };

  # Assembled packages for testing and local building
  assembled =
  let
      dontHaddock = nixpkgs.pkgs.haskell.lib.dontHaddock;

      extendHaskellPackages = pkgs: pkgs.override {
          overrides = self: super:
            let
                buildPackage = name: pkg:
                    dontHaddock (self.callPackage pkg {});
                buildPackages = nixpkgs.lib.mapAttrs buildPackage;

            in buildPackages libs // buildPackages apps;
      };

  in {
    ghc   = extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc7102;
    ghcjs = extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghcjs;
  };
in
{
  # Apps and libs are intended for downstream inclusion in the nix files of
  # other.
  inherit libs;
  inherit apps;

  inherit assembled;
}
