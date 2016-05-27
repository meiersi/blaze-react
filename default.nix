# Convenient re-exports of the build functions for the individual cabal
# packages constituting blaze-react.
{ pkgs ? import <nixpkgs> {} }:
let
  # Hardcoding the version of nixpkgs to make it easier to try out
  # this branch, since it needs a very recent version.
  # We can remove this later.
  nixpkgs = import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "38fa633b3aa7f239de0cb8e7554b1a2539257df6";
    sha256 = "1dym1zkzspaqv80abrzqnxi81icbmn1xg0idi1315zdxj54hd0f0";
  }) {};

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
        sha256 = "00gchcy1smygsmdl9l7kd67ysfy6f3cr93v2wqb9m9j5haxvwhya";
        rev    = "ca958c8cb31e5ed7fbdee394bb3cafc978022c7b";
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
      preBuild = ''
        sed -e "s/JSRef/JSVal/g" -i src/Servant/Common/Req.hs
      '';
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

  shims = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "shims";
    rev = "356acca65861e8f250d569f9e2f3676fd2827e98";
    sha256 = "166iva95mvagzds3adigr7g6kfxwza25pqdjfn3s6cd2970midx5";
  };

  ghcjsBoot = nixpkgs.fetchgit {
    url = git://github.com/ghcjs/ghcjs-boot.git;
    rev = "ad1db0f6a2ccfd5f9e21f549c0868bded340b0b9";
    sha256 = "0d3xiw3r7vkb33fbnxn1s6xlvn5q9izq3x5cxq0259g1si3j3shy";
    fetchSubmodules = true;
  };

  ghcjs = nixpkgs.haskell.compiler.ghcjs.override { inherit shims ghcjsBoot; };

  ghcjs' = nixpkgs.haskell.lib.overrideCabal ghcjs (oldAttrs: {
    src = nixpkgs.fetchFromGitHub {
      owner = "ghcjs";
      repo = "ghcjs";
      rev = "eff0443c3fb188997807431fe52b9ce1a89694b2";
      sha256 = "18q5sdgdq6fgmfbjnzirpl8k1m72b6rdspc992sk6lsyihs5scyx";
    };
    preConfigure = ''
      sed -e "s/lens.*/lens,/" -i ghcjs.cabal
      sed -e "s/vector.*/vector,/" -i ghcjs.cabal
    '';
  });

  ghcjs-packages = nixpkgs.haskell.packages.ghcjs.override { ghc = ghcjs'; };

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
    ghc   = extendHaskellPackages nixpkgs.haskell.packages.ghc7102;
    ghcjs = extendHaskellPackages ghcjs-packages;
  };
in
{
  # Apps and libs are intended for downstream inclusion in the nix files of
  # other.
  inherit libs;
  inherit apps;

  inherit assembled;
}
