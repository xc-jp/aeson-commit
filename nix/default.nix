{ nixpkgs ? import <nixpkgs> {}, compiler ? null }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == null
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  Diff = haskellPackages.callPackage ./Diff.nix {};
  aeson = haskellPackages.callPackage ./aeson.nix { inherit Diff; };
  aeson-qq = haskellPackages.aeson-qq.override { inherit aeson; };

  f = { mkDerivation, base, containers, hspec, mtl
      , stdenv, tasty, tasty-hspec, text, transformers
      }:
      mkDerivation {
        pname = "aeson-commit";
        version = "0.1.0.0";
        src = ../.;
        libraryHaskellDepends = [ aeson base mtl text ];
        testHaskellDepends = [
          aeson aeson-qq base containers hspec mtl tasty tasty-hspec text
          transformers
        ];
        description = "Parse Aeson data with commitment";
        license = stdenv.lib.licenses.bsd3;
      };

  aeson-commit = haskellPackages.callPackage f {};

in

# pkgs // { inherit aeson-commit; }

aeson-commit
