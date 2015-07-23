{ nixpkgs ? import <nixpkgs> {},haskellPackages ? (import <nixpkgs> {}).haskellPackages, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, either
      , monad-control, mtl, random, stdenv, text, time, transformers
      , uuid, free, shelly
      }:
      mkDerivation {
        pname = "ghpass";
        version = "0.1.0.0";
        src = ./src;
        isLibrary = false;
        isExecutable = true;
        buildTools = [ haskellPackages.cabal-install haskellPackages.happy haskellPackages.alex haskellPackages.hlint];
        buildDepends = [
          aeson base bytestring either monad-control mtl random text time
          transformers uuid shelly free
        ];
        homepage = "https://github.com/wfaler/ghpass";
        license = stdenv.lib.licenses.unfree;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
