{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, cereal
      , cereal-conduit, conduit, conduit-combinators, conduit-extra
      , messagepack, network, optparse-applicative, stdenv, text, unix
      }:
      mkDerivation {
        pname = "pipes-hs";
        version = "0.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base bytestring cereal cereal-conduit conduit
          conduit-combinators conduit-extra messagepack network
          optparse-applicative text unix
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
