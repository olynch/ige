{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, array, base, bytestring, cairo
      , conduit, conduit-combinators, containers, directory, fgl, gtk
      , linear, megaparsec, microlens-platform, mtl, mwc-random
      , optparse-applicative, protolude, stdenv, stm, stm-chans
      , stm-conduit, text, transformers, wl-pprint-text
      }:
      mkDerivation {
        pname = "ige";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson array base bytestring cairo conduit conduit-combinators
          containers directory fgl gtk linear megaparsec microlens-platform
          mtl mwc-random optparse-applicative protolude stm stm-chans
          stm-conduit text transformers wl-pprint-text
        ];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
