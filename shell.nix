{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, array, base, bytestring, cairo
      , conduit, conduit-combinators, containers, directory, fgl, gtk
      , linear, megaparsec, microlens-platform, mtl, mwc-random
      , protolude, stdenv, stm, stm-chans, stm-conduit, text
      , transformers, wl-pprint-text
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
          mtl mwc-random protolude stm stm-chans stm-conduit text
          transformers wl-pprint-text
        ];
        homepage = "https://github.com/olynch/ige";
        description = "An keyboard-driven interactive graph editor";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
