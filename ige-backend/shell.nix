{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, binary-conduit, bytestring
      , conduit, conduit-combinators, conduit-extra
      , network, optparse-applicative, stdenv, text, unix
      , binary, containers, data-binary-ieee754, deepseq, groom
      , hashable, hspec, QuickCheck, unordered-containers, vector, void
      }:
      let
        data-msgpack = import ./hs-msgpack {
          inherit mkDerivation;
          inherit base;
          inherit binary;
          inherit bytestring;
          inherit containers;
          inherit data-binary-ieee754;
          inherit deepseq;
          inherit groom;
          inherit hashable;
          inherit hspec;
          inherit QuickCheck;
          inherit stdenv;
          inherit text;
          inherit unordered-containers;
          inherit vector;
          inherit void;
        };
      in
      mkDerivation {
        pname = "ige-backend";
        version = "0.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base binary-conduit bytestring conduit conduit-combinators
          conduit-extra data-msgpack network optparse-applicative text unix
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
