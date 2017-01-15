{ pkgs ? import <nixpkgs> {} }:

let
  stdenv = pkgs.stdenv;
  libPath = with pkgs; with xlibs; stdenv.lib.makeLibraryPath [
    libX11
    libXcursor
    libXi
    libXxf86vm
  ];
in
stdenv.mkDerivation {
  name = "ige";
  buildInputs = with pkgs; [
    gcc
    gdb
    pkgconfig
    cairo
    gnome2.pango
    gnumake
  ];
  shellHook = "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${libPath}";
}
