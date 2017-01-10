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
  name = "rust-opengl";
  buildInputs = with pkgs; [
    rustc
    cargo
    cmake
    gcc
    gdb
    linuxPackages.perf
    pkgconfig
  ];
  shellHook = "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${libPath}";

  RUST_BACKTRACE=1;
  RUST_SRC_PATH="${pkgs.rustc.src}";
}
