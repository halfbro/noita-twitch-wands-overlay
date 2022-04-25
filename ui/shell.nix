{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = [
    pkgs.elm2nix
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.zlib
  ];
}
