{ pkgs }:

with pkgs;

{
  pybase16-builder = python37Packages.callPackage (import ./pybase16-builder.nix) {};
}
