{ pkgs }:

with pkgs;

{
  pybase16-builder = python311Packages.callPackage (import ./pybase16-builder.nix) {};
}
