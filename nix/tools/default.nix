{ pkgs, inputs }:

with pkgs;

{
  pybase16-builder = python311Packages.callPackage (import ./pybase16-builder.nix) {};
  hecate = python311Packages.callPackage (import ./hecate.nix) { inherit inputs; };
}
