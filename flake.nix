{
  description = "gg - git (G)UI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=9e49f8f1f37bc906cda1adb33064c325d760819a";
    static-haskell = {
      url = "github:nh2/static-haskell-nix";
      flake = false;
    };
    libgit2 = {
      url = "github:libgit2/libgit2?ref=v1.6.4";
      flake = false;
    };
    vty = {
      url = "github:u-quark/vty?ref=gg";
      flake = false;
    };
    brick = {
      url = "github:jtdaugherty/brick?rev=3d34ef115631700ab6d46088357f7e2ab13c424e";
      flake = false;
    };
    base16-schemes = {
      url = "github:tinted-theming/base16-schemes?rev=9a4002f78dd1094c123169da243680b2fda3fe69";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };


  outputs = { self, ... }@inputs: rec {
    package = import ./nix { inputs = inputs; };
    packages.x86_64-linux.gg = package.gg;
    packages.x86_64-linux.default = packages.x86_64-linux.gg;
    apps.x86_64-linux.gg = {
      type = "app";
      program = "${package.gg}/bin/gg";
    };
    apps.x86_64-linux.default = apps.x86_64-linux.gg;
    devShells.x86_64-linux.default = package.devShell;
  };
}
