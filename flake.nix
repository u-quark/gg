{
  description = "gg - git (G)UI";

  inputs = {
    nixpkgs.url = "github:u-quark/nixpkgs?ref=gg";
    static-haskell = {
      url = "github:u-quark/static-haskell-nix?ref=gg";
      flake = false;
    };
    libgit2 = {
      url = "github:libgit2/libgit2?ref=v1.7.1";
      flake = false;
    };
    vty = {
      url = "github:u-quark/vty?ref=gg";
      flake = false;
    };
    brick = {
      url = "github:jtdaugherty/brick";
      flake = false;
    };
    base16-schemes = {
      url = "github:tinted-theming/base16-schemes";
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
