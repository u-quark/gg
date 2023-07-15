{
  description = "gg - git (G)UI";

  inputs = {
    MissingH = {
      url = "github:haskell-hvr/missingh?ref=v1.4.2.0";
      flake = false;
    };
    base16-schemes = {
      url = "github:tinted-theming/base16-schemes?rev=9a4002f78dd1094c123169da243680b2fda3fe69";
      flake = false;
    };
    brick = {
      url = "github:jtdaugherty/brick?rev=3d34ef115631700ab6d46088357f7e2ab13c424e";
      flake = false;
    };
    libgit2 = {
      url = "github:libgit2/libgit2?ref=v1.6.4";
      flake = false;
    };
    nh2-nixpkgs = {
      url = "github:nh2/nixpkgs?rev=11aa987ea5b5a593c9ca7a38b391804959f905e5";
      flake = false;
    };
    static-haskell = {
      url = "github:nh2/static-haskell-nix?rev=e3823be4caba6917c7c9f9dde9bece8ed8a4179b";
      flake = false;
    };
    vty = {
      url = "github:u-quark/vty?rev=729636962937d6f3ae0979312d902beafa2ee56d";
      flake = false;
    };
    yesodweb-authenticate = {
      url = "github:yesodweb/authenticate?ref=authenticate-1.3.5";
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
