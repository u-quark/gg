{ inputs }:

let
  ghc = "ghc881";
  pkgs = import inputs.nh2-nixpkgs { system = "x86_64-linux"; };
  static-pkgs-overlay = import ./static-pkgs-overlay.nix {
    inherit inputs;
    inherit pkgs;
  };
  haskell-pkgs-overlay = import ./haskell-pkgs-overlay.nix {
    inherit inputs;
    haskellLib = pkgs.haskell.lib;
    callCabal2nix = pkgs.haskell.packages."${ghc}".callCabal2nix;
  };
  haskellPkgsExtend = hp: f: hp.override (oldArgs: {
    overrides = pkgs.lib.composeExtensions (oldArgs.overrides or (_: _: {})) f;
  });
  patched-pkgs = pkgs.extend (self_1: super_1: {
    pkgsMusl = (super_1.pkgsMusl.extend static-pkgs-overlay).extend (self_2: super_2: {
      haskell = pkgs.lib.recursiveUpdate super_2.haskell {
        packages."${ghc}" = haskellPkgsExtend super_2.haskell.packages."${ghc}" haskell-pkgs-overlay;
      };
    });
  });
  static-haskell = import (inputs.static-haskell + "/survey") {
    normalPkgs = patched-pkgs;
    compiler = "${ghc}";
  };
  tools = import ./tools { inherit pkgs; };
  haskell-base16-schemes = import ./haskell-base16-schemes.nix {
    inherit pkgs;
    pybase16-builder = tools.pybase16-builder;
    base16-schemes = inputs.base16-schemes;
  };
  gg = static-haskell.haskellPackagesWithLibsReadyForStaticLinking.callPackage (import ./gg.nix) {
    ncurses = patched-pkgs.pkgsMusl.static-ncurses;
    zlib = patched-pkgs.pkgsMusl.zlib;
    inherit haskell-base16-schemes;
  };
in
{
  inherit gg;
}
