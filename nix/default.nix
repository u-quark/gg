let
  ghc = "ghc881";
  sources = import ./sources.nix;
  pkgs = import sources.nh2-nixpkgs {};
  static-pkgs-overlay = import ./static-pkgs-overlay.nix {
    inherit sources;
    inherit pkgs;
  };
  haskell-pkgs-overlay = import ./haskell-pkgs-overlay.nix {
    inherit sources;
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
  static-haskell = import (sources.static-haskell + "/survey") {
    normalPkgs = patched-pkgs;
    compiler = "${ghc}";
  };
  gg = static-haskell.haskellPackagesWithLibsReadyForStaticLinking.callPackage (import ./gg.nix) {
    ncurses = patched-pkgs.pkgsMusl.static-ncurses;
  };
in
{
  inherit gg;
}
