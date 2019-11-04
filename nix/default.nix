let
  sources = import ./sources.nix;
  nh2-pkgs = import sources.nh2-nixpkgs {};
  overlay = import ./static-pkgs-overlay.nix { pkgs = nh2-pkgs; };
  pkgsStatic = nh2-pkgs.pkgsStatic.extend overlay;
  pkgs = nh2-pkgs // { pkgsStatic = pkgsStatic; };
  static-haskell = import (sources.static-haskell + "/survey") {
    normalPkgs = pkgs;
    compiler = "ghc865";
  };
  static-haskell-pkgs = static-haskell.haskellPackagesWithLibsReadyForStaticLinking;
  gg = static-haskell-pkgs.callPackage (import ./gg.nix) {
    libgit2 = pkgsStatic.libgit2;
    ncurses = pkgsStatic.ncurses;
  };
  shell = pkgs.stdenv.mkDerivation {
    name = "gg-environment";
    buildInputs = [ pkgs.nix ];
  };
in
{
  inherit shell;
  inherit gg;
}
