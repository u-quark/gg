{ inputs, lib, haskellLib, callCabal2nix }:

with haskellLib;

let modifyPkg = pkg: dontHaddock (disableExecutableProfiling (disableLibraryProfiling (doJailbreak (dontCheck pkg)))); in

self: super:
rec {
  vty = modifyPkg (callCabal2nix "vty" inputs.vty {});
  brick = modifyPkg (callCabal2nix "brick" inputs.brick {
    vty = vty;
  });
}
