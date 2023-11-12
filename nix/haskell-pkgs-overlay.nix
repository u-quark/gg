{ inputs, lib, haskellLib, callCabal2nix }:

with haskellLib;

let modifyPkg = pkg: dontHaddock (disableExecutableProfiling (disableLibraryProfiling (doJailbreak (dontCheck pkg)))); in

self: super:
rec {
  vty = modifyPkg (callCabal2nix "vty" inputs.vty {});
  vty-unix = modifyPkg (callCabal2nix "vty-unix" inputs.vty-unix {
    vty = vty;
  });
  vty-crossplatform = modifyPkg (callCabal2nix "vty-crossplatform" inputs.vty-crossplatform {
    vty = vty;
    vty-unix = vty-unix;
  });
  brick = modifyPkg (callCabal2nix "brick" inputs.brick {
    vty = vty;
    vty-unix = vty-unix;
    vty-crossplatform = vty-crossplatform;
  });
}
