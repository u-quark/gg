{ inputs, haskellLib, callCabal2nix }:

with haskellLib;

let modifyPkg = pkg: dontHaddock (disableExecutableProfiling (disableLibraryProfiling (doJailbreak (dontCheck pkg)))); in

self: super:
{
  MissingH = modifyPkg (callCabal2nix "MissingH" inputs.MissingH {
    hslogger = self.hslogger;
    regex-compat = self.regex-compat;
  });
  vty = modifyPkg (callCabal2nix "vty" inputs.vty {
    microlens-th = self.microlens-th;
  });
  brick = modifyPkg (callCabal2nix "brick" inputs.brick {
    vty = self.vty;
    microlens-th = self.microlens-th;
  });
  hslogger = self.hslogger_1_3_1_0;
  microlens-th = self.microlens-th_0_4_3_2;
  regex-compat = self.regex-compat_0_95_2_0;
  RSA = self.RSA_2_4_1;
  authenticate-oauth = modifyPkg (callCabal2nix "authenticate-oauth" "${inputs.yesodweb-authenticate}/authenticate-oauth" {
    RSA = self.RSA;
  });
}
