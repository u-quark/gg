{
  base,
  brick,
  bytestring,
  c2hs,
  generic-lens,
  libgit2,
  hpack,
  lens,
  MissingH,
  mkDerivation,
  ncurses,
  nix-gitignore,
  process,
  stdenv,
  time,
  vector,
  vty
}:

mkDerivation {
  pname = "gg";
  version = "0.1.0.0";
  src = nix-gitignore.gitignoreSource [ ".git/*" "nix/*" "*.nix" ] ./..;
  isLibrary = false;
  isExecutable = true;
  enableSharedLibraries = false;
  enableSharedExecutables = false;
  preConfigure = "hpack";
  configureFlags = [
    "--ghc-option=-optl=-static"
    "--ghc-option=-optl=-pthread"
  ];
  executableHaskellDepends = [
    base
    brick
    bytestring
    generic-lens
    lens
    MissingH
    process
    time
    vector
    vty
    libgit2
    ncurses
  ];
  buildDepends = [ hpack c2hs ];
  homepage = "https://github.com/u-quark/gg#readme";
  license = stdenv.lib.licenses.bsd3;
}

