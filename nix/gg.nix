{
  mkDerivation,
  stdenv,
  nix-gitignore,
  # Build deps
  hpack,
  c2hs,
  # Haskell deps
  base,
  time,
  brick,
  vty,
  lens,
  generic-lens,
  vector,
  process,
  bytestring,
  MissingH,
  # C deps
  libgit2,
  # Transitive Haskell C deps
  ncurses
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
    # Haskell deps
    base
    time
    brick
    vty
    lens
    generic-lens
    vector
    process
    bytestring
    MissingH
    # C deps
    libgit2
    # Transitive Haskell C deps
    ncurses
  ];
  buildDepends = [ hpack c2hs ];
  homepage = "https://github.com/u-quark/gg#readme";
  license = stdenv.lib.licenses.bsd3;
}
