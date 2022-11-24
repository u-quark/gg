{
  mkDerivation,
  stdenv,
  # Build deps
  hpack,
  c2hs,
  # Haskell deps
  base,
  containers,
  time,
  directory,
  http-client,
  brick,
  vty,
  lens,
  generic-lens,
  vector,
  process,
  bytestring,
  MissingH,
  wreq,
  colour,
  # C deps
  libgit2,
  # Transitive Haskell C deps
  ncurses,
  zlib,
  # Generated
  haskell-base16-schemes
}:

mkDerivation {
  pname = "gg";
  version = "0.1.0.0";
  src = stdenv.lib.sourceByRegex ./.. [
    "app" "app/.*" "src" "src/.*" "test" "test/.*"
    "stack.yaml" "package.yaml" "Setup.hs" "LICENSE" "README.md" "ChangeLog.md"
  ];
  postUnpack = ''
    cp ${haskell-base16-schemes}/BuiltinColorSchemes.hs gg/src/GG/UI/
  '';
  isLibrary = false;
  isExecutable = true;
  enableSharedLibraries = false;
  enableSharedExecutables = false;
  preConfigure = "hpack";
  configureFlags = [
    "--ghc-option=-optl=-static"
    "--ghc-option=-optl=-pthread"
    "--extra-lib-dirs=${zlib.static}/lib"
  ];
  executableHaskellDepends = [
    # Haskell deps
    base
    containers
    time
    directory
    http-client
    brick
    vty
    lens
    generic-lens
    vector
    process
    bytestring
    MissingH
    wreq
    # C deps
    libgit2
    # Transitive Haskell C deps
    ncurses
  ];
  buildDepends = [ hpack c2hs ];
  homepage = "https://github.com/u-quark/gg#readme";
  license = stdenv.lib.licenses.gpl3Plus;
}

