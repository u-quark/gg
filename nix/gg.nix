{
  mkDerivation,
  lib,
  # Build deps
  hpack,
  c2hs,
  git,
  hecate,
  # Haskell deps
  base,
  containers,
  time,
  directory,
  http-client,
  brick,
  vty,
  vty-unix,
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
  # Generated
  haskell-base16-schemes
}:

mkDerivation {
  pname = "gg";
  version = "0.1.0.0";
  src = lib.sourceByRegex ./.. [
    "app" "app/.*" "src" "src/.*" "test" "test/.*" "e2e-tests" "e2e-tests/.*"
    "stack.yaml" "package.yaml" "Setup.hs" "LICENSE" "README.md" "ChangeLog.md"
  ];
  patchPhase = ''
    cp ${haskell-base16-schemes}/BuiltinColorSchemes.hs src/GG/UI/
  '';
  isLibrary = false;
  isExecutable = true;
  enableSharedLibraries = false;
  enableSharedExecutables = false;
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  dontStrip = false;
  preConfigure = "hpack";
  configureFlags = [
    "--ghc-option=-optl=-static"
    "--ghc-option=-optl=-pthread"
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
    vty-unix
    lens
    generic-lens
    vector
    process
    bytestring
    MissingH
    wreq
    colour
    # C deps
    libgit2
  ];
  buildDepends = [ hpack c2hs git hecate ];
  checkPhase = ''
    export GG_PATH="$(realpath ./dist/build/gg/gg)"
    (cd e2e-tests; pytest -vvv)
  '';
  homepage = "https://github.com/u-quark/gg#readme";
  license = lib.licenses.gpl3Plus;
}

