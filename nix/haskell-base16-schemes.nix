{
  pkgs,
  pybase16-builder,
  base16-nix
}:

with pkgs;

let schemes = lib.mapAttrsToList (_name: value: fetchgit value) (lib.importJSON "${base16-nix}/schemes.json");
in

stdenv.mkDerivation {
  name = "gg-haskell-base16-schemes";

  paths = schemes;
  src = stdenv.lib.sourceByRegex ../base16-haskell-template [".*"];

  buildInputs = [ schemes pybase16-builder ];
  buildPhase = ''
    mkdir schemes
    for s in ${lib.escapeShellArgs schemes}; do
      ln -s $s schemes/$(basename $s)
    done
    pybase16 build -t gg-haskell -o output
    cat haskell-prologue >>BuiltinColorSchemes.hs
    local first_time;
    first_time='true'
    for s in output/gg-haskell/src/*.hs; do
      if ! $first_time; then
        echo '  ,' >>BuiltinColorSchemes.hs
      fi
      cat $s >>BuiltinColorSchemes.hs
      first_time='false'
    done
    cat haskell-epilogue >>BuiltinColorSchemes.hs
  '';

  installPhase = ''
    mkdir $out
    mv BuiltinColorSchemes.hs $out/
  '';
}
