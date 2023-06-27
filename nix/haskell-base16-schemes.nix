{
  pkgs,
  pybase16-builder,
  base16-schemes
}:

with pkgs;

stdenv.mkDerivation {
  name = "gg-haskell-base16-schemes";

  src = stdenv.lib.sourceByRegex ../base16-haskell-template [".*"];

  buildInputs = [ base16-schemes pybase16-builder ];
  buildPhase = ''
    for scheme in ${base16-schemes}/*yaml; do
        scheme_name=$(basename $scheme)
        scheme_name=''${scheme_name%.*}
        mkdir -p schemes/$scheme_name
        cp $scheme schemes/$scheme_name/
    done
    pybase16 build --template gg-haskell --output output
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
