{ sources, pkgs }:

self: super:
{
  http-parser = super.http-parser.overrideAttrs (
    old: {
      buildFlags = "library";
      postBuild = ''
        ar rcs libhttp_parser.a libhttp_parser.o
      '';
      postInstall = ''
        cp libhttp_parser.a $out/lib/
      '';
      nativeBuildInputs = [ pkgs.binutils ];
    }
  );
  # We cannot override regular ncurses because all the tools
  # ghc etc depend on this and then nothing works!
  static-ncurses = super.ncurses.override {
    enableStatic = true;
  };
  libgit2 = super.libgit2.overrideAttrs (old: {
    src = sources.libgit2;
    version = sources.libgit2.version;
    cmakeFlags = old.cmakeFlags ++ [ "-DBUILD_SHARED_LIBS=OFF" ];
    nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.binutils ];
    postBuild = ''
      mv libgit2.a libgit2_original.a
      ar -M <<EOM
        CREATE libgit2.a
        ADDLIB libgit2_original.a
        ADDLIB ${self.zlib_both}/lib/libz.a
        ADDLIB ${self.nghttp2.lib}/lib/libnghttp2.a
        ADDLIB ${self.curl.out}/lib/libcurl.a
        ADDLIB ${self.libssh2.out}/lib/libssh2.a
        ADDLIB ${self.openssl.out}/lib/libssl.a
        ADDLIB ${self.openssl.out}/lib/libcrypto.a
        ADDLIB ${self.http-parser}/lib/libhttp_parser.a
        SAVE
        END
      EOM
      ar -s libgit2.a
      rm libgit2_original.a
    '';
  });
}
