{ ghc
, pkgs ? import ./pinned.nix {}
}:
let
  buildInputs = with pkgs; [
    bzip2
    gmp
    gsl
    igraph
    liblapack
    lzma
    pcre
    postgresql
    xz
    zlib
    blas
    gfortran7
    gfortran7.cc.lib
  ];

  libraryPaths = pkgs.lib.makeLibraryPath buildInputs;
in
pkgs.haskell.lib.buildStackProject rec {
  inherit ghc;
  inherit buildInputs;
  name = "gargantext";

  shellHook = ''
    export LD_LIBRARY_PATH="${libraryPaths}"
    export LIBRARY_PATH="${libraryPaths}"
  '';
}
