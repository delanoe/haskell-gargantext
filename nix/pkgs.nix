{ pkgs ? import ./pinned-21.05.nix {} }:

rec {
  inherit pkgs;
  ghc = pkgs.haskell.compiler.ghc8104;
  hsBuildInputs = [
    ghc
    pkgs.cabal-install
  ];
  nonhsBuildInputs = with pkgs; [
    bzip2
    git
    gmp
    gsl
    igraph
    liblapack
    lzma
    pcre
    pkgconfig
    postgresql
    xz
    zlib
    blas
    gfortran7
    gfortran7.cc.lib
  ];
  libPaths = pkgs.lib.makeLibraryPath nonhsBuildInputs;
  shellHook = ''
    export LD_LIBRARY_PATH="${libPaths}"
    export LIBRARY_PATH="${libPaths}"
  '';
  shell = pkgs.mkShell {
    name = "gargantext-shell";
    buildInputs = hsBuildInputs ++ nonhsBuildInputs;
    inherit shellHook;
  };
}
