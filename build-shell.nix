{ghc}:
with (import ./pinned.nix {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "gargantext";
  buildInputs = [
    docker-compose
    blas
    bzip2
    #gfortran
    gfortran.cc.lib
    glibc
    gmp
    gsl
    igraph
    liblapack
    pcre
    postgresql
    #stack
    xz
    zlib
  ];
}
