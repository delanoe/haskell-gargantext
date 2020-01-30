{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "gargantext";
  buildInputs = [
    docker-compose
    blas
    bzip2
    gfortran
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
