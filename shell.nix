{ pkgs ? import ./pinned-19.09.nix {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    docker-compose
    #glibc
    #gmp
    #gsl
    haskell-language-server
    #igraph
    lorri
    #pcre
    #postgresql
    stack
    #xz
  ];
}
