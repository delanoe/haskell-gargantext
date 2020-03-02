{ pkgs ? import ./pinned.nix {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    docker-compose
    #glibc
    #gmp
    #gsl
    #igraph
    #pcre
    #postgresql
    #stack
    #xz
  ];
}
