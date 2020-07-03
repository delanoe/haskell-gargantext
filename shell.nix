{ pkgs ? import ./pinned-19.09.nix {} }:

let
  hie = (import (pkgs.fetchFromGitHub {
                   owner="domenkozar";
                   repo="hie-nix";
                   rev="e3113da";
                   sha256="05rkzjvzywsg66iafm84xgjlkf27yfbagrdcb8sc9fd59hrzyiqk";
                 }) {}).hie84;
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    docker-compose
    #hie
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
