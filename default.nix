with (import <nixpkgs> {});
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv {
    name = name;
    paths = buildInputs;
  };
  buildInputs = [
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
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup
    touch $out
  '';
}
