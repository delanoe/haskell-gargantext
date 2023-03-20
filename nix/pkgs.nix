{ pkgs ? import ./pinned-22.05.nix {} }:

rec {
  inherit pkgs;
  ghc = pkgs.haskell.compiler.ghc8107;
  igraph_0_10_4 = pkgs.igraph.overrideAttrs (finalAttrs: previousAttrs: {
    version = "0.10.4";

    src = pkgs.fetchFromGitHub {
      owner = "igraph";
      repo = "igraph";
      rev = "0.10.4";
      hash = "sha256-LsTOxUktGZcp46Ec9QH3+9C+VADMYTZZCjKF1gp36xk=";
    };

    postPatch = ''
      echo "0.10.4" > IGRAPH_VERSION
    '';

    outputs = [ "out" "doc" ];

    buildInputs = [
      pkgs.arpack
      pkgs.blas
      pkgs.glpk
      pkgs.gmp
      pkgs.lapack
      pkgs.libxml2
      pkgs.plfit
    ] ++ pkgs.lib.optionals pkgs.stdenv.cc.isClang [
      pkgs.llvmPackages.openmp
    ];

    cmakeFlags = [
      "-DIGRAPH_USE_INTERNAL_BLAS=OFF"
      "-DIGRAPH_USE_INTERNAL_LAPACK=OFF"
      "-DIGRAPH_USE_INTERNAL_ARPACK=OFF"
      "-DIGRAPH_USE_INTERNAL_GLPK=OFF"
      "-DIGRAPH_USE_INTERNAL_GMP=OFF"
      "-DIGRAPH_USE_INTERNAL_PLFIT=OFF"
      "-DIGRAPH_GLPK_SUPPORT=ON"
      "-DIGRAPH_GRAPHML_SUPPORT=ON"
      "-DIGRAPH_OPENMP_SUPPORT=ON"
      "-DIGRAPH_ENABLE_LTO=AUTO"
      "-DIGRAPH_ENABLE_TLS=ON"
      "-DBUILD_SHARED_LIBS=ON"
    ];

    postInstall = ''
      mkdir -p "$out/share"
      cp -r doc "$out/share"
    '';

    postFixup = previousAttrs.postFixup + ''
      CUR_DIR=$PWD
      cd "$out/include/igraph" && cp *.h ../
      cd $CUR_DIR
    '';

  });
  hsBuildInputs = [
    ghc
    pkgs.cabal-install
  ];
  nonhsBuildInputs = with pkgs; [
    bzip2
    czmq
    docker-compose
    git
    gmp
    gsl
    #haskell-language-server
    hlint
    igraph_0_10_4
    libffi
    liblapack
    lzma
    pcre
    pkgconfig
    postgresql
    xz
    zlib
    blas
    gfortran7
    #    gfortran7.cc.lib
    expat
    icu
    graphviz
    llvm_9
  ] ++ ( lib.optionals stdenv.isDarwin [
       darwin.apple_sdk.frameworks.Accelerate
       ]);
  libPaths = pkgs.lib.makeLibraryPath nonhsBuildInputs;
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.gfortran7.cc.lib}:${libPaths}:$LD_LIBRARY_PATH"
    export LIBRARY_PATH="${pkgs.gfortran7.cc.lib}:${libPaths}"
  '';
  shell = pkgs.mkShell {
    name = "gargantext-shell";
    buildInputs = hsBuildInputs ++ nonhsBuildInputs;
    inherit shellHook;
  };
}
