let ourpkgs = import ./pkgs.nix {};
    pkgs = ourpkgs.pkgs;
in
pkgs.haskell.lib.buildStackProject rec {
  name = "gargantext";
  ghc = ourpkgs.ghc;
  buildInputs = ourpkgs.nonhsBuildInputs;
  shellHook = ourpkgs.shellHook;
}
