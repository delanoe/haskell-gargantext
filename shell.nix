{ pkgs ? import ./nix/pkgs.nix {} }:
let 
  myBuildInputs = [
    pkgs.pkgs.docker-compose
    pkgs.pkgs.haskell-language-server
    pkgs.pkgs.stack
  ];
in  
pkgs.pkgs.mkShell {
  name = pkgs.shell.name;
  shellHook = pkgs.shell.shellHook;
  buildInputs = pkgs.shell.buildInputs ++ myBuildInputs;
}
