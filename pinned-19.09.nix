# this version of nixpkgs contains liblapack at 3.8.0
# this version of nixpkgs contains gsl at 2.5.0
import (
  builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz";
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  }
)
