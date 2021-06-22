# this version of nixpkgs contains liblapack at ?
# this version of nixpkgs contains gsl at ?
import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-20.09";
  url = "https://github.com/nixos/nixpkgs/";
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-20.09`
  ref = "refs/heads/nixos-20.09";
  rev = "69f3a9705014ce75b0489404210995fb6f29836e";
})
