# this version of nixpkgs contains liblapack at 3.8.0
# this version of nixpkgs contains gsl at 2.5.0
import (
  builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz";
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  }
)
