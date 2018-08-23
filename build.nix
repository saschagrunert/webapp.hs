{ compiler ? "ghc843" }:
let
  bootstrap = import <nixpkgs> { };
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              webapp-common = haskellPackagesNew.callPackage ./default.nix { };
              webapp-backend = haskellPackagesNew.callPackage ./backend/default.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import src { inherit config; };

in {
  webapp-common = pkgs.haskell.packages.${compiler}.webapp-common;
  webapp-backend = pkgs.haskell.packages.${compiler}.webapp-backend;
}
