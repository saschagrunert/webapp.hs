{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hpack, stdenv }:
      mkDerivation {
        pname = "webapp";
        version = "0.1.0";
        src = ./.;
        libraryHaskellDepends = [ base ];
        libraryToolDepends = [ hpack ];
        preConfigure = "hpack";
        homepage = "https://github.com/saschagrunert/webapp.hs#readme";
        description = "An approach in bringing yesod and miso together";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
