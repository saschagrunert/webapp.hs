{ mkDerivation, base, hpack, stdenv }:
mkDerivation {
  pname = "webapp-common";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  homepage = "https://github.com/saschagrunert/webapp.hs#readme";
  description = "An approach in bringing yesod and miso together";
  license = stdenv.lib.licenses.mit;
}
