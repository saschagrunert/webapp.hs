{ mkDerivation, base, hpack, miso, stdenv, webapp-common }:
mkDerivation {
  pname = "webapp-frontend";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base miso webapp-common ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base miso webapp-common ];
  preConfigure = "hpack";
  homepage = "https://github.com/saschagrunert/webapp.hs#readme";
  description = "An approach in bringing yesod and miso together";
  license = stdenv.lib.licenses.mit;
}
