{ mkDerivation, base, stdenv }:
mkDerivation {
  description = "An approach in bringing yesod and miso together";
  homepage = "https://github.com/saschagrunert/webapp.hs#readme";
  isExecutable = false;
  isLibrary = true;
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.mit;
  pname = "webapp";
  sha256 = "0gsaf1aj5r25ixx4svp8qaqcpbwv0ak41qi3jf6ly9czppmzdhz7";
  src = ./.;
  version = "0.1.0";
}
