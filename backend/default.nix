{ mkDerivation, aeson, base, bytestring, data-default, fast-logger
, file-embed, hpack, http-client, http-client-tls, lens
, monad-logger, stdenv, template-haskell, wai, wai-extra
, wai-logger, warp, webapp-common, yaml, yesod, yesod-core
, yesod-static
}:
mkDerivation {
  pname = "webapp-backend";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring data-default fast-logger file-embed
    http-client http-client-tls lens monad-logger template-haskell wai
    wai-extra wai-logger warp webapp-common yaml yesod yesod-core
    yesod-static
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring data-default fast-logger file-embed
    http-client http-client-tls lens monad-logger template-haskell wai
    wai-extra wai-logger warp webapp-common yaml yesod yesod-core
    yesod-static
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/saschagrunert/webapp.hs#readme";
  description = "An approach in bringing yesod and miso together";
  license = stdenv.lib.licenses.mit;
}
