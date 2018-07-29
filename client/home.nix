{ mkDerivation, base, lens, miso, stdenv, aeson }:
mkDerivation {
  pname = "home";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base lens miso aeson ];
  description = "Home";
  license = stdenv.lib.licenses.unlicense;
  hydraPlatforms = stdenv.lib.platforms.none;
}