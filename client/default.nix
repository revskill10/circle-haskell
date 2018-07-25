{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
	owner = "NixOS";
	repo = "nixpkgs";
	rev = "a0aeb23";
	sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  result = import (pkgs.fetchFromGitHub {
		owner = "dmjio";
		repo = "miso";
		sha256 = "15n813j9h8lszg8b0491s2nhpn3k37910h0hggc576rmdk74db5c";
		rev = "c0a3ec5f6309cdff2b732507f6ce9db992da3cd3";
  }) {};
	inherit (pkgs) runCommand closurecompiler;
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
}