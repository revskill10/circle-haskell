{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
	owner = "NixOS";
	repo = "nixpkgs";
	rev = "a0aeb23";
	sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  result = import (pkgs.fetchFromGitHub {
		rev = "c756a6771a2da437f874645b1930e12d27127650";
    sha256 = "06ra3imm65fxfw41nnns5hvhwpsr56kn13x27glv7g43vx8ny5y3";
    owner = "dmjio";
    repo = "miso";
  }) {};
	inherit (pkgs) closurecompiler;
	miso = result.miso-ghcjs;
in (pkgs.haskell.packages.ghcjs.callPackage ./home.nix {
		miso = miso;
	})
	