{ pkgs ? import <nixpkgs> { }, ... }:
let
  drv = pkgs.haskellPackages.callCabal2nix "backend" ./. { };

  drvWithTools = drv.env.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [
      pkgs.ghc
      pkgs.haskellPackages.haskell-language-server
      pkgs.cabal-install
    ];
    shellHook = ''
      source .env
    '';
  });
in
  {
    inherit drv;
    inherit drvWithTools;
  }
