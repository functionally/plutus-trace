{
  system ? builtins.currentSystem
, config ? { allowUnfreePredicate = (import plutus/lib.nix).unfreePredicate; }
, packages ? import plutus/nix { inherit system config ; }
}:

let

  inherit (packages) pkgs;
  inherit (packages.pkgsLocal) haskell;

  ghc = haskell.packages.ghcWithPackages (ps: with ps; [
    marlowe-playground-server
    plutus-playground-server
  ]);

in

  pkgs.stdenv.mkDerivation {
    name = "plutus-trace-env";
    buildInputs = [
      pkgs.cabal-install
      ghc
    ];
  }
