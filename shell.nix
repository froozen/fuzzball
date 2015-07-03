with (import <nixpkgs> {}).pkgs;
let
    ghc = haskellPackages.ghcWithPackages (pkgs:
        with pkgs; []
    );
in
    stdenv.mkDerivation {
        name = "fuzzy-env";
        buildInputs = [ ghc haskellPackages.cabalInstall ];
        shellHook = "eval $(grep export ${ghc}/bin/ghc)";
    }