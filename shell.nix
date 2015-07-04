with (import <nixpkgs> {}).pkgs;
let
    ghc = haskellPackages.ghcWithPackages (pkgs:
        with pkgs; [
            ncurses
            transformers_0_4_1_0
            (lib.overrideDerivation mtl_2_2_1 (attrs:
                {
                  buildDepends = [ transformers_0_4_1_0 ];
                  buildInputs = [ transformers_0_4_1_0 ];
                }))
            ]
    );
in
    stdenv.mkDerivation {
        name = "fuzzy-env";
        buildInputs = [ ghc haskellPackages.cabalInstall ];
        shellHook = "eval $(grep export ${ghc}/bin/ghc)";
    }
