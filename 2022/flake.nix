{
  description = "Advent of code solutions";

  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      haskellPackages = pkgs.haskell.packages.ghc925.override {
        overrides = final: prev: {
          ListLike = pkgs.haskell.lib.dontCheck prev.ListLike;
          aoc-2022 = final.callCabal2nix "aoc-2022" "${self}/2022" { };
        };
      };
      projectGhc = haskellPackages.ghcWithHoogle (_:
        haskellPackages.aoc-2022.getBuildInputs.haskellBuildInputs
      );
    in
    {
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          # Nix tooling
          pkgs.nixpkgs-fmt
          pkgs.nix-linter

          # Haskell tooling
          projectGhc
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server

          # Rust tooling
          pkgs.cargo
          pkgs.rustc
          pkgs.rustfmt
          pkgs.rust-analyzer

          # Extra scripts
          (pkgs.writeShellScriptBin "hs-test" ''
            if [[ $# -gt 0 ]]; then
              cabal test --test-show-details=direct --test-option=--match="$*"
            else
              cabal test --test-show-details=direct
            fi
          '')
        ];
      };
    };
}
