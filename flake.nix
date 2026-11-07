{
  description = "engi";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      # engi.nix comes from `cabal2nix . > engi.nix`.
      # Rerun it when engi.cabal changes. This avoids import-from-derivation.
      engiFor = pkgs: pkgs.haskellPackages.callPackage ./engi.nix { };
    in
    {
      # The package to install has no tests. `checks.tests` runs them.
      packages = forAllSystems (pkgs: rec {
        engi = pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.lib.dontCheck (engiFor pkgs));
        default = engi;
      });

      checks = forAllSystems (pkgs: {
        tests = engiFor pkgs;
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ (engiFor pkgs) ];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.zstd
            pkgs.age
            pkgs.rsync
          ];
        };
      });
    };
}
