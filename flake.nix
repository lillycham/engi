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

      # The package to install has no tests. `checks.tests` runs them.
      # `engi export` runs age, so the installed program has age on its PATH.
      # A scheduled service does not have the PATH of a shell.
      installFor = pkgs:
        let
          hlib = pkgs.haskell.lib;
          base = hlib.justStaticExecutables (hlib.dontCheck (engiFor pkgs));
        in
        hlib.overrideCabal base (drv: {
          buildTools = (drv.buildTools or [ ]) ++ [ pkgs.makeWrapper ];
          postInstall = (drv.postInstall or "") + ''
            wrapProgram $out/bin/engi --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.age ]}
          '';
        });
    in
    {
      packages = forAllSystems (pkgs: rec {
        engi = installFor pkgs;
        default = engi;
      });

      # The tests include a round trip with the real age, so age is a tool of the
      # test run. Without it that test prints "skipped" and passes.
      checks = forAllSystems (pkgs: {
        tests = pkgs.haskell.lib.overrideCabal (engiFor pkgs) (drv: {
          testToolDepends = (drv.testToolDepends or [ ]) ++ [ pkgs.age ];
        });
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
