{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];
      perSystem = { self', config, pkgs, ... }:
        let
          cookcookhnya-admin-cli = self'.packages.cookcookhnya-admin-cli;
        in {
        haskellProjects.default = {
          autoWire = ["packages" "apps"];
          packages = { };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          packages = [pkgs.nixd pkgs.hpack];

          shellHook = ''
            set -a
            source ./.env
            set +a
          '';
        };

        packages.default = cookcookhnya-admin-cli;
      };
    };
}
