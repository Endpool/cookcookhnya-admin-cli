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

          admin-cli-image = pkgs.dockerTools.buildLayeredImage {
            name = "cookcookhnya-admin-cli";
            tag = "latest";
            contents = [cookcookhnya-admin-cli pkgs.cacert];
            config = {
              Entrypoint = ["${cookcookhnya-admin-cli}/bin/cookcookhnya-admin-cli"];
            };
          };
        in {
        haskellProjects.default = {
          autoWire = ["packages" "apps"];
          packages = { };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          packages = [pkgs.nixd pkgs.hpack];
        };

        packages = {
          default = cookcookhnya-admin-cli;
          inherit admin-cli-image;
        };
      };
    };
}
