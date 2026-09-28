{ inputs, ... }: {
  den.aspects.deploy = {
    layers = [ "dev" ];
    inputs.deploy-rs.url = "github:serokell/deploy-rs";
    inputs.deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
  };

  perSystem = { pkgs, ... }: {
    devshells.default = {
      packages = [
        pkgs.deploy-rs
      ];
    };
  };

  flake =
    {
      lib,
      config,
      ...
    }:
    {
      deploy.nodes = lib.mapAttrs' (
        hostname: nixosConfiguration:
        let
          inherit (nixosConfiguration.config.nixpkgs.hostPlatform) system;
        in
        {
          name = hostname;
          value = {
            inherit hostname;
            fastConnection = false;
            profiles.system = {
              sshUser = "root";
              remoteBuild = true;
              confirmTimeout = 300;
              path = inputs.deploy-rs.lib.${system}.activate.nixos nixosConfiguration;
            };
          };
        }
      ) (lib.filterAttrs (_: c: !c.config.boot.isContainer) config.nixosConfigurations);
    };
}
