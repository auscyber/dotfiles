{ config, pkgs, ... }:

{
  nix.extraOptions = ''
    !include ${config.age.secrets.access-tokens.path}
        		'';
  age = {
    identityPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];
    secretsDir = "${config.home.homeDirectory}/.config/agenix";
    secretsMountPoint = "${config.home.homeDirectory}/.config/agenix.d";
    secrets = {
      access-tokens = {
        file = ../../secrets/access-tokens.age;
      };
    };
  };
}
