{ config, pkgs }:

{
  nix.settings.extraOptions = ''
    		!include ${config.age.secrets.access-tokens.path}
    		'';
  age = {
    identityPaths = [ "~/.ssh/id_ed25519" ];
    secrets = {
      access-tokens = {
        file = ../secrets/access-tokens.age;
      };
    };
  };
}
