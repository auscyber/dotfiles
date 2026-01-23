{
  config,
  lib,
  pkgs,
  flakeConfig,
  system,
  hostname,
  systemIdentifier,
  ...
}:
let
  cfg = config.auscybernix.nix.builders;
  inherit lib;

in
with lib;
{
  options.auscybernix.nix.builders = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable distributed Nix builds using remote builders.";
    };
    sshPublicKey = mkOption {
      type = types.str;
      default = "";
      description = "SSH Public key for accessing build machines";
    };
    builderConfig = {
      enable = mkEnableOption "Use this machine as a Nix build machine.";
      builderUser = mkOption {
        type = types.str;
        default = "builder";
        description = "Username for the build machine.";
      };
      systems = mkOption {
        type = types.listOf types.str;
        default = [ system ];
        description = "List of systems this build machine can build for.";
      };
      maxJobs = mkOption {
        type = types.int;
        default = 1;
        description = "Maximum number of concurrent jobs this build machine can handle.";
      };
      speedFactor = mkOption {
        type = types.int;
        default = 1;
        description = "Speed factor for this build machine.";
      };
      hostname = mkOption {
        type = types.str;
        default = "${systemIdentifier}-builder";
        description = "Hostname of the build machine.";
      };
      ipAddress = mkOption {
        type = types.str;
        default = config.auscybernix.vpn.ipAddress;
      };
      features = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "List of supported features for this build machine.";
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      age.secrets."builder-ssh-key" = {
        generator.script =
          { pkgs, file, name,... }:
          ''(exec 3>&1; ${pkgs.openssh}/bin/ssh-keygen -q -t ed25519 -N "" -C ${lib.escapeShellArg "${hostname}:${name}"} -f /proc/self/fd/3 <<<y >${
            lib.escapeShellArg (lib.removePrefix file ".age" + ".pub")
          } 2>&1; true)'';
      };
      auscybernix.nix.builders.sshPublicKey =
        builtins.path { path = config.age.rekey.generatedSecretsDir; } + ("/builder-ssh-key.pub");

      nix.distributedBuilds = true;
      # optional, useful when the builder has a faster internet connection than yours
      nix.extraOptions = ''
        	  builders-use-substitutes = true
        	'';
    }
        (mkIf cfg.builderConfig.enable {
      services.openssh.extraConfig = ''
        SetEnv PATH=/nix/var/nix/profiles/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
        	'';
      nix.settings.trusted-users = [
        cfg.builderConfig.builderUser
      ];
      users.users.${cfg.builderConfig.builderUser} = {
        openssh.authorizedKeys.keyFiles = flakeConfig.flake.auscybernix.builders.sshKeys;

      };

    })
  ]);

}
