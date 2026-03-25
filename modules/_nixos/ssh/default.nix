{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.ssh;
in
{

  options.auscybernix.ssh = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Allow rssh users to login via ssh.";
    };
  };
  config = lib.mkIf cfg.enable {
    services.openssh = {
      enable = true;

      settings.StreamLocalBindUnlink = "yes";
    };
    security.sudo = {
      enable = true;
      #	extraRules = {

      #	};
    };
	environment.etc =
    lib.concatMapAttrs
      (user: value: {
        "authorized_keys/${user}.keys" = {
          text = builtins.concatStringsSep "\n" value.openssh.authorizedKeys.keys;
        };

      })
      (
        lib.filterAttrs (
          name: user: user.isNormalUser && user.openssh.authorizedKeys != null
        ) config.users.users
      );

    security.pam.services.sudo = {
      rssh = true;
    };
    security.pam.rssh = {

      enable = true;
      settings.cue = true;
      settings.cue_prompt = "please touch";
    };
  };
}
