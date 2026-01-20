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
