{ den, ... }:
{
  # ── SSH aspect ────────────────────────────────────────────────────────────────
  # NixOS: OpenSSH server, pam-rssh, sudo.
  # Home-manager: SSH client matchBlocks.
  # Include in host aspects (nixos) and user aspects (homeManager).
  den.aspects.ssh = {
    nixos =
      { config, lib, pkgs, ... }:
      {
        options.auscybernix.ssh.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable OpenSSH server with rssh PAM authentication.";
        };

        config = lib.mkIf config.auscybernix.ssh.enable {
          services.openssh = {
            enable = true;
            settings.StreamLocalBindUnlink = "yes";
          };
          security.sudo.enable = true;
          environment.etc = lib.concatMapAttrs
            (user: value: {
              "authorized_keys/${user}.keys" = {
                text = builtins.concatStringsSep "\n" value.openssh.authorizedKeys.keys;
              };
            })
            (lib.filterAttrs
              (name: user: user.isNormalUser && user.openssh.authorizedKeys != null)
              config.users.users);
          security.pam.services.sudo.rssh = true;
          security.pam.rssh = {
            enable = true;
            settings.cue = true;
            settings.cue_prompt = "please touch";
          };
        };
      };

    homeManager =
      { config, lib, pkgs, ... }:
      {
        options.auscybernix.programs.ssh.enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Enable SSH client configuration.";
        };

        config = lib.mkIf config.auscybernix.programs.ssh.enable {
          programs.ssh = {
            enable = true;
            enableDefaultConfig = false;
            matchBlocks = {
              "faggot.sh" = { hostname = "faggot.sh"; user = "ivy"; forwardAgent = true; };
              "secondpc" = { hostname = "121.200.22.213"; forwardAgent = true; user = "auscyber"; };
              "imflo.pet" = { hostname = "imflo.pet"; forwardAgent = true; user = "ivy"; };
            };
          };
        };
      };
  };
}
