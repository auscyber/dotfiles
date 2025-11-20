{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.user;

in

{
  options.auscybernix.user = {
    username = lib.mkOption {
      type = lib.types.str;
      description = "Username for the user to manage SSH keys for.";
      default = "auscyber";
    };
  };
  config = {

    users.users = {
      "${cfg.username}" = {
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H"
        ];
      };
    };
  };
}
