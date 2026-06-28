{ den, lib, ... }:
{
  den.aspects.pam-touchid = {
    darwin =
      { pkgs, ... }:
      {
        security.pam.services.sudo_local.text = lib.concatLines [
          "auth       sufficient     pam_tid.so"
        ];
      };
  };
}
