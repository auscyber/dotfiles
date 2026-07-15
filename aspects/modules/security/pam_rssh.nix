{ den, lib, ... }:
{

  nvfetcher.sources.pam_rssh = {
    src.github = "z4yx/pam_rssh";
    fetch.github = "z4yx/pam_rssh";
    cargo_lock = [ "Cargo.lock" ];
    git.fetchSubmodules = true;
  };
  # Pure pam_rssh wiring. No key management lives here — see main-ssh-key.nix.
  # Auto-attached to every host via den.schema.host.includes so every system
  # has sudo-via-ssh-agent enabled out of the box.
  den.aspects.pam-rssh = {
    nixos = {
      security.pam.rssh = {
        enable = true;
        settings.cue = true;
        settings.cue_prompt = "please touch";
      };
      security.pam.services.sudo.rssh = true;
    };
    overlays = { sources, ... }: {
      pam_rssh = self: super: {
        pam_rssh = super.pam_rssh.overrideAttrs (old: {
          inherit (sources.pam_rssh) src version;
          checkFlags = [
            # reason for disabling test
            "--skip=auth_keys::test_parse_authorized_keys"
            "--skip=tests::parse_user_authorized_keys"
          ];
          meta.platforms = old.meta.platforms ++ [ "aarch64-darwin" ];
        });
      };

    };

    darwin =
      { pkgs, ... }:
      {
        security.pam.services.sudo_local.text = lib.mkAfter ''
          auth       sufficient     ${pkgs.pam_rssh}/lib/libpam_rssh.dylib auth_key_file=/etc/authorized_keys/%u.keys
        '';
      };
  };

  den.schema.host.includes = [ den.aspects.pam-rssh ];
}
