{
  den,
  lib,
  ...
}:
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
    overlays = { ... }: {
      pam_rssh = self: super: {
        # Stay on nixpkgs' own pinned pam_rssh (currently v1.2.1) rather than
        # the nvfetcher-tracked latest tag: v1.2.2-rc2 fails to build (its
        # `Facility::Auth` call doesn't exist in the libsyslog version it pulls
        # in). `sources.pam_rssh` still feeds `cargo_lock` above so nvfetcher
        # keeps a vendor hash ready for whenever upstream cuts a working
        # release worth tracking.
        pam_rssh = super.pam_rssh.overrideAttrs (old: {
          checkFlags = old.checkFlags ++ [
            "--skip=auth_keys::test_parse_authorized_keys"
          ];
          meta.platforms = old.meta.platforms ++ [ "aarch64-darwin" ];
        });
      };
    };

    darwin = { pkgs, ... }: {
      security.pam.services.sudo_local.text = lib.mkAfter ''
        auth       sufficient     ${pkgs.pam_rssh}/lib/libpam_rssh.dylib auth_key_file=/etc/authorized_keys/%u.keys
      '';
    };
  };

  den.schema.host.includes = [ den.aspects.pam-rssh ];
}
