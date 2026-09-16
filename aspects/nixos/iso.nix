# Minimal NixOS installer ISO carrying the master SSH key and ivy's neovim.
#
#   nix build .#packages.x86_64-linux.installer-iso
#   nix build .#packages.aarch64-linux.installer-iso
#   nix run   .#iso-dd -- ivy@1.2.3.4 /dev/sda
#
# Deliberately a standalone `nixosSystem`, not a den host: a den host lands in
# `nixosConfigurations`, which ../tooling/deploy.nix maps into `deploy.nodes`
# and ../tooling/ci-matrix.nix turns into a CI build. Installer media wants
# neither, and it has no agenix identity to rekey secrets to -- which is why no
# password is set here (`initialHashedPassword = ""`, as the installer profile
# already does for its own users).
{
  inputs,
  lib,
  den,
  ...
}:
let
  mainKey = import ../security/_main-key.nix;

  # Takes the perSystem `pkgs`, which ../tooling/overlays.nix has already built
  # with the repo's collected overlays. A bare `nixpkgs.lib.nixosSystem` has
  # none of them, and the nixvim config reaches for `vimPlugins.eagle-nvim`
  # from ../../packages -- without this the ISO fails with
  # "vimPlugins.eagle-nvim cannot be found in pkgs".
  isoModule = overlaidPkgs: { modulesPath, ... }: {
    imports = [ "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix" ];

    nixpkgs.pkgs = overlaidPkgs;

    services.openssh.enable = true;
    services.openssh.settings.PermitRootLogin = "prohibit-password";

    # Current nixpkgs no longer holds sshd out of multi-user.target, but
    # older revisions did. Stated so the ISO does not depend on that.
    systemd.services.sshd.wantedBy = lib.mkForce [ "multi-user.target" ];

    # Populates /etc/ssh/authorized_keys.d/<user>, which is both the sshd
    # login path and pam_rssh's default `auth_key_file`
    # (/etc/ssh/authorized_keys.d/$ruser) -- so one declaration covers login
    # and sudo-via-ssh-agent.
    users.users.ivy = {
      isNormalUser = true;
      description = "Ivy";
      extraGroups = [
        "wheel"
        "networkmanager"
      ];
      initialHashedPassword = "";
      openssh.authorizedKeys.keys = [ mainKey ];
    };
    users.users.root.openssh.authorizedKeys.keys = [ mainKey ];

    # The installer profile autologins `nixos`; this image is ivy's.
    services.getty.autologinUser = lib.mkForce "ivy";

    security.pam.rssh.enable = true;
    security.pam.rssh.settings.cue = true;
    security.pam.rssh.settings.cue_prompt = "please touch";
    security.pam.services.sudo.rssh = true;

    # The same nixvim config the fleet's users get, built standalone -- the
    # ISO has no home-manager, so the forward in ../nixvim/batteries.nix
    # never fires here.
    environment.systemPackages = [ (den.lib.nixvim.mkPackage { pkgs = overlaidPkgs; }) ];
  };
in
{
  perSystem =
    {
      system,
      pkgs,
      config,
      ...
    }:
    {
      packages =
        # `//` on two `packages` attrsets would drop one of them wholesale, so
        # the conditional entry is merged INSIDE the attribute, not beside it.
        lib.optionalAttrs (lib.hasSuffix "-linux" system) {
          # No `system` argument: that writes `nixpkgs.system`, which
          # conflicts with the `nixpkgs.pkgs` the module sets. The platform
          # comes from the package set instead.
          installer-iso =
            (inputs.nixpkgs.lib.nixosSystem {
              modules = [ (isoModule pkgs) ];
            }).config.system.build.isoImage;
        }
        // {
          # Stream an image to a disk on a machine already booted from live
          # media. zstd over the wire because the tail of an ISO is zeros and
          # the target is usually on the end of a slow link.
          iso-dd = pkgs.writeShellApplication {
            name = "iso-dd";
            runtimeInputs = [
              pkgs.openssh
              pkgs.zstd
              pkgs.coreutils
            ];
            text = ''
              if [ "$#" -lt 3 ]; then
                echo "usage: iso-dd <ssh-target> <device> <image>" >&2
                echo "  e.g. iso-dd ivy@100.64.0.5 /dev/sda ./result/iso/nixos.iso" >&2
                echo "" >&2
                echo "Writes <image> to <device> ON THE REMOTE HOST. This DESTROYS" >&2
                echo "everything on that device. To install NixOS properly use" >&2
                echo "nixos-anywhere + disko instead -- dd'ing an ISO to a disk" >&2
                echo "leaves you with live media, not an installed system." >&2
                exit 2
              fi

              target="$1"; device="$2"; image="$3"

              if [ ! -r "$image" ]; then
                echo "iso-dd: cannot read $image" >&2
                exit 1
              fi

              echo "==> $target: block devices" >&2
              ssh "$target" 'lsblk -o NAME,SIZE,TYPE,MODEL,MOUNTPOINTS || lsblk' >&2

              echo "" >&2
              echo "About to OVERWRITE $device on $target with:" >&2
              echo "  $image ($(du -h "$image" | cut -f1))" >&2
              echo "" >&2
              printf 'Type the device path again to confirm: ' >&2
              IFS= read -r confirm </dev/tty
              if [ "$confirm" != "$device" ]; then
                echo "iso-dd: '$confirm' != '$device' -- aborted" >&2
                exit 1
              fi

              echo "==> writing" >&2

              # Quoted HERE, on the client, on purpose -- the remote shell must
              # receive a single safe token. That is exactly what SC2029 warns
              # about, so it is silenced rather than worked around.
              rdev="$(printf '%q' "$device")"

              # `conv=fsync` so dd does not report success while the write is
              # still sitting in the target's page cache.
              # shellcheck disable=SC2029
              zstd -c -T0 "$image" \
                | ssh "$target" "zstd -d | sudo dd of=$rdev bs=4M conv=fsync status=progress"

              echo "==> re-reading the partition table" >&2
              # shellcheck disable=SC2029
              ssh "$target" "sudo blockdev --rereadpt $rdev || true"
              echo "done" >&2
            '';
          };
        };

      apps.iso-dd = {
        type = "app";
        program = lib.getExe config.packages.iso-dd;
      };
    };
}
