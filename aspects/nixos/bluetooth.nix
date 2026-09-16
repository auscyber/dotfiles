{ den, ... }:
# BlueZ + a pipewire stack that can actually carry A2DP, factored out of the
# inline block in ../hosts/auspc.nix so a second host can have it.
{
  den.aspects.bluetooth = {
    nixos = { pkgs, ... }: {
      hardware.bluetooth = {
        enable = true;
        powerOnBoot = true;
        # Without this BlueZ advertises no A2DP sink role and phones see the
        # box as a headset only.
        settings.General = {
          Enable = "Source,Sink,Media,Socket";
          Experimental = true;
        };
      };
      services.blueman.enable = true;

      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        pulse.enable = true;
        wireplumber.enable = true;
      };

      environment.systemPackages = [ pkgs.bluez-tools ];
    };
  };
}
