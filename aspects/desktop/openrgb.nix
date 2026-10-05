{
  # The GUI, for a person who wants to drive the lights by hand.
  #
  # ── WHAT USED TO BE HERE, AND WHY IT LEFT ──
  #
  # This file also wrote `services.hardware.openrgb` -- `enable = false`,
  # `package`, `motherboard = "amd"`, `server.port = 6742` -- which is the
  # "two halves of one thing in two places" this tree keeps unpicking. The
  # daemon is now ../services/openrgb.nix, which owns the port because it
  # publishes it as a capability, and a host gets the server by including that
  # aspect rather than by having a desktop user.
  #
  # Three things were wrong with the old block and only the first was inert:
  #
  #   * `enable = false` was the module's own default, so it said nothing while
  #     looking like a decision -- and it conflicted the moment any aspect
  #     wanted the server on, which is how it was found.
  #   * `motherboard = "amd"` on a ROLE-scoped aspect is a per-host hardware
  #     fact stated in a shared file. The only host this reaches is auspc,
  #     whose facter report says `GenuineIntel` (an i9-9900K), so it loaded
  #     `i2c-piix4` instead of `i2c-i801` and the motherboard's own SMBus
  #     devices were invisible. nixpkgs derives this from the microcode options
  #     anyway, so the correct value is no value.
  #   * `server.port` was a second copy of a number ../services/openrgb.nix
  #     now publishes through `gives.tcp`.
  #
  # The package stays, because this half genuinely is about the desktop: a GUI
  # on the user's PATH. `openrgb-with-all-plugins` rather than plain `openrgb`
  # so the GUI can see whatever the plugins add -- and the SERVER uses the same
  # build for the same reason, which that aspect states for itself.
  den.aspects.rgb.gui.provides.to-users.homeManager =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.openrgb-with-all-plugins ];
    };
}
