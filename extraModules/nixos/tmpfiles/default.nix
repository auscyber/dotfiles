# NixOS half of the cross-platform `auscybernix.tmpfiles` option.
#
# The schema (lib/tmpfiles.nix) is deliberately systemd-tmpfiles' own, so on
# NixOS this is a pass-through: the settings go to systemd-tmpfiles verbatim and
# retain full tmpfiles.d(5) fidelity, including `age` clean-up and boot-time
# application -- neither of which the darwin backend can offer.
#
# The point of the indirection is that a module wanting a state directory can
# declare it once, under a name that exists on both host classes, instead of
# reaching for `systemd.tmpfiles` and thereby becoming NixOS-only.
{
  config,
  lib,
  ...
}:
let
  tmpfiles = import ../../../lib/tmpfiles.nix { inherit lib; };
  cfg = config.auscybernix.tmpfiles;
in
{
  options.auscybernix.tmpfiles = {
    settings = tmpfiles.settingsOption;

    # Declared but unused here: activation phasing is a darwin concern (see
    # extraModules/darwin/tmpfiles). It exists on this class so that a module
    # setting it is not NixOS-vs-darwin conditional, matching the `os` class
    # rule that anything written there must be an option on *both* systems.
    phase = lib.mkOption {
      type = lib.types.enum [
        "preActivation"
        "extraActivation"
        "postActivation"
      ];
      default = "postActivation";
      visible = false;
      description = ''
        No effect on NixOS -- systemd-tmpfiles handles ordering itself. Present
        only so the option set matches the darwin backend.
      '';
    };
  };

  config.systemd.tmpfiles.settings = cfg.settings;
}
