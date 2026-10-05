{ den, lib, ... }:
# PROBE: can a host ENTITY field be DERIVED from the class content of the
# aspects that host includes -- reading declarations, never evaluated config?
#
# That is the one thing the chosen design needs. "A class aspects contribute,
# folded onto the entity" is only safe if the fold reads DECLARATIONS: a fold
# over evaluated config would be config -> entity -> config, which is the shape
# that deadlocked twice. Aspect definitions are flake-level data, so reading
# them is a different direction entirely.
#
# Shape under test: a plain attribute `lan` on an aspect definition, collected
# across the aspects a host includes.
{
  den.aspects.probe-attach-wired = {
    lan.lan-home = {
      address = "192.168.0.26";
      prefix = "16";
      interface = "br0";
    };
  };

  den.aspects.probe-attach-guest = {
    lan.lan-guest.interface = "wlan0";
  };

  den.schema.host =
    { config, ... }:
    {
      options.lanProbe = lib.mkOption {
        type = lib.types.raw;
        readOnly = true;
        internal = true;
        default =
          let
            # Which aspects does this host include? `aspects` is on the entity.
            mine = config.aspects or [ ];
            # Their `lan` content, merged. Declarations only.
            contributions = map (a: a.lan or { }) mine;
          in
          {
            aspectCount = builtins.length mine;
            withLan = builtins.length (builtins.filter (c: c != { }) contributions);
            merged = lib.foldl' lib.recursiveUpdate { } contributions;
          };
      };
    };
}
