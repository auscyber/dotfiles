{ ... }:
# OpenRGB's SDK server: the RGB hardware inside a machine, offered to the house
# as something Home Assistant can switch.
#
# ── WHY THIS IS A SERVICE ASPECT AND NOT A HARDWARE ONE ──
#
# The lighting is hardware and the i2c buses it reaches are a fact about one
# motherboard, which argues for ../nixos/. What settles it the other way is the
# SDK server: the point of enabling this is that something off the box drives
# the lights, so the aspect's reason to exist is a port other hosts dial, and
# ../nixos/turntable.nix's test -- "would this still be worth having with
# nothing consuming it" -- comes out no.
#
# ── WHAT THE MODULE DOES AND DOES NOT DO ──
#
# `services.hardware.openrgb` (nixos/modules/services/hardware/openrgb.nix on
# the locked nixpkgs) runs `openrgb --server --server-port <n>`, loads
# `i2c-dev` plus the chipset's SMBus driver, and installs the package's udev
# rules. It has NO firewall option at all, which is the right shape for this
# tree: ../base/firewall.nix derives the hole from `gives` below, so the port
# opens on exactly the wires named and nowhere else.
#
# It also has no bind address, and neither does OpenRGB -- `--server` listens
# on every interface and the binary takes no address. So there is deliberately
# no `bind` on the port below: stating one would be a claim this aspect cannot
# honour, and ../base/capability-types.nix's `bind.address` is for a service
# that can actually be told. The firewall is the whole of the access control
# here, which is worth knowing before exposing it anywhere wider than a house.
let
  # OpenRGB's registered SDK port, and the module's own default. Written once
  # here so `gives.tcp` and the module cannot disagree.
  sdkPort = 6742;
in
{
  den.aspects.openrgb = {
    # ── A HOUSE AMENITY, BECAUSE THE CONSUMER IS ON ANOTHER BOX ──
    #
    # Home Assistant runs on secondpc and the lights are in auspc, so a take
    # from another machine in this household has to resolve here. `host` would
    # make the integration unresolvable and the symptom would be a link with no
    # candidates rather than anything about lighting.
    reach = "household";

    # ── AND THE INTEGRATION, CONTRIBUTED BY THE THING THAT PROVIDES IT ──
    #
    # Same move as ./homeassistant/esphome.nix and ./homeassistant/matter.nix:
    # the component goes in through the edge rather than into core.nix's
    # hand-written `extraComponents`, so taking this aspect off auspc takes the
    # integration out of Home Assistant with it instead of leaving an entry
    # behind for a service that is gone.
    #
    # THIS IS THE FIRST SUCH TAKER ON A DIFFERENT HOST, and it found a real gap:
    # core.nix folded contributions with `capability.contributions`, which asks
    # `takersOn host` -- takers on HA's OWN box. Every existing taker happened
    # to be co-located, so the limitation never showed. This one is not, and its
    # component reached nobody until `contributionsReaching` existed. See the
    # note on that function in ../base/capability.nix.
    takes.home-assistant = {
      extraComponents = [ "openrgb" ];
      # Home Assistant dials this; ../base/firewall.nix derives the hole from
      # where HA actually is.
      listensOn = {
        capability = "tcp";
        port = "sdk";
      };
    };

    # ── AND THE PART THAT CANNOT BE DECLARATIVE, STATED PRECISELY ──
    #
    # The component above is the whole of what this edge can push. The HOST AND
    # PORT are not pushed and cannot be: `homeassistant/components/openrgb/
    # __init__.py` defines no `CONFIG_SCHEMA`, no `PLATFORM_SCHEMA` and no
    # `async_setup` -- only `async_setup_entry` -- so the integration is config
    # ENTRY only and has nothing to read a YAML key with. `takes.home-assistant`
    # does carry a `config` option, and using it here would be dead weight.
    #
    # So one manual step survives, once per install, in the UI:
    #
    #   Settings -> Devices & Services -> Add Integration -> OpenRGB
    #
    # NO ADDRESS IS WRITTEN HERE, deliberately. An address in a comment is a
    # fact with no reader and nothing to keep it true -- this tree had
    # secondpc's old `192.168.0.26` sitting in four documents after the machine
    # moved, which is precisely the failure. Ask the resolver instead, which
    # answers from whatever the host record currently says:
    #
    #   nix eval --impure --raw .#debug.config --apply 'cfg:
    #     let cap = cfg.den.lib.capability; hs = cfg.den.hosts.x86_64-linux;
    #         l = cap.dial "tcp" { name = "openrgb"; } hs.secondpc;
    #     in "${l.address}:${toString l.ports.sdk.port}"'
    #
    # The entry itself lands in HA's `.storage`, which is the same
    # "only copy of a credential or a config entry" ./homeassistant/state.nix
    # exists to snapshot -- so it survives a rebuild but not a lost state
    # directory.

    gives =
      { self, ... }:
      {
        tcp = _: {
          # `name`, so a consumer asks for THIS endpoint rather than for a port
          # number. The same reasoning as ./homeassistant/music-assistant.nix's
          # `name`: a take naming 6742 is a take that has stopped resolving.
          name = self.name;

          ports.sdk = {
            port = sdkPort;
            meta.description = "OpenRGB's SDK protocol, dialed from Home Assistant on another box; the binary binds every interface, so the firewall is the whole access control.";
          };

          carrier = self.name;
        };
      };

    nixos =
      { pkgs, ... }:
      {
        services.hardware.openrgb = {
          enable = true;
          server.port = sdkPort;

          # The plugin build, which is what makes this worth exposing: the base
          # package covers the common controllers, and the plugins are where the
          # per-vendor device support lives. ../desktop/openrgb.nix puts the same
          # build on a desktop user's PATH, for the same reason.
          package = pkgs.openrgb-with-all-plugins;

          # NO `motherboard`, DELIBERATELY. It selects the SMBus driver --
          # `i2c-piix4` for amd, `i2c-i801` for intel -- and nixpkgs already
          # derives it from `hardware.cpu.*.updateMicrocode`, which the host's
          # own hardware config sets. Stating it in an aspect that several hosts
          # could include is how ../desktop/openrgb.nix came to declare `amd` on
          # an i9-9900K, loading the wrong SMBus driver so the motherboard's own
          # devices never appeared. The derived answer cannot be wrong in that
          # way.
        };
      };
  };
}
