{ den, lib, ... }:
# AN HDA PIN-DEFAULT PATCH, BUILT INTO THE CONFIGURATION.
#
# `hdajackretask` from alsa-tools can retask a jack at runtime and can write a
# firmware patch file, but the runtime half does not survive a boot and the file
# it writes lands in /lib/firmware -- which on NixOS is not a directory anything
# reads. MEASURED on auspc: there is no /lib/firmware/hda-jack-retask.fw there,
# so whatever retasking is in effect was done by hand and is gone on the next
# boot. This aspect is the declarative version of that file.
#
# WHAT THE KERNEL READS. `snd-hda-intel`'s `patch=` parameter takes a firmware
# name per card index and `request_firmware`s it; the format is hdajackretask's
# own, two sections:
#
#   [codec]
#   <vendor id> <subsystem id> <address>
#
#   [pincfg]
#   <nid> <pin default value>
#
# The `[codec]` line is a MATCH, not a label: the parser walks the probed codecs
# and applies the `[pincfg]` block only to the one whose vendor id, subsystem id
# and address all agree. A value that is merely wrong therefore makes the patch
# SILENTLY DO NOTHING, which is why every one of them is an option with no
# default below rather than a plausible-looking constant. (That matching
# behaviour is read off the kernel's documented behaviour for the patch loader,
# not measured here.)
#
# ── WHY `patch=` IS REPEATED, NOT WRITTEN ONCE ──
#
# `patch=` is an ARRAY parameter indexed by CARD, and the index is probe order.
# auspc has two HDA controllers -- the Intel PCH at `0000:00:1f.3` carrying the
# Realtek codec, and the nVidia one at `0000:01:00.1` carrying HDMI audio -- so
# `patch=hda-jack-retask.fw` with one entry patches whichever card won the race,
# and on the boot where that is the GPU the retasking just does not happen.
#
# Writing the same filename once per card slot is safe precisely BECAUSE of the
# `[codec]` match above: the controller that does not hold this codec reads the
# file, fails to match, and applies nothing.
#
# ── WHY THE FILE IS NOT COMPRESSED ──
#
# `hardware.firmware` runs every path through `compressFirmware`
# (nixos/modules/services/hardware/udev.nix), which defaults to zstd on kernels
# >= 5.19, and the kernel's loader only sees through that with
# FW_LOADER_COMPRESS_ZSTD. nixpkgs' own kernel config sets it, but auspc runs
# the CachyOS kernel (../nixos/kernels/cachyos.nix), whose config comes from
# upstream and is NOT something this flake asserts over. `compressFirmware =
# false` on the derivation is the module's own escape hatch and it costs nothing
# for a ~100-byte text file -- so the question does not have to be answered.
let
  inherit (lib) mkOption types;

  # An HDA id is exactly eight hex digits after `0x` -- the form
  # /proc/asound/card*/codec* prints. A constraint in the TYPE rather than a
  # `throw` in a helper, per ../../CLAUDE.md: a malformed value is named at the
  # option that holds it, before anything is rendered.
  hdaId = types.strMatching "0[xX][0-9a-fA-F]{8}";
in
{
  den.aspects.hda-jack-retask = {
    nixos =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      let
        cfg = config.hardware.hdaJackRetask;

        # `[pincfg]`, one line per retasked node. Sorted, because `mapAttrsToList`
        # already is and a stable file means a stable store path.
        pincfg = lib.mapAttrsToList (nid: value: "${nid} ${value}") cfg.pins;

        patchText = ''
          [codec]
          ${cfg.codecId} ${cfg.subsystemId} ${toString cfg.address}

          [pincfg]
          ${lib.concatStringsSep "\n" pincfg}
        '';

        # `passthru.compressFirmware = false` is read by udev.nix as
        # `firmware.compressFirmware or true`, and `passthru` attrs sit at the
        # top level of a derivation -- so this is the attribute that check looks
        # for. See the header for why it matters here and not in general.
        firmware = pkgs.runCommand "hda-jack-retask-firmware" {
          passthru.compressFirmware = false;
          inherit patchText;
          passAsFile = [ "patchText" ];
        } ''install -Dm444 "$patchTextPath" "$out/lib/firmware/${cfg.firmwareName}"'';
      in
      {
        options.hardware.hdaJackRetask = {
          # ── THE THREE `[codec]` FIELDS, NONE WITH A DEFAULT ──
          #
          # Deliberately undefaulted. A wrong id is indistinguishable at runtime
          # from no patch at all, so "used but not defined" from the module
          # system is strictly better than any guess -- including a guess that
          # happens to be right on one board.
          codecId = mkOption {
            type = hdaId;
            example = "0x10ec1220";
            description = ''
              The codec's `Vendor Id`, verbatim from
              `/proc/asound/card<n>/codec#<m>`.
            '';
          };

          subsystemId = mkOption {
            type = hdaId;
            example = "0x1462cb12";
            description = ''
              The codec's `Subsystem Id` from the same file. NOT the PCI
              subsystem of the controller -- they often agree and are not the
              same field.
            '';
          };

          address = mkOption {
            type = types.ints.unsigned;
            default = 0;
            description = ''
              The codec's `Address` on its HDA bus. 0 for the only codec on a
              controller, which is the ordinary case; a default rather than a
              required value because unlike the two ids above, 0 is right far
              more often than not and a wrong address fails the same loud way
              the ids do -- by matching nothing.
            '';
          };

          pins = mkOption {
            type = types.attrsOf hdaId;
            default = { };
            example = {
              "0x15" = "0x01a11c30";
              "0x16" = "0x01a11c40";
            };
            description = ''
              Pin defaults to write, keyed by node id. The VALUE is a whole
              32-bit pin-default word of the kind `Pin Default` prints in
              `/proc/asound/card<n>/codec#<m>`, not a flag to be or-ed into the
              existing one -- the patch REPLACES the pin's default.

              Empty means this aspect emits nothing at all: no firmware, no
              `patch=` line. That is the point. A `[pincfg]` with no entries is
              a patch that loads and does nothing, which looks exactly like a
              working retask until someone plugs a turntable in.
            '';
          };

          firmwareName = mkOption {
            type = types.strMatching "[A-Za-z0-9._-]+";
            default = "hda-jack-retask.fw";
            description = ''
              Basename under `lib/firmware`. `request_firmware` resolves a bare
              relative name against `firmware_class`'s `path`, which
              nixos/modules/services/hardware/udev.nix points at
              `''${config.hardware.firmware}/lib/firmware` -- so this name and
              the one in `patch=` are the same string and nothing has to know
              the store path.

              The default is hdajackretask's own output name, which is also what
              a hand-written `patch=` line in the wild will say.
            '';
          };

          cardSlots = mkOption {
            type = types.ints.between 1 32;
            default = 2;
            description = ''
              How many HDA card slots to offer the patch to. See the header: the
              parameter is indexed by card, the index is probe order, and the
              `[codec]` match makes the wrong card ignore the file. The default
              covers the common "onboard codec plus one GPU HDMI codec" box.
            '';
          };
        };

        # `pins` IS the switch. There is no `enable`, because an enabled retask
        # with nothing to retask is not a state worth being able to express, and
        # `mkIf` here is what keeps the undefaulted ids above from being
        # evaluated on a host that never sets any.
        config = lib.mkIf (cfg.pins != { }) {
          hardware.firmware = [ firmware ];

          # `types.lines`, so this merges with anything else writing modprobe
          # config -- checked: nothing else in this tree sets an `options
          # snd-hda-intel` line (../nixos/nvidia.nix uses
          # `extraModprobeConfig` but only for `nvidia*`). Two `options` lines
          # for one module would NOT merge semantically, so if a second one ever
          # appears it has to be folded into this option rather than added
          # beside it.
          boot.extraModprobeConfig = ''
            options snd-hda-intel patch=${lib.concatStringsSep "," (lib.replicate cfg.cardSlots cfg.firmwareName)}
          '';
        };
      };
  };
}
