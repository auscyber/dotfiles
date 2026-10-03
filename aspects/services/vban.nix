{
  den,
  lib,
  fleet,
  ...
}:
# VBAN sender: a line-level input on this host, streamed to a VBAN receiver
# elsewhere in the fleet. Here that is the turntable on auspc's onboard Realtek
# codec, going to the `vban_receiver` provider of the Music Assistant server on
# secondpc (./homeassistant).
#
# NOT `pkgs.vban` (`vban_emitter`/`vban_receptor`): nixpkgs has no VBAN tooling
# at all -- only the `aiovban`/`aiovban-pyaudio` python libraries Music
# Assistant's own provider is built on -- so there is no emitter binary to run.
# pipewire ships `module-vban-send` natively instead, which is the better route
# anyway: the sender is a node in the same graph as the capture device, so
# wireplumber links it and no second audio client sits in between.
#
# Nothing is opened in the firewall. VBAN is unidirectional UDP and this host is
# only ever the sender; the listening socket is the receiver's.
let
  inherit (lib) mkOption types;

  # The receiver is named, not addressed: ../hosts/fleet.nix already knows every
  # machine, so a stream says `host = "secondpc"` and the address follows the
  # fleet record. `lanAddress` and not `builder.ipAddress` -- that one is a
  # wireguard address, and pushing uncompressed audio between two boxes on the
  # same switch through the tunnel is both pointless and a fragmentation
  # problem, since a full VBAN packet does not fit a 1420-byte tunnel MTU.
  lanOf =
    name:
    let
      addr = fleet.${name}.lanAddress or null;
    in
    if addr != null then
      addr
    else
      throw "services.vbanSend: fleet host '${name}' declares no `lanAddress`; set one on it or give the stream an explicit `destination`.";

  # The rates VBAN's header can encode (module-vban/vban.h `vban_SR`). Anything
  # else is sent with the index that means 44100, so the receiver resamples
  # against a rate the graph is not actually running at.
  vbanRates = [
    6000
    8000
    11025
    12000
    16000
    22050
    24000
    32000
    44100
    48000
    64000
    88200
    96000
    128000
    176400
    192000
    256000
    352800
    384000
    512000
    705600
  ];

  streamModule =
    { config, ... }:
    {
      options = {
        host = mkOption {
          type = types.nullOr (types.enum (lib.attrNames fleet));
          default = null;
          example = "secondpc";
          description = "Fleet machine running the receiver; its `lanAddress` becomes `destination`.";
        };

        destination = mkOption {
          type = types.str;
          default = lanOf config.host;
          defaultText = lib.literalMD "the `lanAddress` of `host` in the fleet";
          description = "Address of the VBAN receiver, if it is not a fleet machine.";
        };

        port = mkOption {
          type = types.port;
          default = 6980;
          description = "VBAN's registered port; both ends default to it.";
        };

        source = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "alsa_input.pci-0000_00_1f.3.analog-stereo";
          description = ''
            pipewire node to capture from (`target.object`). Null follows
            whatever wireplumber picks as the default source, which is rarely
            what is wanted for a fixed physical input.
          '';
        };

        captureSink = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Take `source`'s monitor ports rather than its input ports, for
            mirroring a sink's playback instead of capturing a device.
          '';
        };

        rate = mkOption {
          type = types.enum vbanRates;
          default = 48000;
          description = "Sample rate to send at; must be one VBAN can encode.";
        };

        channels = mkOption {
          type = types.ints.between 1 256;
          default = 2;
        };

        format = mkOption {
          type = types.enum [
            "U8"
            "S16LE"
            "S24LE"
            "S32LE"
            "F32LE"
            "F64LE"
          ];
          default = "S16LE";
          description = "The subset of pipewire formats VBAN has a datatype for.";
        };

        ttl = mkOption {
          type = types.ints.between 1 255;
          default = 16;
        };
      };
    };

  moduleOf = name: stream: {
    name = "libpipewire-module-vban-send";
    args = {
      "destination.ip" = stream.destination;
      "destination.port" = stream.port;
      "sess.name" = name;
      "sess.media" = "audio";
      "audio.format" = stream.format;
      "audio.rate" = stream.rate;
      "audio.channels" = stream.channels;
      # The module defaults this to 1, which is correct only while sender and
      # receiver share a broadcast domain. One router hop -- a VLAN split, a
      # mesh node bridging onto another segment -- and the packets die silently
      # with the stream still showing as running.
      "net.ttl" = stream.ttl;
      "stream.props" = {
        "node.name" = "vban-send.${name}";
        "node.description" = "VBAN send: ${name}";
        # Overriding the module's own `Audio/Sink` default. A sink would make
        # this a playback target applications pick, which is the wrong shape for
        # a line-level instrument feed: as a capture stream it takes the input
        # ports of a real device and is unaffected by whatever else is playing.
        "media.class" = "Stream/Input/Audio";
      }
      // lib.optionalAttrs (stream.source != null) { "target.object" = stream.source; }
      // lib.optionalAttrs stream.captureSink { "stream.capture.sink" = true; };
    };
  };
in
{
  den.aspects.vban = {
    nixos =
      { config, ... }:
      let
        cfg = config.services.vbanSend;
      in
      {
        options.services.vbanSend.streams = mkOption {
          default = { };
          description = ''
            VBAN audio streams this host emits, one `module-vban-send` each. The
            attribute name is the VBAN stream name, which the receiving end
            matches on -- it has to agree with the stream configured in the
            Music Assistant VBAN provider exactly.
          '';
          type = types.attrsOf (types.submodule streamModule);
        };

        config = lib.mkIf (cfg.streams != { }) {
          assertions = lib.mapAttrsToList (name: _: {
            # `char stream_name[16]` in the VBAN header. Over that the module
            # truncates and the receiver is left matching a different name.
            assertion = builtins.stringLength name <= 16;
            message = "services.vbanSend.streams.${name}: a VBAN stream name is at most 16 characters.";
          }) cfg.streams;

          services.pipewire.extraConfig.pipewire."99-vban-send"."context.modules" =
            lib.mapAttrsToList moduleOf cfg.streams;
        };
      };
  };

  # The turntable feed itself, separate from the generic aspect above so a host
  # that wants to send something else does not inherit this one.
  den.aspects.vban-turntable = {
    includes = [ den.aspects.vban ];

    nixos.services.vbanSend.streams.turntable = {
      host = "secondpc";
      # Onboard Realtek codec (Z390 ACE), `0000:00:1f.3` in the facter report.
      # wireplumber's name for its analog capture node under the default
      # profile; if the card ends up on `pro-audio` instead this becomes
      # `alsa_input.pci-0000_00_1f.3.pro-input-0` -- check `wpctl status`.
      source = "alsa_input.pci-0000_00_1f.3.analog-stereo";
    };
  };
}
