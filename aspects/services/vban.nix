{
  den,
  lib,
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

  # ── THE RECEIVER IS RESOLVED, NOT NAMED ──
  #
  # Was `host = "secondpc"` on the stream plus a `lanOf` that read that
  # machine's `lanAddress` out of ../hosts/fleet.nix. Three things were wrong
  # with it and only the first was visible: the box was named rather than the
  # SERVICE, so moving Music Assistant to another machine was a silent
  # mis-send; the port was a second default here, beside the one the receiver
  # declares; and "not through the tunnel" was a sentence in a comment, which
  # is not a thing resolution can honour.
  #
  # All three are one take now. `name` picks the service (../services/
  # homeassistant/music-assistant.nix furnishes it), the `mtu` clause is the
  # comment turned into a comparison -- ../network/segments.nix gives the house
  # lan `mtu = 1500` and the wireguard tunnel 1420, so the tunnel loses on the
  # numbers rather than on being excluded by name -- and `reach = "household"`
  # on that aspect is what allows the link to leave this box at all.
  #
  # A full VBAN packet does not fit a 1420-byte tunnel MTU, which is what the
  # clause is for: an under-MTU path does not fail, it fragments, and
  # uncompressed audio that fragments is audio that stutters.
  receiverTake = {
    name = "music-assistant";
    mtu = m: m != null && m >= 1500;
  };

  # `{ address; ports; ... }` for the receiver, from this host, or null when
  # nothing carries the flow. ../base/capability.nix's `dial` is the two-step:
  # which giver (reach, clauses, preference) and then which carrier between the
  # two boxes.
  receiverFor = host: den.lib.capability.dial "udp" receiverTake host;

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

  streamModule = _: {
    options = {
      destination = mkOption {
        type = types.str;
        example = "192.168.0.26";
        description = ''
          Address of the VBAN receiver. No default and no `host` option: a
          stream that knows which machine it is sending to has already
          stopped resolving, so the address is either resolved from a take
          (see `den.aspects.vban-turntable` below, which is the shape to
          copy) or it is a literal because the receiver is not something
          this flake describes.
        '';
      };

      port = mkOption {
        type = types.port;
        default = 6980;
        description = ''
          VBAN's registered port. A default rather than a resolution,
          because a stream may point at a receiver this flake knows nothing
          about; a stream that resolves its `destination` reads the port off
          the same link and does not inherit this.
        '';
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
    # The capture rig, so a host that wants this stream gets the sound card
    # configured too. ../nixos/turntable.nix explains why it is a separate
    # aspect rather than more content in this one -- in short, this aspect is
    # excluded by `specialisations.no-turntable` and the hardware should not
    # change when the receiver is off.
    includes = [
      den.aspects.vban
      den.aspects.turntable-input
    ];

    # WHAT THIS STREAM NEEDS, as a take rather than as a destination. The
    # household is what makes it resolvable: Music Assistant is declared by
    # `den.households.ivymectin` and its aspect is `reach = "household"`, so a
    # take from another machine in the same house finds it and one from
    # anywhere else does not.
    takes.udp = receiverTake;

    # ── AND WHAT HAPPENS WHEN THE RECEIVER IS NOT THERE ──
    #
    # A THROW, not a fallback. The old `lanOf` fell back to the fleet's
    # recorded address whatever the state of the world, so a Music Assistant
    # that had moved -- or a tunnel-only path, which fragments -- produced a
    # stream that ran and was heard by nobody. There is no safe default
    # address for "send uncompressed audio somewhere", so this fails at
    # evaluation and says which half is missing.
    #
    # ../base/service-reach.nix turns the same take into a `requires` check, so
    # the RUNTIME version of this question -- the receiver resolved fine and is
    # simply down -- is answered before a build rather than by a silent stream.
    nixos =
      {
        config,
        host ? { },
        ...
      }:
      let
        resolved = receiverFor host;

        receiver =
          if resolved != null then
            resolved
          else
            throw "den.aspects.vban-turntable: nothing on ${host.name or "this host"} resolves `udp` from a service named `music-assistant` over a carrier with a 1500-byte MTU. Either no host in this household serves it, or the only path between the two boxes is the tunnel -- which fragments VBAN.";
      in
      {
        services.vbanSend.streams.turntable = {
          destination = receiver.address;
          # The port the RECEIVER declares, not this module's default: one
          # number, in `gives.udp.ports.vban`, read back through the link.
          inherit (receiver.ports.vban) port;
          # READ OFF THE CAPTURE RIG, not spelled out here.
          #
          # This was `alsa_input.pci-0000_00_1f.3.analog-stereo` -- wireplumber's
          # name for the Realtek's analog capture node under the default
          # profile. Three things were wrong with that and only the last was
          # written down: the node is whatever wireplumber decided to call it
          # (the old comment's own caveat about `pro-input-0` under the
          # `pro-audio` profile); the turntable on auspc is now TWO ADCs merged
          # into one source, which that name cannot refer to at all; and
          # ../nixos/turntable.nix had to be told the node name anyway, so it
          # was a second definition of one fact.
          #
          # `sourceNode` is read-only and equal to the merged source's name by
          # construction, so this stream follows a single-jack or a merged
          # arrangement without an edit here.
          source = config.hardware.turntable.sourceNode;
        };
      };
  };
}
