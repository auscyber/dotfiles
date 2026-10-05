{ den, lib, ... }:
# THE TURNTABLE AS A CAPTURE DEVICE, separate from wherever its audio goes.
#
# ── WHY THIS IS NOT INSIDE `vban-turntable` ──
#
# ../services/vban.nix's `vban-turntable` is a TRANSPORT: it dials a resolved
# Music Assistant and streams at it. ../hosts/auspc.nix therefore carries
# `specialisations.no-turntable.excludes = [ den.aspects.vban-turntable ]` so the
# box can come up when the receiver is off.
#
# Putting the sound card's configuration in that aspect would make that
# specialisation ALSO un-retask the jacks and delete the capture node -- so
# "Music Assistant is switched off" would silently change what the hardware does.
# Those are different facts with different lifetimes, so they are different
# aspects, and ../hosts/auspc.nix includes THIS one directly as well as through
# `vban-turntable`. den's `excludes` is a by-name constraint over the subtree
# (den fx/aspect/children.nix `registerConstraints`), so an aspect the host root
# reaches on its own survives the exclusion of an aspect that merely also
# included it. That is what keeps the retask in the `no-turntable` profile.
#
# It is also what makes a second consumer possible without moving anything: see
# the sendspin note at the bottom.
#
# ── WHY THE SOURCE LIST IS DATA ──
#
# MEASURED on auspc (`/proc/asound/card0/codec#0`, Realtek ALC1220): the codec
# has TWO `Audio Input` widgets, 0x08 and 0x09, which is why ALSA exposes two
# capture streams on that card -- and node 0x1a is ALREADY `[Jack] Line In at
# Ext Rear`, a working stereo input.
#
# So BOTH arrangements are real on this hardware and they need different graphs:
#
#   ONE retasked stereo jack   one capture, two channels, no merge at all.
#                              Simplest, and all a plain turntable needs. On
#                              auspc this would be node 0x1a and no patch.
#   TWO jacks                  two captures, one channel taken from each,
#                              merged into one stereo source. MEASURED to be
#                              the case on auspc -- the factory line-in 0x1a
#                              plus 0x17, which a patch has to enable before
#                              the driver will admit it exists. See
#                              ../hosts/auspc.nix.
#
# Which one is right is a question about which pins were physically retasked,
# not about this file, so the captures are a LIST and the merge appears only
# when there is more than one of them. Hardcoding two `context.objects` entries
# would have baked the answer in, and the single-jack path would then not exist
# for the next host even though the hardware offers it.
#
# ── THE PART NEITHER THE PATCH NOR PIPEWIRE CAN DO: ADC ROUTING ──
#
# A pin-default patch makes a jack an INPUT. It does not say which ADC reads it.
# The ALC1220's two `Audio Input` widgets each have their own `Input Source`
# selector, exposed as two instances of one ALSA mixer enum, and if both land on
# the same pin then the two capture devices carry THE SAME SIGNAL -- which the
# merge duplicates across left and right. That plays, it looks right in every
# UI, and it is mono.
#
# Nothing declarative in the locked nixpkgs expresses "capture device N reads
# pin P":
#
#   * a UCM profile could, but HDA desktop cards do not go through UCM here --
#     wireplumber drives them through the ACP profile set, which has no concept
#     of per-PCM input source;
#   * `services.pipewire.wireplumber.extraConfig` rules set pipewire object
#     PROPERTIES, not ALSA control values;
#   * pipewire's `api.alsa.bind-ctl.*` exposes a control as a node param rather
#     than fixing its value.
#
# So `mixerControls` below renders `amixer` calls into a unit. The commands and
# their arguments are in the store and the unit is part of the configuration, so
# it is declarative in the sense that matters; it is not declarative in the
# sense that ALSA validates it, and a control name that does not exist on the
# card fails the unit rather than failing the build. Flagged rather than hidden.
#
# ── WHY THE CARD IS NAMED, NEVER INDEXED ──
#
# The sketch used `plughw:0,0` and `plughw:0,2`. auspc has TWO HDA controllers --
# the Intel PCH at `0000:00:1f.3` with the Realtek on it, and the nVidia one at
# `0000:01:00.1` which is HDMI audio and has no input jacks at all -- and the
# card index is probe order. On the boot where the GPU enumerates first,
# `plughw:0,0` is the GPU and the turntable captures silence with nothing
# reporting a fault. `plughw:CARD=<name>,DEV=<n>` names the card ALSA named, so
# nothing else can shift it. Same reasoning as every other resolved-rather-than-
# positional reference in this tree.
let
  inherit (lib) mkOption types;

  # The channel names SPA knows that are meaningful for a two-channel line
  # input. An enum rather than a free string, per ../../CLAUDE.md: a typo in a
  # channel name does not fail, it produces a port nothing links to.
  channel = types.enum [
    "MONO"
    "FL"
    "FR"
  ];

  captureModule =
    { config, ... }:
    {
      options = {
        device = mkOption {
          type = types.ints.unsigned;
          example = 0;
          description = ''
            The ALSA PCM device index on the card -- the `DEV=` in
            `plughw:CARD=<card>,DEV=<n>`. On the ALC1220 the two ADCs surface as
            device 0 (the card's main analog PCM) and device 2 (the "Alt Analog"
            capture the driver creates for the second `Audio Input` widget).
          '';
        };

        position = mkOption {
          type = types.listOf channel;
          default = [
            "FL"
            "FR"
          ];
          description = ''
            The channel map of the PCM ITSELF. A retasked analog jack on this
            codec is a stereo TRS jack read by a stereo ADC, so this is `[ FL FR
            ]` even when only one of the two channels is wanted -- `take` below
            is what narrows it.

            `audio.channels` is not a separate option: it is `length position`,
            so the two cannot disagree.
          '';
        };

        take = mkOption {
          type = types.listOf channel;
          default = config.position;
          example = [ "FL" ];
          description = ''
            Which of this PCM's channels feed the merged source. Defaults to all
            of them, which is the single-stereo-jack case.

            THE SKETCH THIS REPLACES SAID `audio.position = "MONO"` HERE, and
            that does not work: with `stream.dont-remix` the stream's ports are
            linked to the source's ports BY NAME, and a stereo ADC has `FL` and
            `FR` ports and no `MONO` one -- so the stream comes up with nothing
            linked to it. Naming the channel actually wanted (`[ FL ]`) links.
          '';
        };

        mapsTo = mkOption {
          type = types.nullOr (types.listOf channel);
          default = null;
          example = [ "FR" ];
          description = ''
            Where this capture's `take` channels land in the merged source, one
            for one. Required once there is more than one capture and ignored
            when there is only one, because then there is no merge to map into.
          '';
        };

        description = mkOption {
          type = types.str;
          default = "";
          description = "Human-readable name for this leg's node; defaults to a generated one.";
        };
      };
    };
in
{
  den.aspects.turntable-input = {
    # The retask and the capture are one rig: the pins this aspect's PCMs read
    # are only inputs BECAUSE of that patch. Including it also means
    # `hardware.hdaJackRetask.pins` is always declared here, which is what lets
    # the warning below be written at all.
    includes = [ den.aspects.hda-jack-retask ];

    nixos =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      let
        cfg = config.hardware.turntable;
        captures = cfg.captures;
        merging = builtins.length captures > 1;

        legName = i: if merging then "${cfg.nodeName}_adc${toString i}" else cfg.nodeName;

        # ── THE FILTER CHAIN, AND WHY IT IS A COMB OF NOTCHES ──
        #
        # MEASURED on auspc, with the deck idle and the cable in, against a
        # cable-out floor of -92.19 dB that proves the card itself is clean:
        #
        #     50 Hz -25.4   100 Hz -49.3   150 Hz -30.1   200 Hz -52.2
        #    250 Hz -41.2   300 Hz -56.4   350 Hz -42.9   400 Hz -59.7
        #    450 Hz -40.4   500 Hz -61.1   550 Hz -42.0   600 Hz -61.7
        #
        # ODD harmonics sit 20-25 dB above EVEN ones, consistently, across the
        # whole series, and the six odd ones sum to -23.85 dB against a total of
        # -23.56 -- so they are 94% of the noise power and everything else is a
        # -35.5 dB broadband residual. That is the shape this chain is built
        # for, and it is also a diagnosis: energy only at odd harmonics means a
        # waveform symmetric about zero, which is a transformer core in
        # saturation coupling MAGNETICALLY into the cartridge coils or the
        # tonearm leads. Rectifier ripple or a ground loop would put energy at
        # 100 Hz, and there is 24 dB less there.
        #
        # So `harmonics` defaults to the ODD multiples. Notching the even ones
        # spends filter on 25 dB less noise and takes more music with it.
        #
        # ⚠ AND THIS IS 12 dB, NOT A FIX. 94% of the power leaves a floor near
        # -35 dB, while music on this rig measures -23 to -31 dB: that is 4-12 dB
        # of SNR, still far short of a clean phono chain. Magnetic induction
        # falls off fast with distance and is DIRECTIONAL -- rotating the deck
        # 90 degrees is often 10-20 dB for nothing -- so this belongs on top of
        # moving the deck away from whatever transformer is radiating, never
        # instead of it.
        filtering = cfg.filter.mains != null || cfg.filter.rumble != null;

        # The channel map the filtered node carries, which is the SOURCE's map
        # and not a third statement of it: a merge publishes `mergedPosition`,
        # a single jack publishes its own `position`.
        outPosition = if merging then cfg.mergedPosition else (lib.head captures).position;

        # Two cascaded 2nd-order sections, so 24 dB/octave rather than 12. A
        # single section at 30 Hz leaves 10 Hz only 19 dB down, which is not
        # enough to be worth the phase shift it costs in the bass.
        rumbleNodes = lib.optionals (cfg.filter.rumble != null) (
          map
            (i: {
              type = "builtin";
              name = "hp${toString i}";
              label = "bq_highpass";
              control = {
                Freq = cfg.filter.rumble;
                Q = 0.7;
              };
            })
            [
              1
              2
            ]
        );

        # `Q` SCALES WITH THE HARMONIC, which is the only subtle part. Q is
        # centre frequency over bandwidth, so a fixed Q gets WIDER in Hz as it
        # goes up: Q=20 is 2.5 Hz at 50 Hz but 27.5 Hz at 550, and a 27 Hz hole
        # in the low midrange is audible on music. Multiplying by the harmonic
        # number holds the bandwidth constant in Hz at `mains / q`, so every
        # notch is as narrow as the first one.
        notchNodes = lib.optionals (cfg.filter.mains != null) (
          map (n: {
            type = "builtin";
            name = "n${toString n}";
            label = "bq_notch";
            control = {
              Freq = cfg.filter.mains * n;
              Q = cfg.filter.q * n;
            };
          }) cfg.filter.harmonics
        );

        chainNodes = rumbleNodes ++ notchNodes;

        # A chain, so each node's output feeds the next. `zipListsWith` over the
        # list against its own tail is the pairwise walk; a one-node chain gives
        # the empty list, which is correct rather than a special case.
        chainLinks = lib.zipListsWith (a: b: {
          output = "${a.name}:Out";
          input = "${b.name}:In";
        }) (lib.init chainNodes) (lib.tail chainNodes);

        filterModule = {
          name = "libpipewire-module-filter-chain";
          args = {
            "node.description" = "${cfg.description} (filtered)";
            # ONE INPUT AND ONE OUTPUT, DELIBERATELY. filter-chain duplicates a
            # graph with a single in and a single out across every channel, so
            # the two channels get bit-identical filtering. Writing the graph
            # per channel instead would make it possible for left and right to
            # drift apart, which is the one thing this input does not need more
            # of -- it already has a 6 dB imbalance from the cartridge.
            "filter.graph" = {
              nodes = chainNodes;
              links = chainLinks;
            };
            "capture.props" = {
              "node.name" = "${cfg.nodeName}-filter";
              "media.class" = "Stream/Input/Audio";
              "target.object" = cfg.nodeName;
              "audio.channels" = builtins.length outPosition;
              "audio.position" = outPosition;
              # The filter does not keep the ADC awake by existing; the source
              # node's own `node.suspend-on-idle = false` does that, and it is
              # the right place for it.
              "node.passive" = true;
            };
            "playback.props" = {
              "node.name" = cleanName;
              "node.description" = "${cfg.description} (filtered)";
              "media.class" = "Audio/Source";
              "audio.channels" = builtins.length outPosition;
              "audio.position" = outPosition;
            };
          };
        };

        cleanName = "${cfg.nodeName}-clean";
        legDescription =
          i: c:
          if c.description != "" then
            c.description
          else if merging then
            "${cfg.description} (ADC ${toString i})"
          else
            cfg.description;

        # One static ALSA capture node per leg. `adapter` rather than the raw
        # `api.alsa.pcm.source` factory because the adapter is what supplies the
        # converter/resampler the graph needs to run this at the session rate --
        # the shape upstream's own pipewire.conf documents for exactly this.
        #
        # `hw:` AND NOT `plughw:`, WHICH IS A REVERSAL. The argument for `plug`
        # was that the ADC's native format is whatever the codec offers, so
        # letting ALSA convert turns a format mismatch into ALSA's problem
        # rather than a reason for the node to fail to open. Two measurements
        # on auspc retired it.
        #
        # `arecord -D hw:CARD=PCH,DEV=0 --dump-hw-params` reports
        # `FORMAT: S16_LE S32_LE`, `RATE: [44100 192000]`, `CHANNELS: 2` -- so
        # the codec gives `rate`/`format` below directly and there is nothing
        # for `plug` to convert. The conversion was insurance against a
        # mismatch that does not exist.
        #
        # And the insurance had a cost. This node is the graph's DRIVER (pipewire
        # sets `node.driver` on an ALSA source itself), so its clock is what
        # schedules every consumer. `plug` interposes a conversion layer in
        # exactly that timing path, and the observed failure was a node that
        # negotiated a format, reported `state: "running"`, held its links, and
        # never produced a cycle -- `pw-top` showing `QUANT 0`, `RATE 0`,
        # `W/Q ???` with both ends `R`. A sender fed by it emits nothing while
        # looking entirely healthy.
        #
        # The `CARD=` form is also CONFIRMED, which was an open question here:
        # only `plughw:0,0` had ever been proven and the argument for naming the
        # card was theoretical. `arecord -L` lists `front:CARD=PCH,DEV=0`, so the
        # name resolves and the probe-order-dependent index is not needed.
        adapterFor = i: c: {
          factory = "adapter";
          args = {
            "factory.name" = "api.alsa.pcm.source";
            "node.name" = legName i;
            "node.description" = legDescription i c;
            "media.class" = "Audio/Source";
            "api.alsa.path" = "hw:CARD=${cfg.card},DEV=${toString c.device}";
            "audio.rate" = cfg.rate;
            "audio.format" = cfg.format;
            "audio.channels" = builtins.length c.position;
            "audio.position" = c.position;
            # A line input is not a desktop microphone. Suspending it means the
            # first seconds of a side are spent waking the ADC up, and the
            # stream that consumes it (VBAN, or a future sendspin client) wants
            # a node that is simply always there.
            "node.suspend-on-idle" = false;
          };
        };

        combineModule = {
          name = "libpipewire-module-combine-stream";
          args = {
            "combine.mode" = "source";
            "node.name" = cfg.nodeName;
            "node.description" = cfg.description;
            # Two ADCs on one codec share a clock, so the drift this corrects
            # for should be nil -- but they are separate PCMs with separate
            # buffers and the compensation is cheap next to being one buffer out
            # between left and right, which is audible as a smeared image rather
            # than as a fault.
            "combine.latency-compensate" = true;
            "combine.props" = {
              "audio.position" = cfg.mergedPosition;
            };
            "stream.props" = {
              # Link ports by channel name instead of running a channel mixer.
              # Without it the single channel taken off each ADC is upmixed to
              # stereo and both legs land in both outputs -- i.e. mono.
              "stream.dont-remix" = true;
            };
            "stream.rules" = lib.imap0 (i: c: {
              matches = [
                {
                  "node.name" = legName i;
                }
              ];
              actions."create-stream" = {
                "audio.position" = c.take;
                "combine.audio.position" = c.mapsTo;
              };
            }) captures;
          };
        };
      in
      {
        options.hardware.turntable = {
          card = mkOption {
            type = types.str;
            example = "PCH";
            description = ''
              The ALSA card NAME, as the second column of `/proc/asound/cards`
              prints it (`aplay -l` says `card N: <name> [...]`).

              NO DEFAULT, on purpose. "PCH" is the usual name for a Realtek on
              an Intel PCH and it is still a guess; an unset value is an
              evaluation error that names this option, whereas a wrong one is a
              capture node pointed at some other card. See the header.
            '';
          };

          captures = mkOption {
            type = types.listOf (types.submodule captureModule);
            default = [ ];
            example = [
              {
                device = 0;
                take = [ "FL" ];
                mapsTo = [ "FL" ];
              }
              {
                device = 2;
                take = [ "FL" ];
                mapsTo = [ "FR" ];
              }
            ];
            description = ''
              The PCMs this turntable is read from, in order. Empty means this
              aspect emits no pipewire configuration at all.

              One entry is one stereo jack and produces a plain capture node.
              More than one produces those nodes plus a `combine-stream` source
              that merges them, and then each entry must say `mapsTo`.
            '';
          };

          rate = mkOption {
            type = types.ints.positive;
            default = 48000;
            description = ''
              Capture rate, in Hz. 48000 because both consumers want it:
              ../services/vban.nix's `rate` must be one VBAN's header can
              encode and defaults to 48000, and Music Assistant's
              `sendspin_source` declares a fixed 48000/16/stereo output
              (`OUTPUT_SAMPLE_RATE` in its `constants.py`). Matching here means
              nothing in the path resamples.
            '';
          };

          format = mkOption {
            type = types.str;
            default = "S32LE";
            description = ''
              PCM format to ask ALSA for. S32LE rather than the S16LE the VBAN
              stream sends: the ADC is read once and the conversion down to
              what the wire carries happens in the adapter, so asking for the
              wider format here costs nothing and does not throw away bits
              before any processing.

              The codec must give this DIRECTLY, since `api.alsa.path` is `hw:`
              and no longer `plughw:` -- see the note beside it. On the ALC1220
              `--dump-hw-params` reports `S16_LE S32_LE`, so it does; a codec
              that offers neither needs this set to one it has rather than a
              conversion layer in the driver's timing path.
            '';
          };

          mergedPosition = mkOption {
            type = types.listOf channel;
            default = [
              "FL"
              "FR"
            ];
            description = "Channel map of the merged source. Only read when there is a merge.";
          };

          nodeName = mkOption {
            type = types.strMatching "[A-Za-z0-9_.-]+";
            default = "turntable";
            description = ''
              Name of the node a consumer targets. With one capture this names
              that capture directly; with several it names the merged source and
              the legs become `<name>_adc<n>`. Either way `sourceNode` below is
              the one thing a consumer reads, so the distinction does not leak.
            '';
          };

          description = mkOption {
            type = types.str;
            default = "Turntable";
            description = "`node.description`, i.e. what a mixer UI shows.";
          };

          sourceNode = mkOption {
            type = types.str;
            readOnly = true;
            default = if filtering then cleanName else cfg.nodeName;
            description = ''
              The pipewire node name of the finished turntable source, for
              ../services/vban.nix's `source` and anything else that consumes
              it. Read-only and DERIVED, which is the whole point: a consumer
              does not have to know whether a merge happened, or whether
              `filter` below inserted a filter-chain node after it.

              That is what makes the filter free to add. ../services/vban.nix
              reads this and nothing else, so turning on `filter.mains` moves
              the stream onto the filtered node with no edit there -- the same
              property that let the two-ADC merge disappear without touching
              the sender.
            '';
          };

          filter = {
            mains = mkOption {
              type = types.nullOr (
                types.enum [
                  50
                  60
                ]
              );
              default = null;
              description = ''
                Mains frequency to notch out, or null for no notches. An enum
                because there are two answers on Earth and a typo here is a
                filter that removes nothing while looking configured.

                See the note in this file's `filtering` binding for the measured
                spectrum this is built around, including why only the ODD
                harmonics are worth notching and why 12 dB is the ceiling.
              '';
            };

            harmonics = mkOption {
              type = types.listOf types.ints.positive;
              default = [
                1
                3
                5
                7
                9
                11
              ];
              description = ''
                Which multiples of `mains` to notch. Odd only by default,
                because a hum field symmetric about zero -- a saturating
                transformer core, which is what auspc measured -- has no even
                harmonics to speak of: 20-25 dB less, consistently, across the
                series.

                Set it to `[ 1 2 3 4 ]` for a 100 Hz-dominant spectrum, which is
                supply ripple or a ground loop rather than induction. The
                measurement that tells the two apart is one `bandpass` at
                100 Hz against one at 150.
              '';
            };

            q = mkOption {
              type = types.numbers.positive;
              default = 20;
              description = ''
                Q of the FIRST notch; higher harmonics scale with it so every
                notch is the same width in Hz (`mains / q`, so 2.5 Hz at the
                default). Narrower misses when the mains frequency wanders,
                wider starts thinning the bass -- at 50 Hz there is real music
                a few Hz away.
              '';
            };

            rumble = mkOption {
              type = types.nullOr types.ints.positive;
              default = null;
              description = ''
                High-pass corner in Hz for platter rumble, or null for none.
                Null by DEFAULT and on auspc, which is a measurement rather than
                an omission: subsonic content there is -43.9 dB, twenty dB below
                the hum, so a rumble filter would be solving a problem this deck
                does not have. A warped record or a worn bearing is what makes
                it worth setting.
              '';
            };
          };

          mixerControls = mkOption {
            type = types.listOf (
              types.submodule {
                options = {
                  control = mkOption {
                    type = types.str;
                    example = "Input Source";
                    description = "ALSA simple-control name, as `amixer -c <card> contents` prints it.";
                  };
                  index = mkOption {
                    type = types.ints.unsigned;
                    default = 0;
                    description = ''
                      Which instance of that control. THIS IS THE WHOLE POINT on
                      a two-ADC codec: the second `Input Source` is index 1, and
                      pointing index 0 and index 1 at different pins is what
                      stops both capture devices reading the same jack.
                    '';
                  };
                  value = mkOption {
                    type = types.str;
                    example = "Line";
                    description = "The enum value to select, spelled as the card spells it.";
                  };
                };
              }
            );
            default = [ ];
            description = ''
              ALSA mixer settings applied by a oneshot unit once the sound card
              is up. See the header for why this is a shell-out and what that
              does and does not buy.
            '';
          };

          releaseDevices = mkOption {
            type = types.listOf types.str;
            default = [ ];
            example = [ "~alsa_card\\.pci-0000_00_1f\\.3.*" ];
            description = ''
              wireplumber `device.name` patterns to DISABLE, because this aspect
              opens the same card statically.

              THIS IS THE PART THE SKETCH WAS MISSING. An ALSA capture device
              opens exclusively. wireplumber's ALSA monitor claims the card's
              main analog PCM for its own `alsa_input.pci-...` node under the
              default profile, so a `context.objects` node on `DEV=0` and
              wireplumber race for it and one of them loses -- which presents as
              an intermittently missing source rather than as an error.

              The card's SECOND capture PCM is usually not part of any profile
              and so is not contended, which is why a two-ADC arrangement may
              need to release only one node, or none at all if neither device is
              the main one.

              MEASURED on auspc, and it is worse than "one of them loses": with
              wireplumber running and no rule, pipewire logs the contention from
              both sides at once --

                spa.alsa: 'front:0': capture open failed: Device or resource busy
                pw.node: (alsa_input.pci-0000_00_1f.3.analog-stereo-58)
                         suspended -> error (Start error: Device or resource busy)
                mod.adapter: can't get format: Device or resource busy

              -- and the static node stays `state: "running"` at the graph level
              with its links intact while its adapter never got a format. So it
              presents as a source that exists, accepts links, and emits
              nothing: no error on the node you are looking at, and silence
              downstream. A VBAN sender fed by it sends no packets at all.

              The failure is at node START, so `node.disabled` does prevent it,
              and a node-level rule was measured to work: with it in place the
              `alsa_input.pci-...` ports leave `pw-link -o` and the static node
              gets the card.

              ── WHY THIS IS THE DEVICE AND NOT THE NODE ──

              Node-level disabling fixes the PCM contention and leaves a second
              fault in place, which this file previously carried as an
              unverified risk and which is now measured: the ACP DEVICE survives
              a disabled node, it owns the card's MIXER, and it restores its
              saved mixer state every time pipewire or wireplumber starts. On
              auspc that put `Capture` back to `63 [100%] [30.00dB]` -- +30 dB
              into a line-level deck, which clips it into a square wave --
              repeatedly, behind a `mixerControls` unit that had already set it
              correctly. It reads as the input level drifting on its own.

              `device.disabled` removes the device, and with it every node the
              device would have created, so it subsumes the node-level rule
              rather than complementing it. That is why there is no
              `releaseNodes` any more.

              A pattern list rather than something derived from `card` above:
              wireplumber names its devices after the PCI/USB address, not the
              ALSA card name, so there is nothing to derive it from here and a
              second way of spelling the same card would be a second thing that
              can be wrong.
            '';
          };
        };

        config = lib.mkIf (captures != [ ]) {
          assertions = [
            {
              assertion = merging -> lib.all (c: c.mapsTo != null) captures;
              message = ''
                hardware.turntable.captures: with more than one capture every
                entry needs `mapsTo`, naming where its `take` channels land in
                the merged source.
              '';
            }
            {
              assertion =
                merging
                -> lib.all (c: c.mapsTo == null || builtins.length c.mapsTo == builtins.length c.take) captures;
              message = ''
                hardware.turntable.captures: `mapsTo` maps `take`'s channels one
                for one, so the two lists must be the same length.
              '';
            }
            {
              # `combine.props.audio.position` declares the merged source's
              # channels; a channel no leg maps onto is a port that exists and
              # is always silent, which is the failure this catches.
              assertion =
                merging
                ->
                  lib.sort (a: b: a < b) (lib.concatMap (c: c.mapsTo) captures)
                  == lib.sort (a: b: a < b) cfg.mergedPosition;
              message = ''
                hardware.turntable: every channel of `mergedPosition` must be
                fed by exactly one capture channel, and no capture may map onto
                a channel that is not in it.
              '';
            }
          ];

          # Loud, but not fatal. The retask is what makes these pins inputs --
          # except that node 0x1a on auspc's ALC1220 is ALREADY `Line In at Ext
          # Rear`, so a single-jack arrangement on that pin genuinely needs no
          # patch. An assertion would refuse that working configuration; a
          # warning says the thing worth saying and lets it build.
          warnings =
            lib.optional (merging && cfg.mixerControls == [ ]) ''
              hardware.turntable: ${toString (builtins.length captures)} capture
              devices are merged but `mixerControls` is empty, so which jack each
              ADC reads is left to whatever the codec defaults to. If both default
              to the same pin the result is ONE signal duplicated across both
              channels -- audio that plays and is silently mono. See this file's
              header.
            ''
            ++ lib.optional (config.hardware.hdaJackRetask.pins == { }) ''
              hardware.turntable: capturing from ${cfg.card} with
              hardware.hdaJackRetask.pins empty, so no jack retasking is part of
              this configuration. That is correct only if every capture above
              reads a pin the codec already defaults to an input (on auspc's
              ALC1220 that is node 0x1a). Otherwise the pin defaults are set by
              hand on the running machine and will be gone after the next boot --
              see ../nixos/hda-jack-retask.nix.
            '';

          services.pipewire = {
            enable = true;
            alsa.enable = true;
            pulse.enable = true;
            wireplumber.enable = true;
          };

          # `extraConfig.pipewire.<name>` is the option the LOCKED nixpkgs has
          # (checked: nixos/modules/services/desktops/pipewire/pipewire.nix
          # builds a `linkFarm` of `share/pipewire/pipewire.conf.d/<name>.conf`
          # and `environment.etc.pipewire` points at it). Not `configPackages`,
          # which is for a package that ships its own conf.d, and not
          # `wireplumber.extraConfig`, which is the session manager's config and
          # cannot create a node.
          #
          # Rendered as strict JSON by `pkgs.formats.json`, which SPA-JSON reads
          # -- so the channel maps are Nix LISTS here and come out as
          # `["FL","FR"]`, the form upstream's module documentation uses, rather
          # than the sketch's `"FL,FR"` string. (That string also parses:
          # `spa_audio_parse_position_n` opens it with
          # `spa_json_begin_array_relax`. The list is simply the documented one.)
          #
          # 50 and 60, so the nodes exist before the module that merges them is
          # loaded. Drop-ins are read in name order.
          services.pipewire.extraConfig.pipewire = {
            "50-turntable-adc"."context.objects" = lib.imap0 adapterFor captures;
          }
          // lib.optionalAttrs merging {
            "60-turntable-merge"."context.modules" = [ combineModule ];
          }
          // lib.optionalAttrs filtering {
            # 70, after the merge at 60: this one's `target.object` names the
            # node the merge creates, and although filter-chain waits for a
            # target to appear rather than failing, there is no reason to make
            # it wait.
            "70-turntable-filter"."context.modules" = [ filterModule ];
          };

          services.pipewire.wireplumber.extraConfig."50-turntable-release" =
            lib.mkIf (cfg.releaseDevices != [ ])
              {
                "monitor.alsa.rules" = map (pattern: {
                  matches = [ { "device.name" = pattern; } ];
                  actions.update-props."device.disabled" = true;
                }) cfg.releaseDevices;
              };

          # THE ADC ROUTING, as a unit. See the header for why there is no
          # option-shaped way to say this.
          #
          # `sound.target` rather than a udev-bound unit: the controls belong to
          # the card and exist for as long as it does, so this has to happen
          # once per boot and not once per device event. `RemainAfterExit` so a
          # `systemctl status` says whether the routing was applied, which is
          # the first thing to look at if the merge comes out mono.
          #
          # THIS UNIT IS NOT SUFFICIENT ON ITS OWN, and that is measured rather
          # than suspected now. wireplumber's ACP device owns the card's mixer
          # and restores its own saved state whenever it starts, which undoes
          # every control set here -- on auspc it put `Capture` back to +30 dB
          # repeatedly while this unit had already set it correctly, and the
          # symptom was a line input that clipped into a square wave for no
          # visible reason. `releaseDevices` above is what stops it: a disabled
          # DEVICE has no mixer to restore, where a disabled node left one.
          #
          # So a host setting `mixerControls` and not `releaseDevices` has a
          # race it will lose, and the first sign is a level that changes
          # without anything changing it. `amixer -c <card> sget <control>`
          # before and after a pipewire restart is the way to see it.
          systemd.services.turntable-adc-routing = lib.mkIf (cfg.mixerControls != [ ]) {
            description = "Point each turntable capture device at its own jack";
            after = [ "sound.target" ];
            wantedBy = [ "multi-user.target" ];
            path = [ pkgs.alsa-utils ];
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
            };
            script = lib.concatMapStringsSep "\n" (
              c:
              "amixer -c ${lib.escapeShellArg cfg.card} -- sset ${lib.escapeShellArg "${c.control},${toString c.index}"} ${lib.escapeShellArg c.value}"
            ) cfg.mixerControls;
          };
        };
      };
  };

  # ── WHY VBAN AND NOT SENDSPIN, FOR NOW ──
  #
  # Music Assistant 2.10.6 (the version nixpkgs locks) ships a builtin plugin
  # `sendspin_source`, "Play live audio inputs (line-in, turntable, microphone)
  # from Sendspin clients that support the source role", and the nixpkgs module
  # installs it on every build via `providersBuiltins` -- so the RECEIVING end of
  # that route already exists on secondpc with no configuration change. On
  # paper it is the better protocol for this: TCP with pairing instead of
  # unidirectional UDP at 6980, server-initiated so nothing is sent while nobody
  # is listening, a variable-rate resampler bridging the ADC's clock to the
  # player's, and a `line_sense` feature that starts playback when the needle
  # goes down. None of which VBAN has.
  #
  # It is not implementable here yet, and the reason is on the SENDING side.
  # `aiosendspin` (read in the store: `aiosendspin/client/source.py`) has the
  # protocol -- `SourceCapture`, Noise pairing, role negotiation -- but no
  # capture backend whatsoever: no ALSA, no pipewire, no sounddevice anywhere in
  # the package, and `SourceCapture` is documented as something you "feed PCM"
  # to. There is no sendspin source client in nixpkgs and no binary to run, so
  # the client is a program that would have to be written: pipewire capture, an
  # Opus encoder, timestamping against the client clock, and a pairing PIN typed
  # into the Music Assistant UI once -- which is not declarative either.
  #
  # So VBAN stays the transport and nothing here is ripped out. What this file
  # changes is that the transport now reads `sourceNode` above instead of naming
  # an ALSA node itself, which is exactly the seam a sendspin client would
  # attach to: it would be a second consumer of the same node, in its own
  # aspect, with no change to this one.
}
