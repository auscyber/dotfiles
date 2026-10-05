{ den, ... }:
# What used to be the Music Assistant add-on.
let
  # Music Assistant's webserver. Not an option on the nixpkgs module -- it
  # is the server's own default and the module passes no `--port` -- so this
  # is the one place the number is written, and `gives.tcp` below is how
  # anything else learns it.
  musicAssistantPort = 8095;

  # The Sendspin server's own port, likewise not an option on the module -- the
  # provider's README fixes the endpoint at `ws://<host>:8927/sendspin`.
  sendspinPort = 8927;
in
{
  # ── ITS OWN ASPECT, PULLED IN BY HOME ASSISTANT ──
  #
  # Same move as ./esphome.nix, and for the same reason: this file and that one
  # both wrote into `den.aspects.homeassistant`, which works for class content
  # (den merges it) and breaks for a `takes.<cap>` -- a take is keyed by the
  # taking ASPECT, so two services on one aspect contend for one slot.
  #
  # `den.aspects.music-assistant` already existed in this file, holding the
  # component this service pushes into HA and the account it runs as -- in a
  # second block at the bottom. Both halves are now one aspect, which is the
  # same "two halves of one thing in two places" the serviceType/aspect split
  # was.
  den.aspects.homeassistant.includes = [ den.aspects.music-assistant ];

  den.aspects.music-assistant = {
    # Same reasoning as ./esphome.nix: an ex-ingress panel with no auth of its
    # own, so the browser leg goes through oauth2-proxy. A PORT, not an
    # upstream -- the proxy supplies the scheme and the loopback address.
    takes.gated = {
      port = musicAssistantPort;
      websockets = true;
    };

    # WHAT HOME ASSISTANT NEEDS IN ORDER TO TALK TO THIS SERVICE, contributed BY
    # this service through the edge rather than sitting in ./core.nix's
    # `extraComponents` list -- which put "HA integrates with Music Assistant"
    # in a file that knows nothing about Music Assistant, and left a dangling
    # entry behind if the service ever went away.
    #
    # This was briefly a separate `den.aspects.music-assistant-hass` writing
    # `services.home-assistant.extraComponents` directly. Same parallel-channel
    # mistake as a quirk: it works, but the contribution has no wanter identity,
    # so "who asked for this component" is unanswerable and nothing ties it to
    # the service's lifetime.
    # A HOUSE AMENITY. The VBAN receiver below is the whole reason this is not
    # `host`: auspc's turntable streams to it across the LAN, so a take on
    # another machine in this household has to be able to resolve here.
    # ../households/ivymectin.nix used to say this instead.
    reach = "household";

    takes.home-assistant.extraComponents = [ "music_assistant" ];

    # A BROKER, ASKED FOR RATHER THAN ASSUMED. Music Assistant publishes player
    # state over MQTT where one is available, and ./mosquitto.nix gives it an
    # account because of this line -- not because a `users` block somewhere
    # happened to list it.
    takes.mqtt = { };

    gives =
      { self, ... }:
      {
        # A SERVICE THE HOUSE OFFERS.
        service = _: { };

        # The HTTP/websocket API. HA reaches it over loopback when co-located,
        # which is what ../nextcloud.nix's note about not acquiring
        # DNS/TLS/tunnel dependencies for a same-box call is about. NOT exposed:
        # nothing off this box talks to it directly.
        # TWO PORTS THAT MEAN DIFFERENT THINGS, which is why they are named.
        # `http` is the API Home Assistant talks to over loopback; `vban` is the
        # audio receiver auspc's turntable streams to, and it has a DIFFERENT
        # constraint -- a full VBAN packet does not fit a 1420-byte tunnel MTU,
        # so that leg needs the lan while the API does not care.
        #
        # As a single `port` the VBAN receiver was a firewall literal in
        # ./default.nix with a comment explaining the module's `openFirewall`
        # missed it.
        # ── `name`, SO A CONSUMER CAN ASK FOR THIS ONE ──
        #
        # `name` is on `carried` (../../base/capability-types.nix), so every
        # capability has it and a take filters on it. Without it the only thing
        # distinguishing this udp endpoint from home-assistant's mDNS ports on
        # the same box is the port number, and a consumer naming a port number
        # is a consumer that has stopped resolving and started hardcoding --
        # which is exactly what ../vban.nix did with `host = "secondpc"`.
        tcp = _: {
          name = self.name;
          ports.http = {
            port = musicAssistantPort;
            meta.description = "Music Assistant's API and web UI, which Home Assistant dials over the loopback on this same box.";
          };
          # THE EXTERNAL SENDSPIN CLIENTS' PORT, which the note below said to
          # declare here "if a real external client appears". One has:
          # ../sendspin-bluetooth.nix runs `sendspin daemon` on
          # ../../hosts/birdsarentreal.nix and dials
          # `ws://<this host>:8927/sendspin`, so the port is resolved from here
          # rather than written at both ends.
          #
          # `expose` is the house lan and nothing else: the client is a speaker
          # in this house, and the web UI reaches the same server over the
          # loopback. That is also what keeps `openFirewall` off -- see below.
          ports.sendspin = {
            port = sendspinPort;
            meta.description = "The sendspin websocket an external player dials -- birdsarentreal's `sendspin daemon` speaks it over the house lan.";
            expose = [ "lan-home" ];
          };
          carrier = self.name;
        };

        udp = _: {
          name = self.name;
          ports.vban = {
            port = 6980;
            meta.description = "VBAN audio receiver for auspc's turntable. Lan only -- a full VBAN packet does not fit a 1420-byte tunnel MTU.";
            # THE HOUSE LAN, which is also the only carrier that can take this
            # flow: ../vban.nix's take demands a 1500-byte MTU and both
            # tunnels are below it. So the wire the sender must use and the
            # wire this is open on are the same fact, resolved rather than
            # stated twice.
            expose = [ "lan-home" ];
          };
          carrier = self.name;
        };

        user = _: {
          name = "music-assistant";
          carrier = self.name;
        };
      };

    nixos = {
      # `providers` names only the ones with extra dependencies to install;
      # the module folds in the package's builtins itself. This list is the
      # add-on's enabled providers minus those builtins. ../audiobookshelf.nix
      # appends `audiobookshelf` to it from its own file, which works because
      # this is a NixOS list option.
      #
      # `hass` IS NOT FREE HERE THE WAY IT WAS AS AN ADD-ON, and this is the
      # one provider on the list whose move cost something. Music Assistant's
      # own `providers/hass/__init__.py` says it: the URL and auth token "are
      # collected by the setup flow ... unless running as a Home Assistant
      # add-on, where they are fixed" -- under the supervisor it used
      # `http://supervisor/core/api` with the token auto-retrieved. Native,
      # `running_as_hass_addon` is false, so both have to be typed into Music
      # Assistant's own UI once:
      #
      #   Settings -> Providers -> Home Assistant
      #     url    http://127.0.0.1:8123
      #     token  a Home Assistant long-lived access token
      #
      # The URL is the LOOPBACK one, not `home.<domain>`: both run on this box,
      # so the public name would send every call out through the cloudflare
      # tunnel and back (../nextcloud.nix:327,
      # ../../hosts/secondpc/sso.nix:385).
      #
      # The token has no declarative mint -- the same fact ./mcp-server.nix
      # spells out at length and ./state.nix records about `.storage` being the
      # only copy of one. It lives only in /var/lib/private/music-assistant,
      # which is why ./state.nix snapshots that directory.
      #
      # ── NO `openFirewall = true`, AND THE REASON HAS CHANGED ──
      #
      # This comment used to say the option DOES NOT EXIST. On the nixpkgs this
      # flake now locks (`nix eval --raw .#inputs.nixpkgs.rev` -> f20cd90e) it
      # does: `openFirewall` is declared and writes a `networking.firewall`
      # block. The conclusion survives, the premise does not -- so here is what
      # the option actually opens, read off the module:
      #
      #   TCP 8097 always; 5000 and 7000 with `airplay`; 8927 with `sendspin`
      #     -- which is EVERY build, see below;
      #   1780 with `snapcast`; 3483/9000/9090 with `squeezelite`;
      #   TCP 32768-65535 with `spotify_connect`;
      #   UDP 3483 with `squeezelite`;
      #   UDP 32768-65535 with `airplay`.
      #
      # We enable `airplay`, so turning it on would open THE WHOLE EPHEMERAL
      # UDP RANGE -- 32768-65535, on every interface, because the module writes
      # the global lists which carry no `-i`. That is not a redundant rule, it
      # is a worse one than anything this aspect declares: ../../base/firewall.nix
      # opens a port on exactly the carriers its `expose` names, which for the
      # VBAN receiver is the house lan and nothing else.
      #
      # And it still would not cover what the service genuinely needs: VBAN's
      # UDP 6980 is in no branch above, which is what the comment this replaces
      # was reaching for when it was written against a module that had no
      # firewall code at all.
      #
      # ── `sendspin` IS NOT IN THE LIST BELOW, AND MUST NOT BE ──
      #
      # Sendspin is Music Assistant's NATIVE playback protocol, not an optional
      # provider: `providers/sendspin/manifest.json` is `"builtin": true` with
      # `"allow_disable": false`, and the module itself writes
      # `services.music-assistant.providers = cfg.package.providersBuiltins`
      # (so does `sendspin_source`). The list below merges with that one, so
      # the protocol and its `aiosendspin[server]` dependency are present
      # whatever this file says -- and naming it here would be a second
      # definition of a fact the module already supplies.
      #
      # It also means the `8927 with sendspin` branch above is reached on EVERY
      # build, since `lib.elem "sendspin" cfg.providers` is always true.
      #
      # 8927 is for EXTERNAL Sendspin clients (speakers and mobile apps that
      # are not the web UI). There is now one -- ../sendspin-bluetooth.nix on
      # ../../hosts/birdsarentreal.nix -- so the port IS declared, in
      # `gives.tcp.ports.sendspin` above with `expose = [ "lan-home" ]`, which
      # is where this note already said it would belong. Not in `openFirewall`,
      # for the reason above: that option would take the ephemeral UDP range
      # with it.
      #
      # The other `sendspin` in this tree is still unrelated: it is an MQTT
      # USERNAME for the ESPHome nodes (../homeassistant/esphome.nix's
      # `takes.mqtt.username`), and Sendspin does not authenticate over MQTT --
      # so the matching name is not evidence that those nodes speak it.
      #
      # `extraOptions` IS load-bearing, though: the module defaults it to
      # `[ "--config" "/var/lib/music-assistant" ]`, `--config` is an alias of
      # `--data-dir`, and overriding it without that pair moves the state
      # directory to `$HOME/.musicassistant` -- which ./state.nix would then be
      # snapshotting the wrong path for.
      services.music-assistant = {
        enable = true;
        providers = [
          "hass"
          "airplay"
          "airplay_receiver"
          "chromecast"
          "dlna"
          "sonos"
          "universal_group"
          "vban_receiver"
          "filesystem_local"
          "tidal"
          "lastfm_recommendations"
          "lastfm_scrobble"
          "ambient_sounds"
          "party"
          "smart_fades"
          "milkdrop_visualizer"
        ];
      };

      # NO `allowedUDPPorts` HERE. 6980 was opened by hand AND declared in
      # `gives.udp` above; the hand-written one was the only one that worked,
      # because ../../base/firewall.nix was asking a projected host entity
      # and getting no providers back. With that fixed the declaration is the
      # rule, and it lands on `br0` alone rather than on every wire -- which
      # is also the only wire that can carry the flow, since ../vban.nix's
      # take demands a 1500-byte MTU.
    };
  };
}
