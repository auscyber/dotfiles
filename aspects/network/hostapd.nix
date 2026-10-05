{
  den,
  lib,
  houseWifi,
  ...
}:
# A second 2.4GHz AP for the house network, bridged onto the LAN rather than
# routed behind its own subnet -- wireless clients land in 192.168.0.0/16 and
# take their lease from the house router at 192.168.0.1, exactly like a device
# plugged into the switch. There is deliberately no DHCP or DNS server here.
#
# DANGER -- this aspect touches the L2 path that SSH to this box runs over.
# ../hosts/secondpc/default.nix already defines `br0` as the bridge holding
# `enp2s0` and carrying `lanAddress`, so the AP only has to JOIN that bridge:
# this file does NOT create a bridge, does NOT enslave enp2s0 and does NOT move
# the host address. If it did any of those, a mistake would strand the box. The
# only member added to br0 is a USB wireless adapter, which carries no path back
# to the switch, so no loop is possible and enp2s0's forwarding is untouched.
#
# Hardware (read off the live box, kernel 6.18.50): USB 0846:9072, MediaTek
# MT7925 driven by `mt7925u`. `iw list` reports AP in "Supported interface
# modes", so hostapd can drive it, but the interface combinations are
# `#{ AP } <= 1 ... #channels <= 1` -- ONE BSS on ONE channel. A dual-band AP is
# physically impossible on this radio; 5GHz would mean giving up 2.4GHz.
#
# That same `#{ AP } <= 1` is why only ONE SSID is declared below. Serving both
# house SSIDs ("Hallway Beehive" and "Bedroom Beehive") off this stick was asked
# for and cannot be done: a second SSID needs either a second AP interface,
# which the combination forbids, or MBSSID, which is absent from the adapter's
# "Supported extended features". hostapd would fail at startup creating the
# second BSS. AP/VLAN is advertised but does not help -- it tags stations on the
# existing BSS onto VLANs, it does not carry a second SSID. A second SSID needs
# a second radio.
#
# The adapter advertises HE (WiFi 6) for the AP iftype on all three bands, VHT
# only on 5GHz, and NO EHT at all -- so despite MT7925 being a WiFi 7 part, the
# driver exposes WiFi 6 as the ceiling here and `wifi7` would be a lie.
{
  den.aspects.hostapd = {
    includes = [ den.aspects.agenix-rekey ];

    # The house WPA passphrase, which is an EXTERNAL fact: this AP advertises the
    # same SSID as the existing router so clients roam between them, and that
    # only works if the key matches what the router was configured with. So no
    # generator -- neither a random one (the router would reject the clients) nor
    # a `decrypt`-based one derived from ../hosts/birdsarentreal.nix's
    # `bees_wifi.age`, which is the same PSK but has never been populated (0
    # bytes; see security/recover-master.sh). A plain rekeyFile it is.
    #
    # MUST CONTAIN A BARE PASSPHRASE, one line, nothing else. The neighbouring
    # bees_wifi.age holds `bees_psk=<passphrase>` because wpa_supplicant's `ext:`
    # indirection wants a key=value file; hostapd does NOT, and would treat the
    # whole `bees_psk=...` line as the passphrase.
    #
    # That one bare line then serves both password options below: hostapd's SAE
    # entry format is `<password>[|mac=...][|vlanid=...]`, so a file holding only
    # a passphrase is simultaneously a valid `wpaPasswordFile` and a valid
    # `saePasswordsFile`.
    secrets.passphrase = {
      rekeyFile = houseWifi.secretFile;
      restartUnits = [ "hostapd.service" ];
    };

    nixos =
      { config, scoped, ... }:
      {
        options.hostapd.ssid = lib.mkOption {
          # ── AN ENUM OF THE DECLARED SSIDS ──
          #
          # `#{ AP } <= 1` on this chipset (see the header), so the radio
          # carries exactly one of the house's networks and has to say which.
          # The type is what checks it: a typo or a stale name is refused
          # naming the valid ones, where `nonEmptyStr` accepted
          # "CHANGEME-house-ssid" for as long as nobody looked.
          #
          # It was deliberately NOT an enum while that placeholder was in
          # ../hosts/secondpc/default.nix -- an enum would have turned an
          # unfinished deploy into a refusal to evaluate that host. The real
          # SSID is set now, so the check costs nothing and catches the next
          # rename of a network this radio is supposed to join.
          type = lib.types.enum houseWifi.allSsids;
          description = ''
            SSID to advertise. Set this to the EXISTING house network's name so
            this radio becomes another BSS of the same ESS and clients roam onto
            it, rather than a second network competing for the same airtime.

            Declared in ../network/house-wifi.nix as `houseWifi.ssids`; name one
            of them (`houseWifi.ssids.bedroom`) rather than repeating the
            string, so a renamed network is renamed once.
          '';
        };

        config = {
          # Pin the interface name by MAC. This is load-bearing, not tidiness:
          # the adapter was seen as both `wlp3s0u4` and `wlp0s20u9` during
          # setup, because the predictable name of a USB NIC encodes the USB
          # path and changes when it is moved to another port. `services.hostapd
          # .radios` is keyed by interface name and the unit `bindsTo` the
          # matching `.device` unit, so an unpinned name means the AP silently
          # stops existing the next time the stick is replugged elsewhere.
          systemd.network.links."10-ap0" = {
            matchConfig = {
              MACAddress = "28:94:01:c1:ed:dd";
              Type = "wlan";
            };
            linkConfig.Name = "ap0";
          };

          # Hand the interface to hostapd and keep networkd off it. Setting the
          # radio to AP mode requires the interface DOWN, so if networkd brought
          # it up (or enslaved it to br0) first, hostapd would fail with "Could
          # not set interface to AP mode". hostapd brings it up and adds it to
          # br0 itself via `settings.bridge` below, after the mode is set.
          systemd.network.networks."40-ap0" = {
            matchConfig.Name = "ap0";
            linkConfig.Unmanaged = true;
          };

          services.hostapd = {
            enable = true;

            radios.ap0 = {
              band = "2g";

              # Channel 1, chosen off a live scan of the neighbourhood: of the
              # three non-overlapping 2.4GHz channels, 1 carries two other BSSs,
              # 6 one, 11 two -- but the three BSSs sitting on channel 9 (two of
              # them 40MHz wide) smear across both 6 and 11 and leave 1 the
              # quietest in practice. Not ACS (channel = 0): hostapd fails to
              # start on hardware that cannot do it, and that is not worth
              # discovering remotely.
              channel = 1;

              # Australia/Melbourne on the live box, and the associated client
              # reports `Country Code: AU`. This is required, not cosmetic: the
              # adapter currently sits in the world domain (`iw reg get` ->
              # `country 00`), where channels 12-14 and EVERY 5GHz channel are
              # flagged `no IR` -- no initiating radiation -- so hostapd may not
              # beacon on them. Setting it also forces 802.11d/h on (see the
              # nixpkgs module), which is what advertises the allowed channel
              # and power set to clients.
              countryCode = "AU";

              # HT20, by dropping the module's default `HT40`/`SHORT-GI-40`.
              # 40MHz on 2.4GHz spans most of the band and there are already
              # seven BSSs in earshot; the retries cost more than the nominal
              # width buys.
              wifi4.capabilities = [ "SHORT-GI-20" ];

              # MUST be off. This defaults to TRUE in nixpkgs, which would emit
              # `ieee80211ac` and `vht_capab` into a `hw_mode=g` radio -- VHT is
              # 5GHz-only and the adapter advertises VHT on band 2 alone.
              wifi5.enable = false;

              # Band 1 does advertise "HE Iftypes: AP", so 802.11ax on 2.4GHz is
              # real here. `operatingChannelWidth` is left at the module's
              # `20or40` default, which is the only correct value for this band.
              wifi6.enable = true;

              networks.ap0 = {
                inherit (config.hostapd) ssid;

                # Join the LAN bridge. There is NO `bridge` option on this
                # module -- it was rewritten and bridging is only reachable
                # through the per-BSS freeform `settings`, which lands as
                # `bridge=br0` in the BSS block of hostapd.conf.
                settings.bridge = "br0";

                authentication = {
                  # The router this extends is WPA2-Personal, so WPA2 has to
                  # stay available or existing devices stop associating; SAE is
                  # offered alongside it for clients that can do WPA3. The
                  # module asserts transition mode defines exactly one wpa
                  # password option AND one sae option, which is why both appear
                  # here pointing at one file.
                  mode = "wpa3-sae-transition";

                  # Left FALSE deliberately (also the default). Transition
                  # Disable tells a client to drop WPA2 for this SSID forever
                  # after one WPA3 association -- and the house router is
                  # WPA2-only under the same SSID, so turning it on would stop
                  # clients connecting to the ROUTER.
                  transitionDisable = false;

                  wpaPasswordFile = scoped.hostapd.secrets.passphrase.path;
                  saePasswordsFile = scoped.hostapd.secrets.passphrase.path;
                };
              };
            };
          };

          # No firewall change, and that is the point. Frames bridged between
          # ap0 and enp2s0 are forwarded at L2; br0 has `nf_call_iptables 0`
          # (checked live), so they never enter the host's filter chains and the
          # router's DHCP/DNS reaches wireless clients untouched. Traffic
          # ADDRESSED to this box still arrives on br0 and still meets the
          # existing `networking.firewall` rules in ../hosts/secondpc -- so
          # bridging grants wireless clients exactly the access a wired client
          # already had, and nothing more.
          #
          # If clients associate but get no lease, suspect that: docker loads
          # br_netfilter and sets the global bridge-nf sysctl to 1 for its own
          # bridges, and a kernel that honoured it for br0 too would push
          # forwarded frames into docker's `FORWARD DROP` policy.
        };
      };
  };
}
