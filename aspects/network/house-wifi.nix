{
  den,
  lib,
  carrierAttachment,
  ...
}:
# THE HOUSE WIFI AS A CARRIER: one aspect that both CONFIGURES the association
# and FURNISHES the link it creates.
#
# That pairing is the whole idea. ./segments.nix's `lan-home` is a carrier that
# configures nothing -- a cable is plugged in by hand and the aspect only
# describes the wire. A radio is not like that: joining the network is config,
# and the link does not exist until that config is on the box. So the aspect
# that says "this host is on the house wifi" is the same one that makes it true,
# and a host needs one include rather than an include plus a block of
# `networking.*` that could silently disagree with it.
#
# WHAT A HOST GETS FROM THE INCLUDE
#
#   * the SSIDs and the passphrase, from where they are already declared;
#   * `tcp` / `udp` links to any other host on the same radio;
#   * `segment`, carrying the house prefix -- so this is described as the same
#     wire the cabled hosts are on rather than a parallel universe.
#
# ⚠ WHAT IT DOES *NOT* GET, measured rather than assumed: furnishing the same
# `prefix` as `lan-home` does NOT make a wifi-only host a sibling of a cabled
# one. ../base/capability.nix's `linksBetween` computes siblinghood by ASPECT
# NAME (`shared = filterAttrs (n: asp: … || a ? ''${n}) b`) and never compares
# two carriers' prefixes, so two hosts are siblings when they INCLUDE a carrier
# in common. The prefix is therefore a true statement about the wire that
# nothing currently reads for that purpose. A host on both -- ../hosts/auspc.nix
# -- is a sibling of the cabled fleet through `lan-home`, which is why that case
# works today; comparing prefixes in `linksBetween` is what would make the
# wifi-only case work, and that is a change to `capability.nix`.
#
# Two facts about this network were written out separately in three places
# before this file existed, which is exactly the drift this repo is organised to
# prevent:
#
# Two facts about this network were written out separately in three places
# before this file existed, which is exactly the drift this repo is organised to
# prevent:
#
#   * the SSIDs, as string literals in ../hosts/birdsarentreal.nix and as prose
#     in ../households/ivymectin.nix's room comment;
#   * the `bees_psk` variable name, in ../hosts/birdsarentreal.nix's template
#     and again in its `pskRaw`.
#
# The PASSPHRASE was already single-sourced -- `secrets/static/wifi_password.age`
# -- and stays that way: this file re-uses that same `rekeyFile`, which is a
# per-host rekey of one encrypted value, not a second copy of it.
#
# `_module.args.houseWifi` rather than a path import, per ../../CLAUDE.md: it is
# evaluated once for the whole flake and no reader hard-codes its position
# relative to this file.
let
  houseWifi = rec {
    # The SSIDs the house actually runs, one per location. Both carry the SAME
    # passphrase, so a client declares both and associates with whichever is in
    # range -- that is a roaming decision for the supplicant, not a config
    # choice.
    #
    # KEYED BY LOCATION, not a bare list, so that one of them can be REFERRED
    # TO rather than repeated. ../network/hostapd.nix's radio extends exactly
    # one of these (`#{ AP } <= 1` on that chipset), and it has to say WHICH --
    # `ssids.bedroom` says it without putting the string in a second file,
    # which a list could not do.
    ssids = {
      hallway = "Hallway Beehive";
      bedroom = "Bedroom Beehive";
    };

    # Every SSID, for a client that declares all of them and lets the
    # supplicant choose.
    allSsids = lib.attrValues houseWifi.ssids;

    # The variable name the passphrase is delivered under. Both consumers want
    # `<var>=<passphrase>` on one line and read it by name at runtime, so the
    # key never reaches the store:
    #
    #   wpa_supplicant   `pskRaw = "ext:<var>"` against `secretsFile`
    #   NetworkManager   `psk = "$<var>"`, envsubst'd from `environmentFiles`
    #
    # systemd's `EnvironmentFile` parser and wpa_supplicant's `file:` backend
    # agree on that format, which is why ONE template serves both.
    pskVar = "bees_psk";

    # The ENCRYPTED PASSPHRASE, named once so the AP and every client point at
    # the same file by construction rather than by three matching path
    # literals. One bare line, the passphrase and nothing else:
    #
    #   nix run .#secret-edit -- secrets/static/wifi_password.age
    #
    # agenix rekeys it per host, so this is one value with several recipients
    # and not a per-host copy that can drift. Bare rather than key=value
    # because hostapd would otherwise take the whole `bees_psk=...` line as the
    # passphrase; the clients that want key=value get it from a TEMPLATE over
    # this, which is why `pskVar` above is a fact and not a literal.
    secretFile = ../../secrets/static/wifi_password.age;
  };
in
{
  _module.args.houseWifi = houseWifi;

  # THE CARRIER AND THE CLIENT, as one aspect. Include it and the host both
  # joins the house network and is described as being on it; it names no SSID,
  # no passphrase and no prefix of its own. The one thing it must still state is
  # its own `attachments.house-wifi`, because which NIC hears the radio is a
  # fact about the hardware and nothing can derive it.
  #
  # NetworkManager rather than `networking.wireless`, which is the other shape
  # in this tree (../hosts/birdsarentreal.nix): the hosts that will include this
  # are desktops that already run NetworkManager for the applet, and the two
  # cannot both drive a radio -- NM takes wpa_supplicant over DBus and asserts
  # against `networking.wireless.networks` unless interfaces are handed over by
  # name. Declaring the profile through NM means nothing has to be taken off NM,
  # and an interface added later is picked up without an edit here.
  den.aspects.house-wifi = {
    # NOT `includes = [ den.aspects.lan-home ]`, although this claims the same
    # prefix: that include is what DECLARES a wired attachment, and a host on
    # wifi has none. A machine with both says so with both includes (and both
    # attachments), which ../hosts/auspc.nix does.
    includes = [ den.aspects.agenix-rekey ];

    # ── THE LINK THIS ASSOCIATION CREATES ──
    #
    # Shaped after ./segments.nix's `lan-home`, including the asymmetry that
    # file explains at length and which is the easiest thing here to get
    # backwards:
    #
    #   ADDRESS from the PEER      -- where THIS host dials THAT one
    #   INTERFACE from THIS host   -- where THIS host HEARS the carrier, which
    #                                 is what ../base/firewall.nix opens a port
    #                                 on. Taking it off the peer would open one
    #                                 host's ports on another's NIC name.
    #
    # Both halves come from `attachments.house-wifi` on the respective entity
    # (../hosts/fleet.nix), so no host has to be evaluated to answer either.
    #
    # ITS OWN ATTACHMENT KEY, not `lan-home`'s: the NIC name differs (auspc
    # hears the cable on `enp6s0` and the radio on `wlo1`) and a host may hold
    # both at once, so one key could not answer for both.
    #
    # NO FALLBACK TO `peer.lanAddress`, which `lan-home` does have. That value
    # is the address on the CABLE; handing it back as the address of a wifi link
    # would be the same class of mistake as reading the interface off the peer
    # -- right-looking, and wrong about which wire the packet leaves on.
    gives =
      {
        self,
        host ? { },
        ...
      }:
      let
        link =
          { peer, ... }:
          {
            # `${self.name}` rather than the literal `house-wifi`: see
            # ./segments.nix for why an aspect must not restate its own
            # identity to look itself up.
            address = peer.attachments.${self.name}.address or null;
            interface = host.attachments.${self.name}.interface or null;

            # 1500, because 802.11 presents 1500 to IP -- the radio's own
            # fragmentation is below the layer this describes.
            #
            # A CONSEQUENCE WORTH KNOWING RATHER THAN GUARDING: ./segments.nix
            # calls ethernet "the one carrier that can take `realtime-audio`"
            # because ../services/vban.nix needs a full 1500-byte packet to
            # fit, and that is an MTU comparison -- which this carrier passes.
            # It should not WIN one: wifi jitter and loss are exactly what a
            # VBAN stream has no recovery for. Nothing new filters it out,
            # because nothing has to -- `preference` below puts ethernet first
            # on any host holding both, which is every host that could
            # plausibly stream.
            #
            # READ OFF THE ASPECT (`mtu` below) rather than written here: the
            # number is the same for every pair on this radio, so it is a
            # carrier fact and not part of the relation --
            # ./carrier-attachment.nix's roster reads the one declaration.
            inherit (self) mtu;

            # No proxy in the path, so nothing caps a body.
            inherit (self) maxBodyBytes;

            # No `carrier`: ../base/capability.nix derives it from the aspect
            # that furnished the link, which is this one.
          };
      in
      {
        tcp = link;
        udp = link;

        # ── NO `segment`, AND THAT IS THE CORRECTION ──
        #
        # This used to furnish one, with `prefix` read off `lan-home` so that
        # "the same wire" was by construction. It was by construction, and it
        # was the wrong construction: ../base/capability-types.nix's `segment`
        # is "Attachment to a shared wire. Several hosts on it are siblings",
        # so TWO segments carrying ONE prefix are one network under two names
        # -- and `preference = 25` against `lan-home`'s 20 was a tiebreak
        # between synonyms, not between routes.
        #
        # So this aspect is an attachment MECHANISM and not a carrier: it
        # configures the association and yields an interface onto `lan-home`,
        # which is what ./carrier-attachment.nix's
        # `den.carrierAttachments.lan-home.attachesVia` below says in one line.
        # ../hosts/auspc.nix's own ⚠ already said so in prose -- "`lan-home`
        # belongs on the radio, which is what the house-wifi carrier providing
        # a lan link was meant to express".
        #
        # `tcp` and `udp` ABOVE STAY, deliberately and only for now:
        # ../services/openrgb.nix names this carrier in `expose` to get a
        # firewall rule onto `wlo1`, and a carrier that furnishes nothing
        # resolves to nothing, so dropping them would silently stop opening
        # that port. The honest finish is `expose = [ "lan-home" ]` plus
        # `attachments.lan-home.interface = "wlo1"` on auspc, which is an edit
        # to a host file.
        #
        # NO `hostname`, for ./segments.nix's reason exactly: nothing serves a
        # zone for this wire, so there is no name a host on it can resolve for
        # another. A consumer wanting a name finds NO PROVIDER -- loud -- rather
        # than a null read as "no name today". ../services/homeassistant/
        # mosquitto.nix carries the same note from the giver's side. Furnishing
        # one here to silence a resolver would be inventing a namespace.
      };

    # ── NO `preference` AND NO `prefix` ──
    #
    # Both existed to rank and to describe a segment this aspect no longer
    # presents. `preference = 25` ordered the radio between the cable (20) and
    # the tunnel (40), which only means something if the radio is a different
    # network from the cable -- and ./hostapd.nix BRIDGES the AP onto the lan
    # rather than routing it, so an associated client takes its lease from the
    # house router exactly like a device plugged into the switch. It is one
    # broadcast domain, and `prefix` said so by reading `lan-home`'s.
    #
    # The ordering question that remains is real and is not this one: on a host
    # holding both wires, which does traffic leave on? That is the kernel's
    # route metric, and ../network/house-wifi.nix's own NM profile already
    # answers it (`route-metric = 700`). It was never `preference`'s to answer
    # -- `preference` ranks CARRIERS, and there is one carrier here.

    # 1500, because 802.11 presents 1500 to IP -- the radio's own fragmentation
    # is below the layer this describes. On the aspect rather than inside the
    # link for ./carrier-attachment.nix's reason: a carrier fact is the same for
    # every pair on it, so it is not half of a relation.
    mtu = 1500;
    maxBodyBytes = null;

    # THE SAME FILE hostapd serves the network from (../network/hostapd.nix) and
    # the same one ../hosts/birdsarentreal.nix joins it with -- by construction
    # now, since all three read `secretFile` above rather than each spelling the
    # path out.
    secrets.wifi-password.rekeyFile = houseWifi.secretFile;

    # `<var>=<passphrase>`, as a pure function of the bare secret above rather
    # than a second hand-entered file -- so hostapd (which wants the file to be
    # JUST the passphrase) and the clients (which want key=value) can no longer
    # disagree.
    #
    # The dependency is in this aspect's OWN scope. A template depending on
    # another scope's secret closes `age.templates` ->
    # `age.scoped.<other>.access` -> `age.templates`; see the note in
    # ../services/postgres.nix.
    templates."wifi.env" =
      { secrets, ... }:
      {
        restartUnits = [ "NetworkManager-ensure-profiles.service" ];
        dependencies = { inherit (secrets) wifi-password; };
        content =
          { placeholders, ... }:
          ''
            ${houseWifi.pskVar}=${placeholders.wifi-password}
          '';
      };

    # A module FUNCTION, not a bare attrset: as an attrset this is evaluated
    # while den collects class content. See ../hosts/birdsarentreal.nix and
    # `checks.layer-isolation`.
    nixos =
      {
        scoped,
        host ? { },
        ...
      }:
      let
        # THE RADIO THIS PROFILE RIDES, from the host's own attachment.
        radio = host.attachments.house-wifi.interface or null;

        # A STATIC ADDRESS FOR THAT WIRE, if the host declares one. Any
        # attachment may name it -- a host whose cable is out has its `lan-home`
        # address on the radio -- so this looks for an attachment that names
        # THIS interface and carries an address, rather than assuming which
        # carrier it belongs to.
        staticOnRadio =
          if radio == null then
            null
          else
            lib.findFirst (a: (a.interface or null) == radio && (a.address or null) != null) null (
              lib.attrValues (host.attachments or { })
            );

        # NM addresses the radio because NM ASSOCIATES it; a competing
        # `networking.interfaces` block on the same wire would fight the lease.
        # `auto` stays the default -- a host that declares no address for this
        # wire is taking a lease, which is the honest entry.
        ipv4Config =
          if staticOnRadio == null then
            {
              method = "auto";
              route-metric = 700;
            }
          else
            {
              method = "manual";
              address1 = "${staticOnRadio.address}/16,${den.aspects.lan-home.gatewayAddress}";
              route-metric = 700;
            };
      in
      {
        networking.networkmanager.enable = true;

        networking.networkmanager.ensureProfiles = {
          # envsubst reads `$bees_psk` out of here at unit start. The generated
          # keyfile in the store holds the VARIABLE NAME; the unit runs with
          # `UMask 0177` and writes the substituted copy to
          # /run/NetworkManager/system-connections, so no passphrase reaches the
          # store or /etc.
          environmentFiles = [ scoped.house-wifi.templates."wifi.env".path ];

          profiles = lib.listToAttrs (
            map (
              ssid:
              lib.nameValuePair "house-wifi-${lib.toLower (builtins.replaceStrings [ " " ] [ "-" ] ssid)}" {
                connection = {
                  id = ssid;
                  type = "wifi";
                  # NO `interface-name`. The radio is whatever the host has --
                  # auspc's is `wlo1`, birdsarentreal's `wlp2s0` -- and pinning
                  # one here would make this aspect host-specific, which is the
                  # thing it exists not to be.
                };

                wifi = {
                  mode = "infrastructure";
                  inherit ssid;
                };

                # WPA2-PSK. The house router is WPA2-Personal (see
                # ./hostapd.nix's reasoning for why its AP stays in transition
                # mode rather than going WPA3-only), and `wpa-psk` associates
                # with a WPA3 transition BSS as well.
                wifi-security = {
                  key-mgmt = "wpa-psk";
                  psk = "\$${houseWifi.pskVar}";
                };

                # 700 so a WIRED default route always wins. This matters on
                # auspc specifically: ../hosts/auspc.nix sets a static address
                # plus `networking.defaultGateway` with `metric = null`, and
                # scripted networking's `configureGateway` then omits `metric`
                # from `ip route replace` entirely -- so that route lands at the
                # kernel default of 0. NM's own wifi default is 600, already
                # higher, but it is stated here so the two cannot end up
                # fighting if either default moves. Wifi is the route to the
                # internet when the cable is out, not instead of it.
                #
                # (auspc is on SCRIPTED networking, not networkd -- checked:
                # `networking.useNetworkd` evaluates to false there, because
                # ../network/vpn.nix only turns it on for its networkd backend.)
                ipv4 = ipv4Config;

                ipv6 = {
                  method = "auto";
                  addr-gen-mode = "stable-privacy";
                  route-metric = 700;
                };
              }
            ) houseWifi.allSsids
          );
        };
      };

    # ── THE SAME ASSOCIATION, ON DARWIN ──
    #
    # macOS has no NetworkManager and no supplicant to hand a keyfile to. The
    # one supported way to join a network non-interactively is
    # `networksetup -setairportnetwork <iface> <ssid> <passphrase>`, which also
    # stores it as a preferred network, so this runs once and roaming is the
    # system's job afterwards.
    #
    # ⚠ THE PASSPHRASE IS AN ARGV, which `ps` shows to every local user for as
    # long as the call takes. There is no file form of this command. The
    # keychain route (`security add-generic-password` plus
    # `-addpreferredwirelessnetworkatindex`) avoids argv and needs a trusted
    # application list to be usable by the system, which is why this takes the
    # documented path and says what it costs instead of hiding it.
    #
    # THE SAME SECRET, not a second copy: `secrets.wifi-password` above is the
    # one agenix rekeys for the AP and every client, so a darwin host joins
    # with the value hostapd serves.
    darwin =
      {
        scoped,
        host ? { },
        ...
      }:
      let
        # The host's own radio where it states one, as on every other class.
        # Null means "ask the machine": a Mac's Wi-Fi device is `en0` or `en1`
        # depending on the model, and `-listallhardwareports` is the only
        # answer that is right on both without a per-host literal.
        radio = host.attachments.house-wifi.interface or null;

        ssids = lib.concatMapStringsSep " " lib.escapeShellArg houseWifi.allSsids;
      in
      {
        system.activationScripts.postActivation.text = lib.mkAfter ''
          wifi_join() {
            ns=/usr/sbin/networksetup
            iface=${if radio == null then "" else lib.escapeShellArg radio}
            if [ -z "$iface" ]; then
              iface=$("$ns" -listallhardwareports | awk '/Hardware Port: Wi-Fi/{getline; print $2}')
            fi
            if [ -z "$iface" ]; then
              echo "house-wifi: no Wi-Fi device on this Mac; nothing to join" >&2
              return 0
            fi

            pass_file=${lib.escapeShellArg scoped.house-wifi.secrets.wifi-password.path}
            if [ ! -r "$pass_file" ]; then
              echo "house-wifi: $pass_file is not readable yet; leaving the radio alone" >&2
              return 0
            fi

            # ALREADY ON ONE OF OURS IS THE COMMON CASE, and re-associating
            # would drop the connection this activation may be running over.
            current=$("$ns" -getairportnetwork "$iface" 2>/dev/null | sed -n 's/^Current Wi-Fi Network: //p')
            for ssid in ${ssids}; do
              if [ "$current" = "$ssid" ]; then
                return 0
              fi
            done

            "$ns" -setairportpower "$iface" on || true
            for ssid in ${ssids}; do
              if "$ns" -setairportnetwork "$iface" "$ssid" "$(cat "$pass_file")" >/dev/null 2>&1; then
                echo "house-wifi: joined $ssid on $iface" >&2
                return 0
              fi
            done
            echo "house-wifi: none of ${ssids} answered on $iface" >&2
          }
          wifi_join
        '';
      };
  };

  # ── THE RADIO ATTACHES A HOST TO `lan-home`, AND THAT IS THE WHOLE ENTRY ──
  #
  # NOT `den.carrierAttachments.house-wifi`. There is no house-wifi segment to
  # hold a roster entry for -- see the note on the deleted `segment` giver
  # above. What there is, is a second KEY on the entity under which a host
  # states how the radio attaches it to the house lan, and this line adds that
  # key to the one segment's list.
  #
  # ITS OWN KEY AND NOT A SECOND `attachments.lan-home`, for the reason
  # ../hosts/auspc.nix gives: the NIC name differs (auspc hears the cable on
  # `enp6s0` and the radio on `wlo1`) and that box holds both at once, so one
  # key could not answer for both. Which is exactly why `attachesVia` is a LIST
  # and `interfacesOn` is plural -- the segment has two wires into this host,
  # and ../base/firewall.nix needs both.
  #
  # ORDER MATTERS AND IS DELIBERATE: `lan-home` first (./segments.nix declares
  # it as the default), so the ADDRESS still comes from the cable's entry where
  # a host has one. The radio deliberately carries no address on auspc -- it
  # takes a DHCP lease, and ../hosts/fleet.nix's `attachments` note forbids
  # writing a lease down -- so it contributes an interface and nothing else.
  den.carrierAttachments.lan-home.alsoVia = [ "house-wifi" ];
}
