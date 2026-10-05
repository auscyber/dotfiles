{
  den,
  lib,
  carrierAttachment,
  ...
}:
# netboot.xyz over PXE/iPXE on the house LAN.
#
# ── THE ROUTER DOES DHCP AND THIS FLAKE CANNOT TOUCH IT ──
#
# No DHCP server is declared anywhere here and no pool is recorded
# (../hosts/birdsarentreal.nix carries the warning about exactly that), so
# `next-server`/`filename` are not available to set. They are not needed:
# dnsmasq runs in PROXY-DHCP mode, which allocates nothing and answers only
# DHCP packets carrying `PXEClient` in the vendor-class field. The router keeps
# handing out leases; this box only tells a PXE client where its boot file is.
# Nothing has to be configured on the router for this to work.
#
# `dnsmasq` is a host singleton, so a second aspect wanting one on the same
# machine has to merge with this rather than declare its own.
let
  # The real entity rather than the class body's projection --
  # ../base/firewall.nix records why that distinction cost every `expose` in
  # the tree.
  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;

  # The house lan's network address, read off the segment that owns
  # `192.168.0.0/16` (../network/segments.nix). The netmask is not repeated:
  # dnsmasq derives it from the directly-connected interface.
  lanNetwork = lib.head (lib.splitString "/" den.aspects.lan-home.prefix);

  # TFTP, proxy-DHCP, and PXE's boot-server port. Named once so `gives.udp`
  # and the dnsmasq config cannot disagree.
  sockets = {
    tftp = 69;
    dhcp = 67;
    pxe = 4011;
  };

  socketDescriptions = {
    tftp = "TFTP: hands the iPXE binary and the menu to a client that has just been told where to find them.";
    dhcp = "Proxy-DHCP: answers only PXEClient packets, so the router keeps handing out the leases.";
    pxe = "PXE's boot-server port, where a client that already holds a lease asks this box directly for its boot file.";
  };
in
{
  den.aspects.netbootxyz = {
    description = "Boots the netboot.xyz menu over PXE on the house LAN, without touching the router's DHCP.";

    # Inclusion IS the attachment (../network/segments.nix): a host serving
    # this is on the house lan by construction, which is the one wire every
    # port below is exposed on.
    includes = [ den.aspects.lan-home ];

    # NO `gives.service`, so this is not in the household's `services`. That
    # set is the GRANTABLE surface -- ../services/access.nix mints a kanidm
    # group per entry and ../services/homepage.nix draws a tile -- and a NIC
    # asking for a boot file has no identity for a grant to attach to.
    # ../network/hostapd.nix is the precedent: house-wide network plumbing on
    # the includes list and not a household service.

    gives =
      { self, ... }:
      {
        # ../base/firewall.nix opens each of these on the interface `lan-home`
        # furnishes (`br0` here), which is why there is no
        # `networking.firewall.allowedUDPPorts` -- that is nixpkgs'
        # pseudo-interface `default` and would open DHCP and TFTP on the
        # tailnet and the wireguard hub as well.
        #
        # `udp` carries `expose` for the same reason `tcp` does: both include
        # `endpoint` (../base/capability-types.nix), which is where `ports` and
        # their per-port `expose` are declared.
        udp = _: {
          name = self.name;
          ports = lib.mapAttrs (n: port: {
            inherit port;
            meta.description = socketDescriptions.${n};
            expose = [ "lan-home" ];
          }) sockets;
        };
      };

    nixos =
      {
        pkgs,
        host ? { },
        ...
      }:
      let
        me = hostsOf.${host.name or ""} or host;

        # Every wire this host hears the house lan on, not just the first:
        # ../network/carrier-attachment.nix's `interfaces` is the plural
        # because a host can be on one segment over several NICs.
        wires = (carrierAttachment.half "lan-home" me).interfaces;

        # Both halves of ONE netboot.xyz release. nixpkgs packages the UEFI
        # binary and has none for the BIOS ROM, so the ROM is fetched at the
        # version nixpkgs pins -- a nixpkgs bump then breaks this hash loudly
        # instead of serving two different releases to the two architectures.
        bios = pkgs.fetchurl {
          url = "https://github.com/netbootxyz/netboot.xyz/releases/download/${pkgs.netbootxyz-efi.version}/netboot.xyz.kpxe";
          hash = "sha256-eAhpX9u2crNhQXsj3zAYKtYXHUxKAUf0ycqq83FQ3gE=";
        };

        # A store path, so there is no /srv/tftp to populate by hand and
        # nothing on disk that can drift from what this file says.
        tftpRoot = pkgs.runCommandLocal "netbootxyz-tftp-root" { } ''
          mkdir -p $out
          install -m444 ${pkgs.netbootxyz-efi} $out/netboot.xyz.efi
          install -m444 ${bios} $out/netboot.xyz.kpxe
        '';
      in
      {
        # An empty list means no `interface=` line, and dnsmasq with no
        # `interface=` answers PXE on EVERY wire -- the silent-and-wrong
        # failure rather than a missing one.
        assertions = [
          {
            assertion = wires != [ ];
            message = "netbootxyz: ${me.name or "this host"} has no `lan-home` interface, so dnsmasq would answer PXE on every wire it has. Declare `attachments.lan-home.interface` on the host.";
          }
        ];

        services.dnsmasq = {
          enable = true;

          # ── NOT A RESOLVER ──
          #
          # This defaults to TRUE and writes `networking.nameservers =
          # [ "127.0.0.1" ]` plus `resolvconf.useLocalResolver`, which would
          # take this box's DNS off systemd-resolved. With it false and
          # `port = 0` below there is no DNS socket at all, so nothing
          # contends with resolved on 53 and
          # ../network/roaming-resolver.nix's choice of unbound over dnsmasq
          # is untouched.
          resolveLocalQueries = false;

          settings = {
            port = 0;

            interface = wires;
            bind-interfaces = true;

            # PROXY-DHCP: no pool, no leases, no addresses handed out. Note
            # there is deliberately NO `dhcp-authoritative` and no second
            # address in the range -- either would make this a real DHCP
            # server on a segment that already has one.
            dhcp-range = [ "${lanNetwork},proxy" ];

            enable-tftp = lib.concatStringsSep "," wires;
            tftp-root = "${tftpRoot}";

            # ONE ENTRY PER CLIENT ARCHITECTURE, and one per arch is what
            # makes dnsmasq boot it directly instead of drawing a prompt. The
            # basename contains a `.`, which is how dnsmasq decides to use it
            # verbatim rather than appending PXE's `.0` layer suffix
            # (`rfc2131.c`'s `strchr(basename, '.')`).
            #
            # No `ARM64_EFI`: nixpkgs' `netbootxyz-efi` is per-platform and
            # this host is x86_64, so there is no arm64 binary here to serve.
            pxe-service = [
              ''x86PC,"netboot.xyz (BIOS)",netboot.xyz.kpxe''
              ''x86-64_EFI,"netboot.xyz (UEFI)",netboot.xyz.efi''
            ];
          };
        };
      };
  };
}
