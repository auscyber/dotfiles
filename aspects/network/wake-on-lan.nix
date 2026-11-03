{ den, lib, ... }:
# Wake-on-LAN. A host that states `wakeOnLan` gets magic-packet wake on that
# NIC (the firmware setting is still the BIOS's); `wake-on-lan-sender` gives
# `wake <host>` for every host that states one, broadcast on lan-home.
let
  inherit (lib) mkOption types;

  broadcastOf =
    cidr:
    let
      parts = lib.splitString "/" cidr;
      len = lib.toInt (lib.elemAt parts 1);
      octets = map lib.toInt (lib.splitString "." (lib.head parts));
      octet =
        i: o:
        let
          keep = lib.min 8 (lib.max 0 (len - 8 * i));
          host = (lib.foldl' (acc: _: acc * 2) 1 (lib.range 1 (8 - keep))) - 1;
        in
        builtins.bitOr o host;
    in
    lib.concatStringsSep "." (map toString (lib.imap0 octet octets));

  wakeable = lib.filterAttrs (_: h: h.wakeOnLan != null) (
    lib.concatMapAttrs (_: hosts: hosts) den.hosts
  );

  sender =
    pkgs:
    let
      bcast = broadcastOf den.aspects.lan-home.prefix;
      one = name: h: ''
        ${name}) exec ${lib.getExe pkgs.wakeonlan} -i ${bcast} ${h.wakeOnLan.mac} ;;
      '';
      wake = pkgs.writeShellScriptBin "wake" ''
        case "''${1:-}" in
        ${lib.concatStrings (lib.mapAttrsToList one wakeable)}
        *)
          echo "usage: wake <${lib.concatStringsSep "|" (lib.attrNames wakeable)}>" >&2
          exit 2
          ;;
        esac
      '';
    in
    [ wake ]
    ++ lib.mapAttrsToList (
      n: _: pkgs.writeShellScriptBin "wake-${n}" "exec ${lib.getExe wake} ${n}"
    ) wakeable;
in
{
  den.schema.host.options.wakeOnLan = mkOption {
    type = types.nullOr (
      types.submodule {
        options = {
          interface = mkOption { type = types.str; };
          mac = mkOption { type = types.strMatching "([0-9a-f]{2}:){5}[0-9a-f]{2}"; };
        };
      }
    );
    default = null;
    description = "The wired NIC this host can be woken through.";
  };

  den.aspects.wake-on-lan.nixos =
    { host, ... }:
    lib.mkIf (host.wakeOnLan != null) {
      networking.interfaces.${host.wakeOnLan.interface}.wakeOnLan.enable = true;
    };

  den.aspects.wake-on-lan-sender = {
    nixos = { pkgs, ... }: { environment.systemPackages = sender pkgs; };
    darwin = { pkgs, ... }: { environment.systemPackages = sender pkgs; };
  };
}
