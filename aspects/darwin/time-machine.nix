{
  den,
  lib,
  fleet,
  runtimeLib,
  ...
}:
# Time Machine on a darwin host, backing up to the Samba share
# ../hosts/secondpc/samba.nix serves.
#
# Two halves, and they differ sharply in how declarative they can be:
#
#   * the EXCLUSION of /nix is fully declarative and is the main point of this
#     file. On this machine /nix is a separate APFS volume holding 115 GiB of
#     store, every path of which is reproducible from the flake; backing it up
#     would roughly double the size of every backup to store nothing of value.
#     It currently reports `[Excluded]` with no `SkipPaths` entry in
#     /Library/Preferences/com.apple.TimeMachine.plist and no
#     `com.apple.metadata:com_apple_backup_excludeItem` xattr on /nix -- i.e.
#     only implicitly, because backupd does not add an extra APFS volume to the
#     backup set by itself. That is not a guarantee anyone wrote down, so this
#     makes it explicit.
#
#   * the DESTINATION is not, and cannot be, fully declarative. See
#     "ONE-TIME MANUAL STEP".
#
# ## Why a tailscale name and not `fleet.<server>.lanAddress`
#
# A laptop roams, and a destination URL is stored once in backupd's
# preferences -- there is no per-network variant of it. Two tailscale peers on
# the same physical LAN negotiate a DIRECT connection over that LAN (DERP is
# only the fallback for when they cannot), so one MagicDNS name gives
# LAN-speed transfer at home and still resolves from a hotspot. That removes
# the "LAN at home, tunnel away" problem instead of working around it with two
# destinations.
#
# It is also why this does not use `builder.ipAddress` (10.100.0.1), which is
# what had been registered by hand before this file existed: that is the
# wireguard address, and ../services/vban.nix spells out why a tunnel address
# is the wrong way to reach a box you may be sitting next to. For secondpc the
# wireguard hub IS the destination, so there is no extra hop -- but the
# 1420-byte tunnel MTU is still paid on every band file.
#
# The address is resolved AT RUN TIME, not at eval time. ../framework/mobile.nix
# records the reason: a tailnet name is not knowable while evaluating. The
# MagicDNS suffix is assigned by the coordination server, and tailscale appends
# a `-N` collision suffix to the requested hostname -- this machine is
# `ivys-macbook-pro-1`, not `ivys-macbook-pro`, because an older node still
# holds the plain name. So the only eval-time input is the fleet KEY, and the
# script looks the peer up by it the way ../hosts/iphone.nix resolves tailnet
# names for its web clips.
#
# ## ONE-TIME MANUAL STEP
#
# The share authenticates against kanidm as `auscyber`
# (../hosts/secondpc/samba.nix provisions the `timemachine-users` group), and
# macOS keeps a network destination's credential in the SYSTEM keychain. There
# is no way to put it there from a nix build:
#
#   * `tmutil setdestination -p` prompts on the controlling TTY, which an
#     activation script does not have;
#   * `tmutil setdestination smb://user:pass@host/share` and
#     `security add-internet-password -w <pass>` both take the plaintext as a
#     command-line ARGUMENT, i.e. in the process table of a root process --
#     not acceptable for a long-lived password, unlike the single-use
#     preauthorized key ../network/tailscale.nix accepts the same exposure for;
#   * and the password is `ivy_password`, the same intermediary
#     ../hosts/secondpc/samba.nix derives the share's NT hash from, which is
#     one of the secrets unrecoverable while the master key is being
#     recovered. Declaring it as an `age.secrets` dependency here would fail
#     the build outright rather than degrade.
#
# So, ONCE, from a terminal on this machine, after `secondpc` has joined the
# tailnet:
#
#     sudo tmutil setdestination -p "smb://auscyber@secondpc.<suffix>.ts.net/timemachine"
#
# It prompts for auscyber's kanidm password, mounts the share, writes the
# credential into /Library/Keychains/System.keychain and registers the
# destination. `<suffix>` does not have to be looked up: when the activation
# script below finds no matching destination it prints this exact line with the
# name already resolved.
#
# After that, activation is self-maintaining -- the keychain entry is keyed on
# the server, so a drifted destination (a stale URL, or the tailnet name
# gaining a `-N` suffix) is re-registered without a password.
#
# ## What is deliberately NOT used
#
# nix-darwin has no Time Machine options in the pinned revision: there is no
# `system.defaults.TimeMachine` and nothing matching /timemachine/i anywhere in
# its modules. The generic escape hatch
# `system.defaults.CustomSystemPreferences."com.apple.TimeMachine"` would write
# the plist directly, which is the wrong tool twice over -- backupd owns that
# file and recomputes parts of it, and while the file carries no SIP
# `restricted` flag (checked with `ls -lO`), writing it needs the calling
# process to hold Full Disk Access, which a shell driving `darwin-rebuild`
# generally does not have. `tmutil` asks backupd to do the write instead, so it
# is the only interface used here.
let
  # The machine serving the share, as a key into ../hosts/fleet.nix rather than
  # an address -- see above. Nothing of secondpc's own configuration is read:
  # that is the cross-host read ../base/celler/server.nix is the documented
  # exception for, and the fleet record is what exists instead. A key with
  # nothing behind it would produce a peer filter that silently matches
  # nothing, so it throws instead.
  #
  # ../services/vban.nix used to name its peer this same way and no longer
  # does: it declares `takes.udp = { name = "music-assistant"; ... }` and lets
  # ../base/capability.nix's `dial` find the machine. The same move is
  # available here the day samba's share is a household service with an aspect
  # that gives it -- a fleet key is a machine, and what this wants is a share.
  server =
    let
      name = "secondpc";
    in
    if fleet ? ${name} then
      name
    else
      throw "den.aspects.time-machine: '${name}' is not a host in aspects/hosts/fleet.nix.";

  # The share and the account, as ../hosts/secondpc/samba.nix declares them.
  share = "timemachine";
  account = "auscyber";
in
{
  den.aspects.time-machine = {
    # The peer lookup is `tailscale status`, so the CLI and the daemon have to
    # be present. Not a soft dependency: without tailscale there is no name to
    # resolve and the destination half of this is a permanent no-op.
    includes = [ den.aspects.tailscale ];

    darwin =
      {
        config,
        pkgs,
        host ? { },
        ...
      }:
      let
        setup = pkgs.writeShellApplication {
          name = "time-machine-setup";
          # writeShellApplication prepends these to PATH, so naming them is
          # what stops this depending on whatever is in root's PATH at
          # activation time. `tmutil` is deliberately an absolute system path
          # -- it is macOS, not nixpkgs.
          runtimeInputs = [
            config.services.tailscale.package
            pkgs.jq
            pkgs.gnugrep
            pkgs.gnused
            pkgs.coreutils
          ];
          text = ''
            tmutil=/usr/bin/tmutil

            # ---------------------------------------------------------------
            # 1. Exclusions. No network and no credentials, so this half always
            #    runs and is the half that must not be skipped.
            # ---------------------------------------------------------------

            # `/nix` and not `/nix/store`: the mount point covers the store,
            # /nix/var/nix/db (a sqlite database rewritten by every build) and
            # the profile gcroots, which are symlinks into a store that is not
            # being backed up anyway. Excluding a mount point excludes the
            # whole volume.
            #
            # The guard asks whether the exclusion is WRITTEN DOWN, in one of
            # the only two places it can be, and NOT whether `tmutil
            # isexcluded` currently says [Excluded]. That distinction is the
            # whole reason this exists: /nix reports [Excluded] today with
            # SkipPaths absent from the preferences and no xattr on the volume,
            # i.e. only because backupd does not add an extra APFS volume to
            # the backup set by itself. Guarding on `isexcluded` would see that
            # and never record anything -- which is exactly the state being
            # hedged against. `isexcluded` is used below only to report.
            declared() {
              if /usr/bin/plutil -extract SkipPaths json -o - \
                   /Library/Preferences/com.apple.TimeMachine.plist 2>/dev/null \
                 | jq -e 'index("/nix") != null' >/dev/null 2>&1; then
                return 0
              fi
              [ -n "$(/usr/bin/xattr -p com.apple.metadata:com_apple_backup_excludeItem /nix 2>/dev/null || true)" ]
            }

            if declared; then
              echo "time-machine: /nix exclusion is recorded"
            else
              # `-p` records a path exclusion in the Time Machine preferences,
              # which needs root AND Full Disk Access for the caller. The
              # fallback is the sticky form, which sets
              # com.apple.metadata:com_apple_backup_excludeItem on the item
              # itself and so needs neither -- at the cost of living on the
              # filesystem rather than in declared configuration. Both are
              # idempotent, so the guard above is about not writing the plist
              # on every switch, not about correctness.
              #
              # Measured, so do not count on the fallback: `tmutil addexclusion
              # /nix` returns EINVAL ("Invalid argument"), because /nix is a
              # mount point and the sticky xattr cannot be set on a volume
              # root. `-p` is effectively the only mechanism for this path, and
              # it wants Full Disk Access as well as root -- so if the warning
              # below keeps appearing after a switch, grant the terminal
              # driving darwin-rebuild Full Disk Access, or just run the
              # command it prints.
              "$tmutil" addexclusion -p /nix || "$tmutil" addexclusion /nix || true
              if declared; then
                echo "time-machine: recorded the /nix exclusion"
              else
                echo "time-machine: WARNING could not record a /nix exclusion." >&2
                echo "time-machine:   backupd currently says: $("$tmutil" isexcluded /nix 2>&1 || true)" >&2
                echo "time-machine:   which is not a guarantee -- run: sudo tmutil addexclusion -p /nix" >&2
              fi
            fi

            # ---------------------------------------------------------------
            # 2. Destination. Everything below is best-effort by construction:
            #    this is a laptop, and being away from home, on a dead tailnet
            #    or in front of a powered-off server are all ordinary states
            #    that must not fail a switch.
            # ---------------------------------------------------------------

            if ! status="$(tailscale status --json 2>/dev/null)"; then
              echo "time-machine: tailscale is not up; leaving the destination alone" >&2
              exit 0
            fi

            if [ "$(printf '%s' "$status" | jq -r '.CurrentTailnet.MagicDNSEnabled // false')" != "true" ]; then
              echo "time-machine: MagicDNS is off for this tailnet, so no name resolves" >&2
              exit 0
            fi

            # Matched on DNSName, not on HostName: HostName is whatever the
            # machine calls itself (peers in this tailnet report "localhost"
            # and "Ivy's MacBook Pro"), while DNSName's first label is the
            # `--hostname=` ../network/tailscale.nix passes to `tailscale up`,
            # plus tailscale's `-N` suffix if that name was taken. Online peers
            # sort first, so a replacement node wins over the stale record it
            # replaced.
            #
            # `$h` is a jq parameter, not a shell one; writeShellApplication
            # runs shellcheck and fails the BUILD on an info-level finding, so
            # the directive is not optional.
            # shellcheck disable=SC2016
            peers='[ (.Peer // {}) | .[] | select((.DNSName // "") | test("^" + $h + "(-[0-9]+)?\\.")) ] | (map(select(.Online == true)) + .) | (.[0] // {})'

            fqdn="$(printf '%s' "$status" | jq -r --arg h ${lib.escapeShellArg server} "$peers"' | (.DNSName // "") | rtrimstr(".")')"
            online="$(printf '%s' "$status" | jq -r --arg h ${lib.escapeShellArg server} "$peers"' | (.Online // false) | tostring')"

            if [ -z "$fqdn" ]; then
              echo "time-machine: no tailnet peer named ${server}; nothing to register." >&2
              echo "time-machine:   ${server} has to join the tailnet first -- aspects/network/tailscale.nix." >&2
              exit 0
            fi

            url="smb://${account}@$fqdn/${share}"

            # One "URL           : ..." line per registered network destination.
            current="$("$tmutil" destinationinfo 2>/dev/null | sed -n 's/^URL  *: *//p' || true)"

            if printf '%s\n' "$current" | grep -qxF "$url"; then
              echo "time-machine: destination is $url"
              exit 0
            fi

            if [ -n "$current" ]; then
              echo "time-machine: registered destination(s) are not the expected one:" >&2
              printf '%s\n' "$current" | while IFS= read -r line; do
                [ -n "$line" ] && echo "time-machine:   $line" >&2
              done
            fi

            if [ "$online" != "true" ]; then
              echo "time-machine: ${server} is offline; not touching the destination" >&2
              exit 0
            fi

            # `setdestination` without `-a`, i.e. REPLACE rather than append.
            # Appending would leave two URLs pointing at one share, and Time
            # Machine rotates between destinations -- two entries resolving to
            # the same sparsebundle is a lock fight, not redundancy. Replacing
            # keeps the existing backup history, because that lives in the
            # sparsebundle on the share and not in this list.
            #
            # No password is passed (see the header): this succeeds only when
            # the System keychain already holds the credential for this server,
            # which is what the one-time manual step puts there. stdin is
            # closed so a prompt cannot hang activation, and the mount attempt
            # is bounded because an unreachable SMB server can hold a connect
            # open for minutes.
            if timeout 90 "$tmutil" setdestination "$url" </dev/null; then
              echo "time-machine: destination set to $url"
            else
              echo "time-machine: no stored credential for $fqdn. Once, by hand:" >&2
              echo "time-machine:   sudo tmutil setdestination -p \"$url\"" >&2
              echo "time-machine:   (it prompts for ${account}'s kanidm password and stores it in the System keychain)" >&2
            fi

            exit 0
          '';
        };
      in
      {
        # Automatic backups only while secondpc's smbd answers
        # (../base/runtime.nix); Time Machine's own switch is the state.
        runtime.links.time-machine = {
          cap = "tcp";
          resolved = runtimeLib.dialFor host "tcp" { name = "samba"; };
          toggle.on = "/usr/bin/tmutil enable";
          toggle.off = "/usr/bin/tmutil disable";
        };
        # `postActivation` and not a script of its own: nix-darwin's activation
        # runner interpolates a hardcoded list of names, so a fresh
        # `system.activationScripts.<x>` type-checks and then never runs --
        # ../../extraModules/darwin/tmpfiles/default.nix has the long version.
        # `postActivation` is also the one that runs as root after users and
        # launchd, which is what touching the System keychain needs.
        #
        # This therefore runs at `darwin-rebuild switch` only, never at boot
        # (../darwin/base.nix documents that gap). Nothing here needs
        # replanting: it is all persistent state in backupd's own preferences.
        #
        # `|| true` because writeShellApplication turns on `errexit`, and a
        # backup destination must never fail a system switch.
        system.activationScripts.postActivation.text = ''
          ${lib.getExe setup} || true
        '';
      };
  };
}
