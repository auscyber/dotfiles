{
  den,
  lib,
  fleet,
  ...
}:
# ── A SWITCHABLE PART OF nix.conf, WITH NO ACTIVATION AT ALL ──
#
# "can we produce like almost sub activateable and switchable composable parts
# that arent like full system closures? like different /etc/nix/nix.conf"
#
# For nix.conf specifically: yes, and the mechanism costs nothing.
# `include`/`!include` is resolved by nix when a nix PROCESS starts, not at
# activation, so a nix.conf that is a fixed store path can still name a file
# that is not. ../dev/nix-dev.nix already relies on this for its access token.
#
# Measured, with no activation and no rebuild between the two commands:
#
#   $ echo 'cores = 7' > frag; NIX_USER_CONF_FILES=conf nix config show cores
#   7
#   $ echo 'cores = 3' > frag; NIX_USER_CONF_FILES=conf nix config show cores
#   3
#   $ rm frag;               NIX_USER_CONF_FILES=conf nix config show cores
#   0
#
# The third line is why the bang matters. `!include` on a MISSING file is a
# silent no-op, so "no part selected" degrades to exactly the nix.conf the
# declaration produced -- there is no default to plant, no activation step to
# run, and no stale symlink to leave behind. A plain `include` is a hard error
# on a missing file (measured: "file ... not found"), which would turn a
# cleaned-up part into a machine that cannot run nix at all.
#
# ── WHY THE FIRST PART IS `builders` ──
#
# auspc is a build machine at TWO addresses: `builder.ipAddress =
# "10.100.0.2"` (../hosts/auspc.nix, a wireguard address) and `lanAddress =
# "192.168.1.11"` (its `lan-home` attachment). secondpc likewise, 10.100.0.1
# against 192.168.0.26. Both are correct; neither is stale.
# ../base/builders.nix has to pick one at EVALUATION time and it picks the
# tunnel -- so on the laptop, at home, every offloaded build goes over
# wireguard instead of the switch it is plugged into, and away from home the
# dead tunnel address costs a connection timeout per build.
#
# That is ../network/roaming-resolver.nix's problem exactly ("the route is not
# knowable at evaluation, which is precisely why it must not be ranked at
# evaluation"), and `builders` is the one setting where it can be answered
# without a resolver, a probe or a daemon: the whole value is a line of text
# that nix re-reads every time it starts.
#
# ── WHAT MAY BE A PART, AND WHAT MAY NOT ──
#
# The alternatives are all DECLARED, so every one of them is visible to the
# flake. What is NOT visible is which one is live -- and that is the whole
# constraint, because this tree derives firewall rules (./firewall.nix), DNS
# records and secret provisioning from declarations. A runtime choice the
# capability layer cannot see is a fact with no reader.
#
# So: a part is admissible only when every alternative produces the SAME
# derived facts. `builders` passes -- which build machine a client dials opens
# no port on the client, publishes no name, and uses the same
# `builder-ssh-key` secret whichever address it dials. A part that changed a
# bind address, a published name or a secret would NOT pass, and belongs in a
# specialisation instead. ../../docs/granular-activation.md carries that
# argument and the line it draws.
let
  # ── THE SOURCE IS `nix.buildMachines`, NOT `fleet` ──
  #
  # The first draft rebuilt the list from `fleet` the way ./builders.nix does,
  # and that is the same fact written twice -- able to disagree, and it did:
  # ../darwin/linux-builder.nix contributes a `linux-builder` entry that is in
  # `nix.buildMachines` and is in no fleet record, so an `all` alternative
  # derived from `fleet` silently DROPPED it. Rendering the evaluated option
  # instead cannot disagree with the file nix-darwin writes beside it.
  #
  # Field order and the `-` placeholders are nix's `machines` format, matched
  # to nix-darwin's own `environment.etc."nix/machines"` generator so a
  # rendered `all` is byte-comparable with /etc/nix/machines.
  machineLine =
    address: m:
    lib.concatStringsSep " " [
      "${lib.optionalString (m.protocol != null) "${m.protocol}://"}${
        lib.optionalString (m.sshUser != null) "${m.sshUser}@"
      }${address}"
      (
        if m.system != null then
          m.system
        else if m.systems != [ ] then
          lib.concatStringsSep "," m.systems
        else
          "-"
      )
      (lib.defaultTo "-" m.sshKey)
      (toString m.maxJobs)
      (toString m.speedFactor)
      (
        let
          res = m.supportedFeatures ++ m.mandatoryFeatures;
        in
        if res == [ ] then "-" else lib.concatStringsSep "," res
      )
      (if m.mandatoryFeatures == [ ] then "-" else lib.concatStringsSep "," m.mandatoryFeatures)
      (lib.defaultTo "-" m.publicHostKey)
    ];

  # Measured: several machine specs separated by `;` in one inline `builders =`
  # line are all tried.
  buildersLine = specs: "builders = " + lib.concatStringsSep " ; " specs;

  # The fleet entry a build machine entry dials, or `null`. Joined on the
  # DIALLED ADDRESS, which is what ./builders.nix put in `hostName` -- so
  # `faggot.sh` (dialled by DNS, no LAN address) and nix-darwin's own
  # `linux-builder` (not in the fleet at all) both come back null and fall
  # through untouched rather than being guessed at.
  fleetEntryFor =
    hostName:
    let
      matching = lib.filterAttrs (
        _: h: (h.builder or null) != null && (h.builder.hostName or h.builder.ipAddress or null) == hostName
      ) fleet;
    in
    if matching == { } then
      null
    else
      lib.head (lib.mapAttrsToList (name: value: { inherit name value; }) matching);

  # `lib.defaultTo` throughout and never `or`: `lanAddress`, `sshKey` and
  # `publicHostKey` are declared options with `default = null`, so the
  # attribute EXISTS and `or` would never fire -- CLAUDE.md's seven-bug trap.
  lanAddressOf =
    hostName:
    let
      e = fleetEntryFor hostName;
    in
    if e == null then null else lib.defaultTo null (e.value.lanAddress or null);

  # What to call the per-machine alternative. The fleet NAME where there is
  # one, so a human picks `only-auspc` rather than `only-10-100-0-2` -- and the
  # dialled address, sanitised into an attribute name, for anything the fleet
  # does not model.
  nameOf =
    hostName:
    let
      e = fleetEntryFor hostName;
      sanitise = lib.replaceStrings [ "." "@" "/" ":" ] [ "-" "-" "-" "-" ];
    in
    if e == null then sanitise hostName else sanitise e.name;

  alternativesFor =
    machines:
    let
      at = f: map (m: machineLine (f m) m) machines;
    in
    {
      # Every entry of `nix.buildMachines` at its declared address. Byte-equal
      # to /etc/nix/machines, so selecting it is a way back to the declared
      # configuration that does not depend on the part file being absent.
      all = buildersLine (at (m: m.hostName));

      # The same list with every one of our own boxes dialled on the home LAN
      # where it has an address there. On the laptop that moves auspc to
      # 192.168.1.11 and secondpc to 192.168.0.26 -- same boxes, same host
      # keys, same ssh key, one hop instead of a tunnel -- and leaves
      # faggot.sh and linux-builder exactly as declared.
      lan = buildersLine (at (m: lib.defaultTo m.hostName (lanAddressOf m.hostName)));

      # No remote builders. The cafe case: ./caches.nix's `fallback = true`
      # already turns an unreachable SUBSTITUTER into a miss, but an
      # unreachable BUILDER is still a connection timeout per build, and this
      # is the only lever that removes it without a rebuild.
      #
      # An empty value, measured to parse: `nix config show builders` comes
      # back empty rather than erroring.
      local = "builders =";
    }
    // lib.listToAttrs (
      map (
        m: lib.nameValuePair "only-${nameOf m.hostName}" (buildersLine [ (machineLine m.hostName m) ])
      ) machines
    );

  liveDir = "/etc/nix/parts";
  availableDir = "nix/parts-available";

  # Shared by the `nixos` and `darwin` classes, which are separate classes and
  # so cannot share a `let` of their own -- same shape as ./builders.nix's
  # `clientConfig`.
  partsConfig =
    { config, ... }:
    let
      alts = alternativesFor config.nix.buildMachines;
    in
    lib.mkIf (config.nix.buildMachines != [ ]) {
      # ONE STATIC LINE, at the end of nix.conf because `nix.extraOptions` is
      # written last (measured on this Mac: `extra-sandbox-paths` and
      # `builders-use-substitutes` sit below every `nix.settings` key). Nothing
      # else in the tree sets `builders`, so merge order among the several
      # `extraOptions` contributions does not matter here; it would if a second
      # writer appeared.
      nix.extraOptions = ''
        !include ${liveDir}/builders.conf
      '';

      # The alternatives live in the system closure, which is what keeps them
      # alive across a GC without a hand-rolled gcroot, and makes them
      # discoverable with `ls` rather than only through the flake.
      environment.etc = lib.mapAttrs' (
        name: text: lib.nameValuePair "${availableDir}/builders/${name}.conf" { inherit text; }
      ) alts;
    };
in
{
  den.aspects.nix-conf-parts = {
    description = "Makes `builders` a file nix re-reads per invocation, so the build-machine route can change without an activation.";

    nixos = partsConfig;
    darwin = partsConfig;

    os =
      { pkgs, ... }:
      {
        environment.systemPackages = [ pkgs.nix-part ];
      };

    overlays.nix-part = final: _prev: {
      nix-part = final.writeShellApplication {
        name = "nix-part";
        runtimeInputs = [ final.coreutils ];
        text = ''
          live=${liveDir}
          avail=/etc/${availableDir}

          usage() {
            cat >&2 <<'EOF'
          usage: nix-part [<part> [<alternative>|none]]

            <no args>          list every part, its alternatives and which is live
            <part>             list that part's alternatives
            <part> <alt>       make <alt> live, now, with no activation
            <part> none        drop the override; nix.conf's own value applies again

          A part is a nix.conf fragment nix re-reads when it starts, so none of
          this activates anything and none of it builds anything.
          EOF
          }

          parts() {
            [ -d "$avail" ] || return 0
            for d in "$avail"/*; do
              [ -d "$d" ] || continue
              basename "$d"
            done
          }

          # The live answer is read off the filesystem and nowhere else: that
          # is the only place it exists, which is the point.
          show() {
            local part=$1 target name f
            if [ -e "$live/$part.conf" ]; then
              target=$(readlink "$live/$part.conf" || echo "$live/$part.conf")
              name=$(basename "$target" .conf)
              printf '%s\tlive: %s\n' "$part" "$name"
            else
              printf '%s\tlive: (none, so nix.conf'"'"'s own value applies)\n' "$part"
            fi
            for f in "$avail/$part"/*.conf; do
              [ -e "$f" ] || continue
              printf '  %s\n' "$(basename "$f" .conf)"
            done
          }

          case "''${1:-}" in
            -h | --help)
              usage
              exit 0
              ;;
          esac

          if [ $# -eq 0 ]; then
            found=0
            while IFS= read -r p; do
              found=1
              show "$p"
            done < <(parts)
            [ "$found" -eq 1 ] || echo "nix-part: no parts declared on this host" >&2
            exit 0
          fi

          part=$1
          if [ ! -d "$avail/$part" ]; then
            echo "nix-part: no part named '$part'" >&2
            parts >&2
            exit 1
          fi

          if [ $# -eq 1 ]; then
            show "$part"
            exit 0
          fi

          alt=$2
          if [ "$alt" = none ]; then
            # `!include` on a missing file is a no-op, so removing the link IS
            # the way back to the declared configuration. Nothing to plant.
            sudo rm -f "$live/$part.conf"
            echo "nix-part: $part dropped; nix.conf's own value applies again" >&2
            exit 0
          fi

          if [ ! -e "$avail/$part/$alt.conf" ]; then
            echo "nix-part: '$alt' is not an alternative for '$part'" >&2
            show "$part" >&2
            exit 1
          fi

          # Replaced through a temporary name and a rename, so a nix that
          # starts mid-switch sees either the old part or the new one:
          # `ln -sfn` over an existing symlink unlinks first and leaves a
          # window in which the path does not exist.
          sudo mkdir -p "$live"
          tmp="$live/.$part.conf.$$"
          sudo ln -sfn "$avail/$part/$alt.conf" "$tmp"
          sudo mv -f "$tmp" "$live/$part.conf"
          echo "nix-part: $part is now '$alt'" >&2
          show "$part" >&2
        '';
      };
    };
  };

  # Every host that has remote builders at all -- which ./builders.nix makes
  # every host -- gets the switchable form of them. Inert until something
  # writes the live link.
  den.schema.host.includes = [ den.aspects.nix-conf-parts ];
}
