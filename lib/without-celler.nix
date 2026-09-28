# Take the celler cache out of the substituters a script's own `nix` calls use.
#
# cache.ivymect.in is the one substituter this flake both writes and trusts, and
# the only one whose being down is a local problem rather than an internet-wide
# one. When it is unreachable, nix does not shrug and move on -- a narinfo fetch
# that cannot connect is fatal, so an unrelated command dies after ~25s of
# retries with `unable to download ... Could not connect to server`. That is how
# `nix run .#secret-edit` ends up broken by a cache having nothing to do with
# editing a secret.
#
# The filter is computed from the EFFECTIVE substituter list rather than a
# hardcoded one, so aspects/base/caches.nix can gain or lose a cache without
# this going stale. Only the vhost match has to stay true.
#
# `NIX_CONFIG` is the lever because the callers here (`agenix`) spawn their own
# `nix` subprocesses that take no flags from us; it is read by every nix in the
# process tree. Ours goes LAST in the variable, since later nix.conf lines win.
#
# Set DENDRITIC_USE_CELLER=1 to opt back in for one invocation.
{ lib }: rec {
  # Every celler server's host as consumers reach it
  # (aspects/base/celler/server.nix).
  hosts = [
    "cache.ivymect.in"
    "celler2"
  ];

  # `grep -F`: each host is a fixed string, and an unescaped `.` in a regex
  # would also match `cache-ivymect-in`.
  deps = pkgs: [
    pkgs.coreutils
    pkgs.gnugrep
    pkgs.gawk
  ];

  # Leaves `celler_free_substituters` set for a caller that wants to pass
  # `--substituters` explicitly as well, and exports NIX_CONFIG for everything
  # nested.
  snippet = ''
    celler_free_substituters=""
    if [ "''${DENDRITIC_USE_CELLER:-0}" != "1" ]; then
      # `nix config show` concatenates every layer that set the key, so the
      # list arrives with duplicates; awk keeps first-occurrence order.
      celler_free_substituters="$(nix config show substituters 2>/dev/null \
        | tr ' ' '\n' \
        | grep -v '^$' \
        | grep -vF ${lib.concatMapStringsSep " " (h: "-e ${lib.escapeShellArg "://${h}"}") hosts} \
        | awk '!seen[$0]++' \
        | tr '\n' ' ')"
      celler_free_substituters="''${celler_free_substituters% }"

      if [ -n "$celler_free_substituters" ]; then
        # printf rather than a literal newline inside the string: NIX_CONFIG is
        # newline-separated, and an embedded newline here would be re-indented
        # by Nix's indented-string stripping into something nix.conf would not
        # parse. (Spelling the delimiter out would also end this string early.)
        NIX_CONFIG="$(printf '%s\nsubstituters = %s' "''${NIX_CONFIG:-}" "$celler_free_substituters")"
        export NIX_CONFIG
      fi
    fi
  '';
}
