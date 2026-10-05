{ den, ... }:
# celler as capabilities. A cache is any aspect carrying a `cache` record;
# ../base/celler/server.nix finds them by inclusion, so adding one edits no list.
#
# A cache does NOT include its flows: a flow is what a consumer wants, and
# bundling them forced both onto any host that serves the cache.
#
# Only `main` exists -- ../base/celler/celler-keys.json holds `secondpc.main`
# and nothing else, and server.nix throws for a cache with no signing key.
{
  # ── THE CACHE, SEPARATE FROM THE SERVER THAT CARRIES IT ──
  # Two facts, and bundling them was wrong in a way celler2 made visible: it
  # runs its own celler on port 8080 (../hosts/celler2.nix, a container image),
  # so including an aspect that also brings `den.aspects.celler` conflicts on
  # `services.cellerd.expose.port`. A host declaring which cache it serves must
  # not thereby be told how to run a server.
  den.aspects.cache-main = {
    # `name` is the CACHE's name, not the aspect's -- `main` is what
    # ../base/celler/celler-keys.json holds and what a token is scoped to.
    # ../base/celler/server.nix reads this rather than the aspect name.
    cache = {
      name = "main";
      # `cache.ivymect.in`, readable without a credential. Not public by
      # default: the pull branches on this to decide whether it needs a netrc.
      public = true;
    };

    # ── ONLY THE CACHE IS GIVEN. THE FLOWS ARE NOT. ──
    # `pull` and `push` are parametric over the cache above them
    # (../base/capability-types.nix), so they are derived from this record
    # rather than restated here -- which is what makes a second cache arrive
    # with its flows instead of needing two more entries that could disagree
    # with it.
    gives =
      { self, ... }:
      {
        cache = _: {
          inherit (self.cache) name public;
          carrier = self.name;
        };
      };
  };

  # A host that serves cache `main` AND runs the ordinary celler for it. The
  # shape secondpc wants; celler2 takes `cache-main` alone, beside its own
  # container-ised server.
  den.aspects.celler-main.includes = [
    den.aspects.celler
    den.aspects.cache-main
  ];

  # `direction = "out"` is the whole declaration -- `address`'s rule derives the
  # rest, so a carrier that caps request bodies cannot take it.
  den.aspects.celler-cache-push = {
    reach = "fleet";
    takes.http.direction = "out";
  };

  # No clause: the cap does not bind on responses, so a pull may take a route
  # push may not.
  den.aspects.celler-cache-pull = {
    reach = "fleet";
    takes.http = { };
  };
}
