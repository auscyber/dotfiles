{
  den,
  lib,
  capability,
  ...
}:
let
  celler = import ./_lib.nix { inherit lib; };
  inherit (den.lib.policy) pipe;

  # ── THE ADDRESS IS RESOLVED PER FLOW, NOT PER SERVER ──
  #
  # Push and pull are separate aspects because they are separate FLOWS with
  # different constraints, and that only means something if the address differs:
  #
  #   PULL   no constraint. Cloudflare's 100MB cap binds on request BODIES, so
  #          it does not bind on a response -- a pull may take the public route.
  #   PUSH   `direction = "out"`. `celler push` sends NARs far past 100MB, and
  #          ../capability-types.nix's rule on `address` turns that into the
  #          exclusion: a carrier that caps request bodies cannot take a flow
  #          that sends them.
  #
  # This is the loose end the old `celler-use` had in the same place: `pull` and
  # `push` were already split per server while `via` was ONE value shared by
  # both, so a cache reachable for pulling and not for pushing could not be
  # expressed. `cloudflared-tunnel.exclude` holding `cache.ivymect.in` is that
  # gap patched by hand, one host at a time.
  #
  # The takes are read off the aspects rather than restated, so the constraint
  # lives in ../../services/celler-service.nix next to the flow it belongs to
  # and this file cannot disagree with it.
  pullTake = den.aspects.celler-cache-pull.takes.http;
  pushTake = den.aspects.celler-cache-push.takes.http;

  hosts = lib.concatMap lib.attrValues (lib.attrValues den.hosts);

  # `endpointFor take me c` -- where `me` reaches celler server `c` for a flow
  # with `take`'s constraints. A celler server is named after its host, so this
  # is a link between two host entities, which ../capability.nix answers by
  # intersecting the carriers both ends have AND filtering by the flow.
  #
  # Was `c.endpoint`, i.e. `server.addresses.${via}` -- an address map DETECTED
  # from the serving host's evaluated config plus a `via` override per consumer.
  # §2.7 said to adopt that vocabulary and it was right about the shape; what it
  # could not do is answer per PAIR or per FLOW. The evidence was already there:
  # `consumeVia`'s ordered default picks `virtualHost` and both real callers
  # override it to `tailscale`, which is what it looks like when the answer
  # depends on who is asking.
  # ── THE LINK TO A SERVER, AS ../runtime.nix HOLDS CONFIGURATION TO IT ──
  # The candidate `endpointFor` would compose its URL from, as `{ carrier;
  # link; }`; null where nothing carries the flow.
  linkToServer =
    take: me: c:
    let
      serverHost = lib.findFirst (h: h.name == c.name) null hosts;
      ps = lib.mapNullable (h: capability.providersFor h "celler") serverHost;
      cands = lib.optionals (serverHost != null && ps != { }) (
        capability.reachEndpoint "http" take me {
          aspect = lib.head (lib.attrValues ps);
          host = serverHost;
        }
      );
    in
    lib.mapNullable (x: {
      carrier = x.via;
      inherit (x) link;
    }) (lib.findFirst (_: true) null cands);

  endpointFor =
    take: me: c:
    let
      serverHost = if me ? name then lib.findFirst (h: h.name == c.name) null hosts else null;

      # THE COMPOSED QUERY. The endpoint half -- port 8069, scheme `http://`,
      # and the `cache` subdomain where it is published -- comes from
      # `den.aspects.celler`'s own `gives.http`; the carrier half from whichever
      # carrier this host and the server share, filtered by this flow's take.
      # Neither side could answer alone: the service has no address and the wire
      # does not know the port.
      # ── THE ASPECT THAT ACTUALLY GIVES `celler` ON THAT HOST ──
      # Was `den.aspects.celler`, hardcoded -- which is secondpc's celler, on
      # port 8069. ../../hosts/celler2.nix runs a container-ised one on 8080 and
      # declares its own aspect for it, so a hardcoded giver resolves the right
      # server at the WRONG port, and the only symptom is a substituter that
      # 404s.
      cellerAspectOn =
        h:
        let
          ps = capability.providersFor h "celler";
        in
        if ps == { } then den.aspects.celler else lib.head (lib.attrValues ps);

      cands =
        if serverHost == null then
          [ ]
        else
          capability.reachEndpoint "http" take me {
            aspect = cellerAspectOn serverHost;
            host = serverHost;
          };
      l = if cands == [ ] then null else (lib.head cands).link;
      port =
        if l == null || l.ports == { } then
          null
        else
          (l.ports.default or (lib.head (lib.attrValues l.ports))).port;
    in
    if serverHost == null then
      # NO HOST TO RESOLVE FROM -- a standalone home has no `host` binding (see
      # ../../security/age-scope.nix on why). Keep the detected endpoint, which
      # is what this did before, rather than failing a scope that never had the
      # information.
      c.endpoint
    else if l == null || l.address == null || port == null then
      # NOTHING SATISFIES THE FLOW. For a pull this cannot happen (no clause),
      # so reaching here means a constrained flow has no route -- and falling
      # back to the detected endpoint is exactly the silent wrong answer the
      # constraint exists to prevent: that endpoint may be the public vhost,
      # whose 100MB cap is what excluded it in the first place.
      throw
        # THE TABLE, NOT A SUMMARY. `toPretty` of a take prints `<function>` for
        # a predicate clause, which is useless for the one case that matters, so
        # the reason comes per carrier from the judge instead.
        "celler: no carrier from ${me.name} to ${c.name} carries this flow.\n${
          capability.explainTake "http" take me (
            capability.reachEndpoint "http" { } me {
              aspect = cellerAspectOn serverHost;
              host = serverHost;
            }
          )
        }"
    else
      "${l.protocol}${l.address}:${toString port}";

  # A scope's tokens: one per server it declared on `celler-use`  # A scope's tokens: one per server it declared on `celler-use`, scoped to
  # exactly the caches it declared for that server and minted with that
  # server's own signing key (declared here as an intermediary: present for the
  # generator, never deployed). Not `host.name` for the subject: a `secrets`
  # body only sees the target config's module args, and the one thing a host
  # and a user both carry is where agenix puts their generated secrets --
  # `<host>` for a host, `<host>-<user>` for a user.
  tokenSecrets =
    {
      secrets,
      age,
      cellerHost,
      ...
    }:
    let
      dir = age.rekey.generatedSecretsDir;
      inherit (cellerHost) celler-caches celler-use;
    in
    lib.listToAttrs (
      lib.concatMap (server: [
        (lib.nameValuePair "signing_key_${server.name}" (
          celler.signingKey server.name // { intermediary = true; }
        ))
        (lib.nameValuePair (tokenOf server.name) (
          celler.tokenSecret {
            inherit dir server;
            sub = baseNameOf dir;
            signingKey = secrets."signing_key_${server.name}";
          }
        ))
      ]) (celler.use celler-caches celler-use)
    );

  tokenOf = name: "celler_token_${name}";

  # `secrets` and `templates` are ROUTED classes: their bodies become plain
  # modules in the target config and see that config's module args, never den's
  # pipe values -- see `adaptArgs` in ../../security/agenix-rekey.nix, which
  # says so and names this aspect as the thing that tripped on it. So the two
  # quirks `tokenSecrets` reads have to be handed to the target config.
  #
  # A host carries them for its users as well: a host-managed user's `secrets`
  # route into the HOST's config, so that is the only arg namespace they have.
  # The values coincide in practice -- `celler-use` is declared on the user and
  # reaches the host by `celler-use-to-host` below -- and a standalone home gets
  # its own from the homeManager definition.
  #
  # ONE arg, named `cellerHost` and not the quirks themselves. Putting
  # `celler-caches` into `_module.args` under its own name made every den class
  # body that asks for it receive it twice -- once from den's context, once
  # from the module system -- which den refuses with "class module arg
  # 'celler-caches' collides with module-system arg". A name no quirk has
  # cannot collide, and it says what this is: the snapshot taken at the host
  # scope, which is not necessarily what a user scope would have seen.
  # ── `celler-use` DERIVED FROM WHAT A HOST ASKED FOR ──
  #
  # One declaration (the take), one derivation, and ./_lib.nix's merge plus all
  # four consumers -- substituters, netrc, the push hook, the client config --
  # stay exactly as they were. Same strategy as
  # ../../services/proxy-nginx.nix translating a take into `gated.<n>`: the
  # capability becomes the front door and the existing implementation keeps its
  # job.
  #
  # Keyed by the SERVING HOST's name, because that is what `celler-use` and
  # `celler-caches` agree on -- a celler server is named after its host, and the
  # chain supplies it: a `pull` candidate's `host` is the box whose cache aspect
  # furnished it.
  #
  # NOT A QUIRK CONTRIBUTION, and that is forced. A quirk body receives a fixed
  # arg set -- `celler-caches` and `celler-use`, per `quirkArgs` below -- and no
  # `host`, which ./server.nix documents after hitting the same wall with
  # `denServices`. So this is computed at file level from the `host` a CLASS body
  # is given, and merged with whatever was declared by hand.
  # ── WHOSE TREE ANSWERS "WHO ASKED", AND FROM WHERE IT RESOLVES ──
  # Two different questions, and `takersOn` conflates them because it takes one
  # entity for both. MEMBERSHIP is the asking scope's: a user asked for the
  # cache, and `hasAspect` is projected per entity, so asking the HOST about a
  # user-scope aspect gets `false` -- which would drop the laptop's push to
  # celler2 silently, with a post-build hook that simply never fires.
  # RESOLUTION is the host's, because that is where the network is.
  usedBy =
    host: owner:
    let
      # Walks the chain from `celler`, so the serving host comes from the celler
      # level and the cache name from the level below -- which is what
      # `celler-use` needs as `{ <server>.pull = [ <cache> ]; }`.
      askers = lib.filterAttrs (_: a: (owner.hasAspect or (_: false)) a) (capability.takers "celler");

      found = lib.concatMap (
        aspect: capability.resolveChain "celler" ((capability.takesOf aspect).celler or { }) host
      ) (lib.attrValues askers);
    in
    # ONE ENTRY PER LEAF, keyed by the flow the leaf IS. `chain` carries the
    # resolved link at every level, so the server comes from the celler and the
    # cache from the level below without either being restated on the leaf.
    lib.foldl' (
      acc: c:
      let
        server = c.host.name or null;
        cache = c.chain.cache.name or null;
        flow = if c.cap == "push" then "push" else "pull";
      in
      if
        server == null
        || cache == null
        || !(lib.elem c.cap [
          "pull"
          "push"
        ])
      then
        acc
      else
        acc
        // {
          ${server} = (acc.${server} or { }) // {
            ${flow} = lib.unique ((acc.${server}.${flow} or [ ]) ++ [ cache ]);
          };
        }
    ) { } found;

  # The snapshot every celler consumer reads. `celler-use` here is the declared
  # list PLUS what this host asked for via `takes.pull`, so the derivation
  # happens once and all four consumers -- tokens, netrc, substituters, the push
  # hook -- see the same set.
  #
  # This is a CLASS body, so it gets `host`; the `secrets` and `templates`
  # bodies downstream do not, which is why the merge cannot live there.
  quirkArgs =
    {
      celler-caches,
      celler-use,
      host ? { },
      ...
    }:
    {
      _module.args.cellerHost = {
        inherit celler-caches;
        # ── NOT `user`, AND THAT IS NOT A CHOICE ──
        # Taking `user` as an arg here makes den fan this aspect out PER USER on
        # top of resolving it plainly, so the body runs twice in a user scope and
        # `_module.args.cellerHost' is defined multiple times while it's
        # expected to be unique`. An entity arg is a fan-out request, not a
        # lookup.
        #
        # So membership is the host's projected tree, which is what it was. A
        # user-scope take is visible there iff den projects the user's aspects
        # onto the host -- verified per host below rather than assumed, since
        # the failure would be a push hook that never fires.
        celler-use = celler-use ++ [ (usedBy host host) ];
      };
    };
in
{
  den.aspects.celler-quirk-args = {
    includes = [ den.policies.celler-caches ];
    os = quirkArgs;
    homeManager = quirkArgs;
  };

  # Pulls every celler server's `celler-caches` record (./server.nix) into the
  # scope that includes it.
  den.policies.celler-caches = { host, ... }: [
    (pipe.from "celler-caches" [ (pipe.collectAll ({ host, ... }: true)) ])
  ];

  # What a scope uses: `celler-use` on any of its aspects (see `use` in
  # ./_lib.nix for the shape). A user's declaration also reaches its host, so
  # declaring on the user covers both the host (substituters, the push hook)
  # and the user (its celler client config).
  den.quirks.celler-use.description = "celler servers and caches this scope pulls from and pushes to";
  den.policies.celler-use-to-host = { user, ... }: [ (pipe.from "celler-use" [ pipe.expose ]) ];
  den.schema.user.includes = [
    den.policies.celler-use-to-host
    den.aspects.celler-user
  ];

  # ── EVERY HOST PULLS, AND IT ASKS RATHER THAN NAMING ──
  #
  # Was `celler-use.secondpc.pull = [ "main" ]`: a server name and a cache name,
  # both written out, on a default every host inherits. Two strings that nothing
  # checked -- a renamed cache or a moved celler left this pointing at neither,
  # and the only symptom would be a substituter that 404s.
  #
  # The nested take reads as the chain it walks: a celler (anywhere, since
  # `den.aspects.celler` has `reach = "fleet"`), its cache `main`, and that
  # cache's pull. `pull` is not furnished by anybody -- it is derived from the
  # cache above it (../capability-types.nix), so this asks for something no
  # aspect declares. The serving host comes from resolution and `celler-use`
  # below is DERIVED from the answer.
  #
  # `main` is named rather than globbed because it is the only cache there is
  # (../../services/celler-service.nix). `gives.cache."*".gives.pull = { }`
  # would ask for every cache on every reachable celler, which is a wider
  # default than the fleet has ever had.
  #
  # A host that should not pull still excludes this aspect, exactly as before.
  den.aspects.celler-default.takes.celler.gives.cache.main.gives.pull = { };
  den.default.includes = [ den.aspects.celler-default ];

  # The host side: every declared cache as a substituter next to the fleet-wide
  # caches in ../caches.nix, a netrc with the host's token for the servers it
  # uses, and a post-build hook pushing each build to every cache it pushes to.
  den.aspects.celler-host = {
    includes = [
      den.aspects.agenix-rekey
      den.aspects.celler-input
      den.aspects.packages.celler
      den.policies.celler-caches
      den.aspects.celler-quirk-args
    ];

    # ── WHAT THIS ASPECT NEEDS TO BE TRUE ──
    #
    # A celler that does not answer is not a misconfiguration, it is a machine
    # that should come up without celler -- so this is a `requires`
    # (../../framework/requires.nix) rather than an assertion, and
    # ../../tooling/nh-checked.nix turns a failure into the `no-celler`
    # specialisation.
    #
    # THE ENDPOINT IS RESOLVED, NOT WRITTEN. `endpointFor` composes the
    # service's half (port 8069, the scheme) with whichever carrier THIS host
    # and that server share, under the pull flow's own constraints -- loopback
    # on secondpc, the tailnet from the laptop. A literal URL here would be
    # `cache.ivymect.in` for everyone, which is the public route: the one with
    # the 100MB cap, and the one that is up precisely when the direct path is
    # not. Checking it would answer the wrong question.
    #
    # `pullTake` and not `pushTake`: a pull has no body-size clause, so this
    # asks "can I reach it at all" rather than "can I reach it for the flow with
    # the tightest constraints".
    requires =
      { host, ... }:
      let
        # A chain leaf carries its SERVING HOST at `.host`, not a top-level
        # `name` -- filtering on `c.name` silently matched nothing and produced
        # no checks at all, which is the quiet failure this shape invites.
        leaves = lib.filter (c: (c.host.name or null) != null) (
          capability.resolveChain "celler" (den.aspects.celler-default.takes.celler or { }) host
        );

        serverNames = lib.unique (map (c: c.host.name) leaves);

        # `endpointFor` THROWS when no carrier carries the flow, which is right
        # for a build -- a constrained flow with no route is an error, not a
        # fallback -- and wrong here. A server this host cannot route to at all
        # is not one this check can say anything about, so it is skipped rather
        # than turned into a check that always fails for a structural reason.
        resolved = map (
          n:
          builtins.tryEval (
            endpointFor pullTake host {
              name = n;
              endpoint = null;
            }
          )
        ) serverNames;

        urls = lib.unique (map (r: r.value) (lib.filter (r: r.success && r.value != null) resolved));
      in
      lib.optionalAttrs (urls != [ ]) {
        celler-reachable = {
          description = "a celler answers at ${lib.concatStringsSep ", " urls}";
          script = lib.concatMapStringsSep "\n" (u: ''
            curl -fsS --max-time 5 ${lib.escapeShellArg "${u}/nix-cache-info"} >/dev/null
          '') urls;
        };
      };

    secrets = tokenSecrets;

    templates =
      {
        secrets,
        cellerHost,
        ...
      }:
      let
        used = celler.use cellerHost.celler-caches cellerHost.celler-use;
      in
      lib.optionalAttrs (used != [ ]) {
        netrc = {
          dependencies = lib.getAttrs (map (c: tokenOf c.name) used) secrets;
          content =
            { placeholders, ... }:
            lib.concatMapStrings (c: ''
              machine ${celler.urlHost c.endpoint}
              password ${placeholders.${tokenOf c.name}}
            '') used;
        };
      };

    os =
      {
        pkgs,
        scoped,
        cellerHost,
        host ? { },
        ...
      }:
      let
        # ONE DERIVATION, read from the shared snapshot. This re-ran `usedBy`
        # itself while claiming in its own comment to read `quirkArgs` -- so the
        # merge happened twice, and the two copies could only agree by accident.
        # `templates` above already reads the snapshot; this now matches it.
        used = celler.use cellerHost.celler-caches cellerHost.celler-use;
        pushing = builtins.filter (c: c.push != [ ]) used;
        # THE PULL FLOW: substituters are reads, so no constraint applies.
        urlsOf = c: map (cache: "${endpointFor pullTake host c}/${cache}") (builtins.attrNames c.keys);
        urls = lib.concatMap urlsOf used;
        partOf = c: "/etc/nix/parts/link-celler-${c.name}.conf";
        # A server with no cache to read from has nothing to hold.
        substituting = builtins.filter (c: urlsOf c != [ ]) used;
        token = c: scoped.celler-host.secrets.${tokenOf c.name}.path;
        # ── ONE PUSH SCRIPT PER SERVER, PRESENT WHILE IT CAN BE REACHED ──
        # nix waits for its post-build hook, so a push to a server that cannot
        # be reached holds up every build. Each server's script is a path its
        # own link holds (../runtime.nix); the hook nix runs only dispatches to
        # whichever are present.
        hookDir = "/etc/nix/parts/post-build.d";
        pushHookFor =
          c:
          pkgs.writeShellScript "celler-push-${c.name}" ''
            set -eu
            export PATH="$PATH:/nix/var/nix/profiles/default/bin:${pkgs.celler-client}/bin:${pkgs.ts}/bin"
            [ -n "''${OUT_PATHS:-}" ] || exit 0
            celler login ${c.name} ${endpointFor pushTake host c} "$(cat ${token c})"
            export TS_MAXFINISHED=1000
            export TS_SLOTS=10
            ${lib.concatMapStrings (cache: ''
              printf "%s" "$OUT_PATHS" | xargs ts celler push ${c.name}:${cache}
            '') c.push}
          '';
        build-hook = pkgs.writeShellScriptBin "build-hook" ''
          echo "Uploading paths" $OUT_PATHS
          for hook in ${hookDir}/*; do
            [ -x "$hook" ] || continue
            "$hook" || echo "build-hook: $hook failed" >&2
          done
        '';
      in
      {
        # ── EACH SERVER'S SUBSTITUTER, ONLY WHILE IT CAN BE REACHED ──
        # Trust is static: which caches may be used and their keys. Whether one
        # IS used is a nix.conf part held to its link (../runtime.nix), so an
        # unreachable cache is not consulted and builds do not stall on it.
        runtime.links = lib.listToAttrs (
          map (
            c:
            lib.nameValuePair "celler-${c.name}" {
              cap = "http";
              resolved = linkToServer pullTake host c;
              fragments."${partOf c}" = "extra-substituters = ${lib.concatStringsSep " " (urlsOf c)}";
            }
          ) substituting
          ++ map (
            c:
            lib.nameValuePair "celler-push-${c.name}" {
              cap = "http";
              resolved = linkToServer pushTake host c;
              paths."${hookDir}/${c.name}" = pushHookFor c;
            }
          ) pushing
        );
        nix.extraOptions = lib.concatMapStrings (c: "!include ${partOf c}\n") substituting;

        nix.settings = lib.mkMerge [
          {
            trusted-substituters = urls;
            trusted-public-keys = lib.concatMap (c: builtins.attrValues c.keys) used;
          }
          (lib.mkIf (used != [ ]) { netrc-file = scoped.celler-host.templates.netrc.path; })
          (lib.mkIf (pushing != [ ]) { post-build-hook = lib.getExe build-hook; })
        ];
      };
  };
  den.schema.host.includes = [ den.aspects.celler-host ];

  # The user side: a celler client config for the servers it uses, with its own
  # token.
  den.aspects.celler-user = {
    includes = [
      den.aspects.agenix-rekey
      den.aspects.celler-input
      den.aspects.packages.celler
      den.policies.celler-caches
      den.aspects.celler-quirk-args
    ];

    secrets = tokenSecrets;

    homeManager =
      {
        pkgs,
        config,
        scoped,
        celler-caches,
        celler-use,
        host ? { },
        ...
      }:
      let
        used = celler.use celler-caches celler-use;
      in
      {
        config = lib.mkIf (used != [ ]) {
          age.templates."celler_config" = {
            path = "${config.home.homeDirectory}/.config/celler/config.toml";
            # `homeManager` is a terminal class, so `scoped` is the whole tree here
            # and the scope has to be named -- see ../../security/age-scope.nix. Raw
            # rather than in the `templates` class because `path` needs the
            # enclosing home-manager `config`.
            dependencies = lib.getAttrs (map (c: tokenOf c.name) used) scoped.celler-user.secrets;
            content =
              { placeholders, ... }:
              ''
                default-server = "${(builtins.head used).name}"
              ''
              + lib.concatMapStrings (c: ''

                [servers.${c.name}]
                endpoint = "${endpointFor pushTake host c}"
                token = "${placeholders.${tokenOf c.name}}"
              '') used;
          };
          home.packages = [ pkgs.celler-client ];
        };
      };
  };

  # `nix run .#update-celler-keys [server [cache...]]` -- ask a server for each
  # cache's public signing key and refresh celler-keys.json. With no server,
  # refreshes every server/cache already in the file; with a server but no
  # caches, every cache already tracked for it. Uses the caller's own
  # `~/.config/celler/config.toml` (written by celler-push; a server's name is
  # its host's), so any host with a pull token can run it. Merges rather
  # than replaces, so naming one cache never drops the others.
  perSystem =
    { pkgs, ... }:
    let
      updateCellerKeys = pkgs.writeShellApplication {
        name = "update-celler-keys";
        runtimeInputs = [
          pkgs.celler-client
          pkgs.jq
          pkgs.gnused
          pkgs.coreutils
        ];
        text = ''
          dest="aspects/base/celler/celler-keys.json"
          if [ ! -e flake.nix ]; then
          	echo "update-celler-keys: run from the repo root (flake.nix not found)" >&2
          	exit 1
          fi

          tmp="$(mktemp)"
          trap 'rm -f "$tmp" "$tmp.next"' EXIT
          if [ -e "$dest" ]; then cp "$dest" "$tmp"; else echo '{}' >"$tmp"; fi

          refresh() {
          	server="$1"
          	shift
          	if [ "$#" -eq 0 ]; then
          		# shellcheck disable=SC2046 # cache names are never whitespace-y
          		set -- $(jq -r --arg s "$server" '.[$s] // {} | keys[]' "$tmp")
          	fi
          	if [ "$#" -eq 0 ]; then
          		echo "update-celler-keys: no caches tracked for $server; pass names explicitly" >&2
          		exit 1
          	fi
          	for cache in "$@"; do
          		# `celler cache info` reports on stderr; the key line is
          		# "           Public Key: <name>:<base64>".
          		key="$(celler cache info "$server:$cache" 2>&1 | sed -n 's/^ *Public Key: *//p')"
          		if [ -z "$key" ]; then
          			echo "update-celler-keys: no public key for $server:$cache (not found, or token lacks pull)" >&2
          			exit 1
          		fi
          		echo "$server:$cache -> $key"
          		jq --arg s "$server" --arg n "$cache" --arg k "$key" '.[$s][$n] = $k' "$tmp" >"$tmp.next"
          		mv "$tmp.next" "$tmp"
          	done
          }

          if [ "$#" -eq 0 ]; then
          	for server in $(jq -r 'keys[]' "$tmp"); do
          		refresh "$server"
          	done
          else
          	refresh "$@"
          fi

          cp "$tmp" "$dest"
          echo "Wrote $dest"
        '';
      };
    in
    {
      devshells.default.packages = [ updateCellerKeys ];
      packages.update-celler-keys = updateCellerKeys;
      apps.update-celler-keys = {
        type = "app";
        program = lib.getExe updateCellerKeys;
      };
    };
}
