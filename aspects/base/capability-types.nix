{ lib, config, ... }:
# ── CAPABILITIES COMPOSE BY `includes`, THE SAME VERB ASPECTS USE ──
# `den.capabilities.<name>` -- WHAT A CAPABILITY IS, TYPED, AND COMPOSED.
# This is what `den.serviceTypes` was reaching for, done correctly: a capability
let
  inherit (lib) mkOption types;

  # The registry, read while defining it -- which is exactly how an aspect's
  # `includes` reaches `den.aspects.<other>`. Safe because a narrowing never
  caps = config.den.capabilities;

  # ── A CAPABILITY MAY BE PARAMETRIC OVER THE CHAIN IT HANGS UNDER ──
  # `den.capabilities.push = { cache, celler, ... }: { ... }`. A nested
  # capability is not furnished by anyone: there is one `pull` per cache and its
  # every field follows from the cache and the celler above it. So the
  # definition is a function of its ancestors, keyed by capability name, exactly
  # as a parametric aspect is a function of its scope.
  #
  # The args it destructures become DECLARED options -- read off
  # `builtins.functionArgs`, which is how the module system decides what to pass
  # a module -- so `inherit cache celler` is typed rather than freeform, and a
  # chain member the definition never asked for is still an unknown option.
  #
  # Applied with nulls here to give the generic, chain-free view: the
  # declarations (`options`, `includes`, `gives`, `rules`) do not depend on the
  # chain, which is what lets `includes = [ caps.tcp ]` keep referencing by
  # value. ./capability.nix re-applies `parametric` with the resolved chain.
  parametrise =
    f:
    let
      params = lib.attrNames (lib.functionArgs f);
      body = f (lib.genAttrs params (_: null));
    in
    # `inherit cache celler` is written at the TOP LEVEL, where it reads, and
    # lands under `chain`, where it is typed. Moved rather than declared in
    # place because `types.submodule` sets `shorthandOnlyDefinesConfig`, so a
    # definition cannot declare options of its own -- and every top-level key
    # that is NOT one of the destructured args still has to be a real option,
    # which is the typo-safety this would otherwise have cost.
    (removeAttrs body params)
    // {
      chain = lib.intersectAttrs (lib.functionArgs f) body;
      parametric = f;
    };

  # ── A TAKE IS RECURSIVE, BECAUSE `gives` IS ──
  # A sub-take is a take: it filters on characteristics, names an instance, and
  # may itself ask for what that level exposes. Typed recursively rather than as
  # `attrsOf raw`, which is what left a sub-take without the declared clauses
  # (`within`, `carrier`) that `judge` reads off every take.
  takeType = types.submodule {

    # FREEFORM, deliberately, and this is the one place it is right: the
    # characteristic clauses are keys of whatever CAPABILITY is being
    freeformType = types.lazyAttrsOf types.raw;

    options = {
      # Which instance. Filters this level by the `name` its giver
      # furnishes, so `name = "secondpc"` picks that celler.
      name = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Instance to resolve to, by the name its giver furnishes.";
      };

      # Nested requests, mirroring `gives` on the capability:
      #
      #   takes.celler = {
      #     name = "secondpc";
      #     gives.cache.main.gives.pull = { };
      #   };
      #
      # `gives.<cap>.<instance>` is a sub-take against what this level
      # exposes, keyed by the instance's name -- so the name is the key
      # rather than a clause, and the structure matches the chain it
      # walks. Recursive: a sub-take carries its own `gives`.
      gives = mkOption {
        type = types.attrsOf (types.attrsOf takeType);
        default = { };
        description = ''
          Sub-takes against capabilities this one exposes, keyed by capability
          then instance. The key `*` means every instance, and an empty
          instance set is sugar for it -- which is what makes `gives.pull = { }`
          read as "the pull of this cache", since a cache has exactly one. Use
          `*` where every instance also has a sub-take to carry:
          `gives.cache."*".gives.pull = { }`.
        '';
      };

      carrier = mkOption {
        type = types.nullOr (types.either types.str (types.listOf types.str));
        default = null;
        description = ''
          Restrict to these carriers by name. An EDGE property rather
          than a characteristic of the link, which is why it is declared
          and not freeform -- and an escape hatch, since naming a
          carrier is exactly what the capability layer exists to avoid.
          ../hosts/laptop.nix's `celler-use.<server>.via` is the
          standing example of a caller that genuinely needs it.
        '';
      };

      # ── THE ENVIRONMENT, FROM THE CONSUMER'S SIDE ──
      # "in this environment is reach type of deal" -- yes, and `reach`
      # alone is only half of it. `reach` is the GIVER's willingness to
      within = mkOption {
        type = types.nullOr (
          types.enum [
            "host"
            "household"
            "fleet"
          ]
        );
        default = null;
        description = ''
          Furthest environment this take will resolve into. The
          consumer's half of `reach`.
        '';
      };

      sameHost = mkOption {
        type = types.nullOr types.bool;
        default = null;
        description = ''
          Require (or refuse) a provider on the asking host. Null means
          no opinion, which is the usual case -- preference order
          already puts loopback first where it is usable.
        '';
      };

      where = mkOption {
        type = types.nullOr (types.functionTo types.bool);
        default = null;
        description = ''
          The escape hatch: a predicate over the whole candidate
          (`{ carrier, via, link, sameHost }`) for the case no clause
          shape anticipated. Here because a closed filter language
          would force a fork of this mechanism the first time something
          needs a shape nobody thought of.
        '';
      };
    };

  };
in
{
  options.den.capabilities = mkOption {
    type = types.attrsOf (
      types.coercedTo (types.functionTo types.raw) parametrise (
        types.submodule (
          { config, name, ... }:
          {
            # AN ATTRSET OF OPTION DECLARATIONS, which is the shape a module's
            # own `options` has -- so the container is checked (two capabilities
            options.options = mkOption {
              type = types.lazyAttrsOf types.raw;
              default = { };
              description = "The options a giver of this capability must furnish, beyond those it includes.";
            };

            # ── WHICH HALF OF A LINK THIS CAPABILITY FURNISHES ──
            # A link is composed from two sides that know different things, so the
            # type system has to know which side each option belongs to:
            options.side = mkOption {
              type = types.enum [
                "carrier"
                "endpoint"
                "either"
              ];
              default = "either";
              description = "Which half of a composed link this capability's own options belong to.";
            };

            # ── RULES THE CAPABILITY ITSELF IMPOSES ──
            # A take's clauses are the CONSUMER's requirements. These are the
            # capability's own: facts that follow from what the two halves are,
            options.rules = mkOption {
              type = types.listOf (types.functionTo (types.nullOr types.str));
              default = [ ];
              description = "Constraints the capability imposes on a composed link, independent of any take.";
            };

            options.realise = mkOption {
              type = types.nullOr (types.functionTo types.raw);
              default = null;
              description = ''
                How this capability is REALISED from an edge, where its furnished
                fields are not the whole answer. Handed
                `{ giver, taker, resolve, ... }` -- the two ends plus a resolver
                for nested capabilities -- so a capability can ask for the
                narrower ones it is built out of without ./capability.nix knowing
                which.

                This is where a capability's own semantics live. The alternative
                was a per-capability function in the resolver, which makes the
                generic layer the union of every capability's special case.
              '';
            };

            # ── IS IT ACTUALLY THERE, RIGHT NOW ──
            #
            # Resolution answers "where would I dial this", which is an EVAL
            # time fact and the only one Nix can have. Whether anything answers
            # on that address is a runtime fact, and it is the one that decides
            # whether a part of the configuration should be live.
            #
            # DECLARED PER CAPABILITY, NOT PER SERVICE, which is the whole
            # point: "can I reach celler" is not a celler question. It is the
            # `http` question asked of the link celler resolved to, and every
            # other taker of `http` wants the identical test. A probe written
            # per service would be one bespoke script per aspect and would drift
            # from what the aspect actually resolves to.
            #
            # FOLLOWS THE NARROWING CLOSURE like everything else here, so
            # `http` inherits `tcp`'s connect test unless it says something
            # better, and a capability nobody has written a probe for has none
            # rather than a wrong one.
            #
            # ⚠ THE PROBE MUST TEST THE SERVICE, NOT THE BOX. Measured on
            # faggot.sh while this was being written: ICMP answered 2/2 at
            # 76ms and TCP 22 was unreachable -- a host-level ping check would
            # have reported that host healthy while every dial to it failed.
            options.probe = mkOption {
              type = types.nullOr (types.functionTo types.str);
              default = null;
              example = lib.literalExpression ''{ link, ... }: "nc -z -w3 ''${link.address} ''${toString link.port}"'';
              description = ''
                `{ link, ... }: "<shell command>"` -- a command that exits 0 when
                this capability is reachable over the resolved link it is handed,
                and non-zero when it is not.

                PROBES COMPOSE, AND THEY ADD. A probe is about a LINK between
                two ends, so the test for "can I reach this service" is the
                whole chain of them: the carrier's probe (does this wire carry
                anything), then the carrier's probe for that peer (does THAT
                host answer on it), then this one (does the service answer).
                `./capability.nix`'s `probesFor` is what collects them, parent
                first, so a failing segment is reported as a failing segment and
                nothing beneath it is dialled at all.

                The link is the resolved one (`capability.dial`'s answer), so the
                probe reads `address`, `port`, `ports` and the carrier rather
                than knowing anything about the service. Whoever RUNS it supplies
                the binaries; this says what to run, not where it comes from.
              '';
            };

            # ── WHAT A TAKER MAY CONTRIBUTE, as against what it filters on ──
            # A take has two halves and conflating them is a silent failure. A
            # FILTER clause names a key the giver furnishes (`over = "unix"`) and
            #
            # A REAL MODULE, not a description of one. This was
            # `takerOptions.<n> = { type; default; merge; }` -- an attrset
            # describing an option, fed to a throwaway `evalModules` per
            # (aspect, cap). `merge` was a field where a type belongs, and the
            # declarations did not inherit through `includes` at all, so the
            # resolver carried a hand-rolled closure fold to make an inherited
            # declaration reachable. Both are gone: `taker.options.<n>` is
            # `lib.mkOption`, and inheritance is `imports`.
            options.taker = mkOption {
              type = types.deferredModule;
              default = { };
              description = ''
                A module declaring what a TAKER may contribute to a giver of this
                capability -- `taker.options.<n> = lib.mkOption { ... }`, merged
                by the module system like any other option set. The PROVIDER owns
                this vocabulary (`extensions` means something to postgres and is
                an unknown key against a tailnet), which is what keeps the
                contribution surface honest without a central registry of
                what-can-contribute-what.

                Split it across files with `taker.imports` and make a
                contribution mandatory by omitting its `default`.
              '';
            };

            # THE CLOSURE, AS ONE MODULE. A giver of a narrowing accepts
            # everything the capability it narrows accepts, and `imports` is
            # how an option set inherits -- transitively, with defaults
            # materialised, which is exactly what the fold this replaces was
            # imitating.
            options.takerModule = mkOption {
              type = types.deferredModule;
              internal = true;
              default = {
                # ONE KEY PER CAPABILITY, because the closure is a DAG and not a
                # tree: `tcp` includes both `address` and `endpoint`, and both
                # include `carried`, so `carried`'s declarations arrive twice.
                # A keyed module is imported once, which is the module system's
                # own answer to a diamond -- without it the second copy is
                # `The option `listensOn' is already declared`.
                key = "den.capabilities.${name}.takerModule";
                imports = [ config.taker ] ++ map (c: c.takerModule) config.includes;
              };
              description = "This capability's `taker` plus that of everything it narrows. Read by the resolver; not set by hand.";
            };

            # ── WHICH CONTRIBUTIONS THE GIVER READS AS ONE VALUE ──
            # Not an option type and not a merge discipline: it is the shape of
            # the COLLECTION. `capability.contributions` keys a contribution by
            # taker, because "who asked for this" is the thing a provider
            # provisions against; a key named here is reduced across takers
            # instead, by its own option type -- `listOf` concatenates, and a
            # type that cannot merge two definitions reports the conflict with
            # both defining files named.
            options.takerMerged = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Taker options a giver reads as ONE merged value rather than one entry per taker.";
            };

            # REFERENCES, NOT STRINGS, exactly as an aspect's `includes` holds
            # `den.aspects.gateway` rather than `"gateway"`. A string is a name
            options.includes = mkOption {
              type = types.listOf types.raw;
              default = [ ];
              description = ''
                Capabilities this one is a NARROWING of, by reference --
                `includes = [ caps.tcp ]`. Giving `tcp` is giving an `address`, so
                anything that can read an address can read a tcp link; and `unix`,
                which narrows nothing routed, is thereby unable to satisfy a want
                for one.
              '';
            };

            # ── `takes`: USES-A, WHERE `includes` IS IS-A ──
            # These were one field and conflating them was wrong in both
            # directions.
            options.takes = mkOption {
              type = types.listOf types.raw;
              default = [ ];
              description = ''
                Capabilities this one is COMPOSED OF, by reference. Their options
                join this capability's surface -- a giver furnishes them and
                `realise` may read them -- but giving this does NOT satisfy a want
                for them, which is what `includes` is for.
              '';
            };

            # ── NO `gives` ON A CAPABILITY: THE CHILD DECLARES THE EDGE ──
            # There was a `gives` here, listing the capabilities this one exposes,
            # and the parametric form above subsumes it. `push = { cache, celler,
            # ... }:` already says a push hangs beneath a cache beneath a celler,
            # so a `cache.gives = [ caps.push ]` beside it is the same edge
            # declared twice and able to disagree.
            #
            # Declaring it from the child is also the direction `includes` runs: a
            # narrowing names what it narrows. ./capability.nix derives the
            # exposure relation by inverting the parameters, and the direct parent
            # is the one that itself requires all the others -- `push` takes
            # `cache` and `celler`, `cache` takes `celler`, so `cache` is the
            # parent and `celler` the grandparent.

            # ── WHAT MAY BE CLAIMED ONLY ONCE IN A SCOPE ──
            # "port conflicts is more of something that a capability defines as it
            # being only declared onceable in a scope." Exclusivity is a property
            # of what the thing IS, not a check somebody remembers to write: a tcp
            # port is one socket per host, a published subdomain is one service per
            # zone, a unix account is one name per host. Each is the same shape and
            # none of them can be merged -- the second claimant does not get half a
            # port.
            #
            # Declared here so the generic layer enforces every one of them, rather
            # than there being one hand-written walk per kind. A contribution
            # the giver reads as ONE value (`takerMerged`) is the mirror of this
            # on the TAKER side: its own type refuses two consumers disagreeing
            # about a server-global setting, and
            # says nothing about two providers claiming one resource.
            options.exclusive = mkOption {
              type = types.attrsOf (
                types.submodule {
                  options = {
                    scope = mkOption {
                      type = types.enum [
                        "host"
                        "household"
                        "fleet"
                      ];
                      default = "host";
                      description = "How far the claim has to be unique. A socket is per host; a published name is per zone, so fleet.";
                    };

                    side = mkOption {
                      type = types.enum [
                        "endpoint"
                        "carrier"
                      ];
                      default = "endpoint";
                      description = "Which half of the link carries the claim.";
                    };

                    claims = mkOption {
                      type = types.functionTo (types.functionTo (types.listOf types.raw));
                      description = ''
                        `half: ctx: [ claim ]` -- the claims one furnished half
                        makes. Several, because an endpoint has several ports.

                        `ctx` is what the giver's own fields cannot say:
                        `{ host; cap; bindAt; }`, where `bindAt <portName>` is the
                        address this capability's consumers actually resolve to
                        on that host. A port's bind falls back to it, which is
                        how "if loopback only wants it, then only loopback
                        demand" reaches a rule with no resolver of its own.
                      '';
                    };

                    key = mkOption {
                      type = types.functionTo types.str;
                      description = "What identifies a claim. Two claims with one key are the same resource.";
                    };

                    # ── WHEN TWO CLAIMS ON ONE KEY ARE ONE CLAIM ──
                    # `a: b: bool`. Some resources are held by every
                    # participant at once rather than by one holder -- a
                    # multicast group is the case this exists for -- and for
                    # those, two claims on one key are not two claimants
                    # fighting but one shared thing declared twice. They MERGE:
                    # no finding, and the union of what each said about it.
                    #
                    # Null means every key is held exclusively, which is the
                    # right default for a socket, a subdomain, a lock.
                    shares = mkOption {
                      type = types.nullOr types.raw;
                      default = null;
                      description = "`a: b: bool` -- these two claims on one key are one shared resource, so they merge instead of colliding.";
                    };

                    overlaps = mkOption {
                      type = types.nullOr types.raw;
                      default = null;
                      description = ''
                        `a: b: bool`, narrowing equal keys further -- two claims on
                        one port number are not the same socket if they bind
                        different addresses. Null means an equal key is enough.
                      '';
                    };

                    describe = mkOption {
                      type = types.functionTo types.str;
                      description = "One claim, for the message. Two of these and the resource is the whole diagnostic.";
                    };
                  };
                }
              );
              default = { };
              description = "Resources a giver of this capability claims, which no second giver may claim in the same scope.";
            };

            # ── WHAT A DERIVED CAPABILITY IS, where no aspect furnishes it ──
            # There is one pull per cache and every field of it follows from the
            # cache and the celler above: nobody declares it, so nobody can get it
            # wrong, and a second cache gets its flows by existing.
            options.furnishes = mkOption {
              type = types.nullOr types.raw;
              default = null;
              description = ''
                The link this capability furnishes from its chain alone, for a
                capability no aspect gives. Written by the parametric form, where
                the ancestors are in scope. Null means an aspect's `gives` is the
                only source, which is the ordinary case.
              '';
            };

            # The attribute key, so a reference carries its own name and the
            # closure can walk `includes` without being handed the registry. den's
            options.name = mkOption {
              type = types.str;
              default = name;
              description = "Capability name. Defaults to the attribute key.";
            };

            # ── ROUTED, OR A CONTRIBUTION ──
            #
            # Most capabilities are ROUTED: a take resolves to a giver and
            # produces a link with an address, a port, a carrier. `tcp`, `udp`,
            # `celler`, `mqtt`, `http`.
            #
            # Some are not, and the difference is not cosmetic. `share`,
            # `samba-user` and `home-assistant` have NO giver anywhere and never
            # will: the take is a marker that something is wanted, and a
            # collector reads it with `takersOn`/`contributions` -- never with
            # `dial`. Every config contribution is this shape.
            #
            # Without this flag they are indistinguishable from a broken route.
            # Measured, on the samba-user migration: ../base/service-reach.nix's
            # `take-routes` check reported "`samba-user` takes `samba-user` and
            # nothing gives it within reach of any host ... No host in the fleet
            # gives `samba-user` at all", which is true and not a fault. The
            # capability graph has the same blind spot and draws them as
            # unresolved.
            # ── NO `routed` OPTION: IT IS DERIVED ──
            #
            # This was a declared `bool` with `default = true`, and it was a
            # SECOND COPY of a fact the closure already carries:
            # ./capability.nix's `isRouted` is `elem "address" (capClosure cap)`
            # -- does this narrow an address. The two disagreed on 21 of 30
            # capabilities (measured), and different readers took different
            # sides: ../base/service-reach.nix read the option, ../docs/
            # capability-graph.nix read `isRouted`. So a take of `gated` was a
            # routed site to one and a contribution to the other.
            #
            # A CONTRIBUTION IS STILL A CATEGORY ERROR, and deleting the flag
            # does not fix that -- it only stops the error having two answers.
            # `samba-user` and `share` ARE data flowing from many takers to one
            # collector, which is a QUIRK (den's own word for a named data
            # route), and the whole taker/`contributions` mechanism is
            # the same mistake at scale: a parallel delivery system with its own
            # fold and its own merge modes, where `den.lib.policy.deliver`
            # already takes `{ from; to; at; mode = merge|nest|verbatim;
            # guard; }`. The migration list is now `isRouted cap == false`
            # rather than a grep for a hand-written flag.

            options.meta = mkOption {
              type = types.submodule {
                options.description = mkOption {
                  type = types.str;
                  default = "";
                };
              };
              default = { };
              description = "Attached metadata, as on an aspect.";
            };

            # The transitive option set, which is what a giver is actually checked
            # against. Computed here rather than at each call site so a capability
            # cannot be read with a partial view of what it requires.
            # ── THE ANCESTORS, AS RESOLVED ──
            # Null-valued in the registry's chain-free view and filled in by
            # ./capability.nix's `bind`. Written by the parametric form as
            # top-level `inherit cache celler`, which `parametrise` moves here.
            options.chain = mkOption {
              type = types.lazyAttrsOf types.raw;
              default = { };
              description = "This capability's ancestors, keyed by capability name, where it has been resolved beneath them.";
            };

            # The definition as written, kept so a resolution can re-apply it with
            # the real chain. Null for the ordinary, non-parametric form.
            options.parametric = mkOption {
              type = types.nullOr types.raw;
              default = null;
              internal = true;
              description = "The parametric definition, re-applied by ./capability.nix with the resolved chain.";
            };

            options.allOptions = mkOption {
              type = types.raw;
              readOnly = true;
              internal = true;
              default = config.options;
              description = "Own options merged with those of everything included. Filled by the closure below.";
            };
          }
        )
      )
    );
    default = { };
    description = ''
      Capability DEFINITIONS -- the shape a giver must furnish for each one.

      This is the correct form of what `den.serviceTypes` was approximating. A
      capability has no identity and no instances: it is a contract between a
      giver and a consumer, and the only thing it needs is a type and what it
      is a narrowing of.
    '';
  };

  # ── THE ASPECT FIELDS, AS A MODULE ──
  # `den.schema.aspect` is imported into EVERY aspect submodule
  # (den's `nix/lib/aspects/types.nix:895`), which makes it den's own way to add
  config.den.schema.aspect = {
    options = {
      gives = mkOption {
        type = types.nullOr (types.functionTo (types.lazyAttrsOf types.raw));
        default = null;
        description = ''
          What this aspect FURNISHES, keyed by capability. Takes the scope
          context as an attrset -- `{ self, host, ... }` -- and each capability
          is itself a function of the peer being addressed. See
          ./capability.nix for why there are two levels and two different hosts.

          A function rather than data because only the giver can supply the
          capability: a tailnet addresses its members by MagicDNS name, a lan by
          a declared address, a loopback by a constant, and a consumer asking
          for `tcp` must not have to know which.
        '';
      };

      takes = mkOption {
        type = types.attrsOf takeType;
        default = { };
        description = ''
          What this aspect ASKS FOR, keyed by capability, with a filter over
          what it will accept. "Ask, don't announce": the consumer says which
          links are acceptable and the giver says what it is -- nothing is
          negotiated, and nothing is named.
        '';
      };

      # ── THE FACTS A CARRIER CARRIES ABOUT ITSELF ──
      # `prefix` and `domain` were already on ../network/segments.nix and
      # ./tailscale.nix as untyped freeform keys, which meant a misspelled
      prefix = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          The segment's CIDR, for a carrier that is a wire several hosts attach
          to. Its presence is what distinguishes a segment from a loopback.
        '';
      };

      # ── HOW FAR THE WIRE ITSELF GOES ──
      #
      # A LOOPBACK IS A SEGMENT WITH ONE MEMBER, which is the thing the tree
      # used to say by leaving `prefix` out and calling that absence
      # load-bearing. It read as an omission, two files hedged about it, and
      # three predicates were written as "not a provider and gives no
      # `segment`" -- a double negative standing in for a fact nobody had
      # declared.
      #
      # So it is declared. A lan or a tunnel is ONE network that several hosts
      # are each a part of; a loopback and a container bridge are a whole
      # network that stops at one box. Both are segments, and `spans` is the
      # difference -- which is also exactly the question a cross-host link has
      # to ask, so it is asked positively now.
      #
      # NULL for anything that is not a carrier, which is most of this tree.
      spans = mkOption {
        type = types.nullOr (
          types.enum [
            "one-host"
            "one-machine"
            "many-hosts"
          ]
        );
        default = null;
        description = "How far this carrier reaches: one entity, every entity on one machine, or across machines.";
      };

      # ── DOES THIS WIRE CARRY ANYTHING, RIGHT NOW ──
      #
      # The segment half of the same question `den.capabilities.<c>.probe`
      # asks of a service. A probe is about a LINK, so this one is handed the
      # link to a particular peer and answers "is that host reachable over
      # this carrier" -- which is the level above the service and the level
      # below the network.
      #
      # It is the carrier's to declare because the answer is the carrier's: a
      # tunnel is tested differently from a wire, and no service on either
      # should have to know which it is riding.
      probe = mkOption {
        type = types.nullOr (types.functionTo types.str);
        default = null;
        example = lib.literalExpression ''{ link, ... }: "ping -c1 -W2 ''${link.address}"'';
        description = "`{ link, ... }: \"<shell command>\"` -- exits 0 when that peer is reachable over this carrier. Composed ahead of a capability's own probe.";
      };

      domain = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          The DNS zone this carrier addresses its members under -- a tailnet's
          MagicDNS suffix, or the zone gated services are published in. Read by
          the carrier's own `gives`, and composed with a service's `subdomain`
          to form the published name.
        '';
      };

      # ── A CONTAINER BRIDGE'S TWO DEPLOYMENT FACTS ──
      # Beside `prefix` and `domain` and for the same reason: they are what a
      # carrier knows about itself that nothing can derive. The gateway
      # address differs between docker (172.17.0.1 unless `bip` says
      # otherwise) and podman (10.88.0.1), and a host whose daemon is
      # configured differently says so once rather than in each container's
      # connection string. See ../network/container-bridge.nix.
      gatewayAddress = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "172.17.0.1";
        description = "The host's address as seen from inside a container on this bridge.";
      };

      bridgeInterface = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "docker0";
        description = "The bridge on the host's side, which is where a container's traffic arrives.";
      };

      # ── WHO HAS TO BE ATTACHED FOR A LINK TO EXIST ──
      # A SEGMENT needs both ends: two hosts are siblings on the home lan
      # because each is cabled to it, and "A can see B's" is a different and
      attachment = mkOption {
        type = types.enum [
          "both"
          "provider"
        ];
        default = "both";
        description = ''
          Which ends must include this carrier for it to carry a link between
          them. `both` for a wire; `provider` for a route the serving side
          publishes and anyone can reach.
        '';
      };

      preference = mkOption {
        type = types.int;
        default = 50;
        description = ''
          Tie-break order among carriers that both ends share, ASCENDING, and
          read as DEPTH OF LAYERING rather than speed: a loopback has no
          underlay to be up, a wire has one, a tunnel has two. A longer route
          acquires dependencies the call did not have, which is why
          ../services/nextcloud.nix scrapes over loopback rather than its own
          public vhost.

          It orders a candidate list and never shortens it -- see the note on
          `linksBetween` for why the candidate SET is a fact and only the order
          is a judgement.
        '';
      };

      reach = mkOption {
        type = types.enum [
          "host"
          "household"
          "fleet"
        ];
        default = "host";
        description = ''
          How far a take may resolve to this giver. The DEFAULT IS `host`
          deliberately: the safe answer is "do not reach across machines", so
          being shared is opted into rather than discovered by accident.

          `host` is what postgres needs -- auspc must use its own database and
          never secondpc's -- and for a socket-only provider it is belt and
          braces, because `unix` narrows no address and so cannot leave the box
          anyway. `household` is the shared appliance (music-assistant,
          audiobookshelf). `fleet` is infrastructure every host may reach, and
          owes an explanation.
        '';
      };
    };
  };

  # THE PIPELINE MUST NOT DISPATCH THESE AS CLASS CONTENT. den's
  # `fx/key-classification.nix` treats an unrecognised attrset-of-attrsets as
  config.den.reservedKeys = [
    "gives"
    "takes"
    "prefix"
    "spans"
    "probe"
    "domain"
    "attachment"
    "preference"
    "reach"
    # ../network/container-bridge.nix's two deployment facts. Reserved for the
    # same reason the rest are: den's pipeline treats an unrecognised
    # attrset-of-attrsets as class content, and a bare string is not that --
    # but a key the schema declares still has to be kept out of the dispatch.
    "gatewayAddress"
    "bridgeInterface"
  ];

  # ── THE DEFINITION SET, AS AN ARGUMENT ──
  #
  # So a capability can be declared BY WHATEVER OWNS IT rather than here:
  # `mqtt` belongs beside the broker that gives it, `dns` beside the zone that
  # writes records, `share` beside the file services. One central file makes
  # ownership and editing odd -- the thing this tree keeps removing everywhere
  # else -- and the only reason it was central is that definitions reference
  # each other by value (`includes = [ caps.tcp ]`).
  #
  # There is no cycle to work around: `caps` IS `config.den.capabilities`, and
  # a reference into it is lazy. So a file writes
  #
  #   { caps, ... }: { den.capabilities.mqtt.includes = [ caps.tcp ]; }
  #
  # and the registry resolves it like any other option definition.
  #
  # WHAT STAYS HERE: the shared vocabulary everything else narrows --
  # `carried`, `address`, `tcp`, `udp`, `unix`, `credential`, `user`,
  # `hostname`, `segment`. Those are not owned by any one service, which is
  # the test for belonging in this file.
  # `config._module.args`, not a bare `_module`: this file already carries
  # explicit `options.` and `config.` attributes at the top level, and mixing
  # the two forms is "has an unsupported attribute `_module'".
  config._module.args.caps = caps;

  config.den.capabilities = {
    # ── THE PRIMITIVES, each the smallest thing worth asking for alone ──

    # EVERY CAPABILITY SAYS WHO FURNISHED IT. Included by all of them rather
    # than repeated, so a new capability cannot forget it.
    carried = {
      meta.description = "Provenance. Included by every capability.";
      options.carrier = mkOption {
        type = types.str;
        description = "Which giver furnished this. Queryable, never named by a consumer.";
      };

      # ── WHICH ONE, WHICH IS AS GENERIC AS WHICH CARRIER ──
      # Every level of a chain filters on it (`takes.celler.name = "secondpc"`,
      # and `gives.cache.main` keys a sub-take by it), so it is not a fact about
      # cellers or caches -- it is what distinguishes two instances of anything.
      # Null for a capability with nothing to distinguish: there is one loopback
      # and one lan, and neither is named.
      options.name = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Which instance this is, by the name its giver furnishes. What a take's `name` filters on.";
      };

      # ── THE TRANSPORT RIDES WITH THE CONTRIBUTION ──
      #
      # On `carried`, so EVERY capability has it rather than each one growing
      # its own spelling. It is the half that was missing from a config
      # contribution, and the gap was measurable: remove
      # ../services/openrgb.nix's hand-written `expose` and auspc opens
      # NOTHING -- `allowedTCPPorts` empty, no per-interface rule -- while the
      # take that puts the `openrgb` component into Home Assistant resolves
      # perfectly well. Resolution and reachability were two declarations with
      # nothing holding them together.
      #
      # ── WHAT IT IS FOR ──
      #
      # A contribution take -- `takes.home-assistant`, `takes.nginx`,
      # `takes.gated` -- is a provider handing an implementation to a consumer:
      # "here is the config you need in order to use me". For anything that is
      # not purely local, that handover is incomplete without the wire: the
      # consumer has to DIAL the provider, and it is the provider that knows on
      # which port. So the take carries it, and ../base/firewall.nix derives the
      # hole from the consumer's location rather than from a list somebody
      # remembered to write.
      #
      # THE POINT IS THAT IT COLLAPSES AS A UNIT. One declaration produces the
      # component and the hole, so an aspect that is removed, excluded, or fails
      # to resolve takes both with it. Two declarations could not do that: the
      # tree has already had a component contributed and never read, and it can
      # just as easily keep a hole open for a service that is gone.
      #
      # AND THE TWO ENDS NEED NOT BE CO-LOCATED -- that is the case this exists
      # for. The consumer's host is wherever it is; the carrier between the two
      # is resolved, with every clause and `preference` applied, exactly as any
      # other link.
      #
      # NULL FOR A CLIENT-SIDE TAKE, which is most of them. ../services/vban.nix
      # takes `udp` in order to SEND to a receiver; it listens for nothing and
      # has nothing to open. Null is the honest answer there, not an omission.
      taker.options.listensOn = mkOption {
        type = types.nullOr (
          types.submodule {
            options = {
              capability = mkOption {
                type = types.str;
                example = "tcp";
                description = "Which of this aspect's own `gives` the consumer dials it over.";
              };

              # Null means "the capability's only port", which is the common
              # case and needs no annotation. A multi-port giver must name one:
              # exposure is per-port and a capability-level answer would open
              # Home Assistant's loopback-only 8123 on the LAN because its
              # HomeKit bridge has a LAN consumer. See `endpoint`'s own `port`.
              port = mkOption {
                type = types.nullOr types.str;
                default = null;
                example = "sdk";
                description = "Which named port of that capability, or null when the giver furnishes exactly one.";
              };
            };
          }
        );
        default = null;
      };
    };

    # THE GUARANTEE. `tcp` and `udp` are narrowings of this, which is what makes
    # them routed and `unix` not.
    address = {
      meta.description = "A dialable address. The guarantee that something is reachable at all.";
      side = "carrier";
      includes = [ caps.carried ];
      options = {
        address = mkOption {
          type = types.nullOr types.str;
          description = "Where the far end is dialled. Null when this giver has no attachment for that host.";
        };

        # ── AND WHICH WIRE IT ARRIVES ON, WHICH IS THE OTHER HALF ──
        #
        # An address says where the far end is; this says where the near end
        # HEARS it. They are both facts about the attachment and neither is
        # derivable from the other -- secondpc is 192.168.0.26 on `br0`,
        # because its NIC and its access point are bridged, and nothing about
        # the address says so.
        #
        # It exists because `expose` cannot mean anything without it. nixpkgs
        # folds `networking.firewall.allowedTCPPorts` into a pseudo-interface
        # called `default` and emits its rules with no `-i` at all, so a port
        # opened that way is open on every wire the host has -- the tailnet,
        # the lan and whatever the router forwards. "Reachable over the
        # tailnet and not from the LAN" is a sentence that needs this field to
        # be sayable, and ../base/firewall.nix reads it off the LINK rather
        # than off the host, because which interface a carrier lands on is the
        # carrier's fact in exactly the way the address is.
        #
        # Null for a carrier with no interface of its own -- a published name
        # resolving to somebody else's edge, for instance, which is reached
        # over whatever route the host already has rather than over a wire of
        # its own.
        interface = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "br0";
          description = "The kernel interface this carrier's traffic arrives on, from the furnishing host's side.";
        };

        # ── IS IT REACHABLE FROM INSIDE A NETWORK NAMESPACE? ──
        #
        # `127.0.0.1` means a different machine depending on who says it. To a
        # service on the host it is the host; to a container it is the
        # container, and the host is somewhere else entirely -- which is why
        # ../services/splitpro.nix ran with `--network=host`: not because it
        # needed the host's network, but because that is the one way to make
        # `localhost` mean what its connection string assumed.
        #
        # The price was a shared port namespace, and it came due: splitpro and
        # grafana both bound 127.0.0.1:3000 on one box and nginx proxied two
        # vhosts to one socket.
        #
        # So a carrier says whether it survives the namespace boundary. A
        # containerised consumer filters on it and resolves the bridge; the
        # loopback is excluded by the same clause rather than by the consumer
        # knowing it should be.
        namespaced = mkOption {
          type = types.bool;
          default = false;
          description = "Whether a process inside a network namespace on this host can reach the far end over this carrier.";
        };

        # ── HOW BIG A FIBER MAY BE, which is a CARRIER fact ──
        # Furnished rather than remembered, because both numbers here are
        # constraints the tree currently carries as prose and has got wrong
        mtu = mkOption {
          type = types.nullOr types.int;
          description = ''
            Largest packet this carrier passes unfragmented. `null` means no
            limit worth modelling. ../network/vpn.nix
            is 1420 and tailscale's tun is 1280, which is LOWER -- so
            ../services/vban.nix's refusal to send audio through a tunnel ("a
            full VBAN packet does not fit a 1420-byte tunnel MTU") excludes
            both, and the lan is the only carrier that can take it. Verify
            against a live tunnel before relying on the exact number.
          '';
        };

        maxBodyBytes = mkOption {
          type = types.nullOr types.int;
          description = ''
            Largest request body this carrier passes. `null` means UNBOUNDED,
            and because this option has no default that is a claim the giver
            makes rather than a silence anyone can fall into.
            Cloudflare caps a proxied body at 100MB, which is why
            `cache.ivymect.in` is excluded from the tunnel while a `celler
            pull` over it is fine -- the cap binds on bodies, so it is
            asymmetric, and nextcloud's `maxUploadSize = "10G"` on a vhost that
            is NOT excluded is the same constraint still unresolved.
          '';
        };
      };

      # THE RULE THAT FOLLOWS FROM THE TWO HALVES, declared beside the fields it
      # reads rather than restated by every uploading flow.
      rules = [
        (
          { carrier, take, ... }:
          let
            lim = carrier.maxBodyBytes or null;
            # FROM THE TAKE, because direction is the flow's and not the
            # giver's -- see `endpoint`'s own `direction`.
            dir = take.direction or "in";
          in
          if lim != null && (dir == "out" || dir == "both") then
            "caps request bodies at ${toString lim} bytes, and this flow sends them"
          else
            null
        )
      ];
    };

    # A NAME, SEPARATE FROM AN ADDRESS, and askable without one.
    # Conflating the two was the gap this fixes. The lan has an address and no
    hostname = {
      meta.description = "A resolvable name for a host, under a namespace the giver serves.";
      side = "carrier";
      includes = [ caps.carried ];
      options = {
        hostname = mkOption {
          type = types.str;
          description = "The canonical name this giver addresses the host by.";
        };
        aliases = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            Other names resolving to the same host UNDER THIS GIVER. MagicDNS
            answers a bare `secondpc` as well as the fqdn, and ssh checks
            known_hosts against whatever it actually dialled -- so both names
            are the giver's to furnish, not the consumer's to guess.
          '';
        };
        domain = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "The namespace the name sits under, where there is one.";
        };
      };
    };

    # WHAT MAKES A GIVER A SEGMENT RATHER THAN A PER-HOST THING. Several hosts
    # attach to a segment and are thereby connected; every host has a loopback
    segment = {
      meta.description = "Attachment to a wire. `spans` says whether anything else is on it.";
      side = "carrier";

      # NULLABLE, because a loopback is a segment and has no CIDR. The pairing
      # is checked rather than assumed: ../network/carrier-attachment.nix's
      # `carriers-declare-how-far-they-span` is the error.
      options.prefix = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "The segment's CIDR -- a fact about the wire, identical for everything on it.";
      };

      # ── THREE VALUES, BECAUSE A CONTAINER IS AN ENTITY ──
      #
      # `one-host` and `many-hosts` were enough while every entity was its own
      # machine. A container is an entity too -- `den.hosts.x86_64-linux.celler2`
      # is one, class `nixos`, with its own attachment -- and the wire between a
      # container and the machine it runs on is neither: it does not stop at one
      # entity, and it does not leave the box.
      #
      # `one-machine` is that wire. It makes two entities siblings when they
      # share a machine and never otherwise, which is what lets a container
      # resolve its host's services without a loopback claiming to span hosts.
      options.spans = mkOption {
        type = types.enum [
          "one-host"
          "one-machine"
          "many-hosts"
        ];
        description = "One entity, every entity on one machine, or across machines. What decides when two entities are siblings.";
      };
    };

    # ── A CREDENTIAL BELONGS TO THE EDGE, NOT TO EITHER END ──
    # This is the home the five hand-rolled shared secrets never had, and the
    # tree says so itself: ../services/nextcloud.nix manufactures the OIDC
    credential = {
      meta.description = "A secret both ends of an edge read, owned by neither.";
      includes = [ caps.carried ];

      # DECLARED, where it used to be buried. `realise` below resolves each
      # side's `user` to decide who owns that side's copy -- a requirement
      takes = [ caps.user ];

      options = {
        scope = mkOption {
          type = types.str;
          description = ''
            The `age.scoped.<key>` entry holding it -- THE TAKER'S OWN SCOPE,
            not a third one named after the edge.

            This is forced, not stylistic. A template may not depend on another
            scope's secret: `age.templates` -> `age.scoped.<other>.access` ->
            `age.templates` recurses, and it surfaces only at `gen-secrets`
            rather than at eval. ../services/donetick.nix renders its WHOLE
            config as a template interpolating three secrets -- a role password,
            an OIDC client secret and its own jwt secret -- so if a shared
            credential lived in an edge-named scope, that template would reach
            across one and cycle.

            Putting it in the taker's scope keeps every template dependency
            local, and it is what the tree already does: ./nextcloud.nix holds
            BOTH materialisations of the OIDC secret in `nextcloud`'s scope, and
            ./donetick.nix holds the kanidm-owned copy in `donetick`'s.

            The giver reads across scopes, which is fine -- only TEMPLATE
            dependencies cycle, and `scoped.<anyScope>.secrets.<k>.path` is a
            plain read.
          '';
        };
        secretName = mkOption {
          type = types.str;
          description = "The secret's name within that scope.";
        };
        # ── A STATIC SOURCE, WHICH IS THE ALTERNATIVE TO MINTING ONE ──
        #
        # `credential` is a forward: the realisation is what declares the
        # agenix secret, so the credential has to carry where its value comes
        # from. Usually that is `generator` below -- we mint it. Sometimes it
        # cannot be: the far end of the edge is a machine this tree does not
        # configure, its `authorized_keys` or its account was filled by hand,
        # and the value has to be one that already matches. Then the secret has
        # a `rekeyFile` and no generator, and is entered with
        # `nix run .#secret-edit`.
        #
        # SETTING THIS MEANS `generator` IS NOT USED. It is not nullable --
        # every other credential here is minted and wants its default -- so the
        # presence of a source is what says "do not mint this one".
        rekeyFile = mkOption {
          type = types.nullOr types.path;
          default = null;
          example = lib.literalExpression "./secrets/static/fargonekey.age";
          description = ''
            Where this credential's value comes from, repo-relative. NULL IS
            THE NORMAL CASE and means scoped-generated: agenix-rekey derives the
            path from `age.rekey.generatedSecretsDir`, which
            ../security/agenix-rekey.nix keys per host -- so a minted credential
            states nothing and gets `secrets/generated/<host>/<name>.age`, one
            per host that holds it. ../network/vpn.nix's `wireguard_key` is that
            shape: a `generator.script` and no path.

            Stating a path is how an edge we cannot mint says so -- the far end's
            account was filled by hand on a machine this tree does not
            configure, so the value has to be one that already matches it.
            `generator` is then not used.
          '';
        };

        # ── WHAT THE GIVER ACTUALLY READS ──
        #
        # A password is symmetric: the service sends it and postgres checks it,
        # so both ends hold the same secret. A KEYPAIR is not: the client needs
        # the private half and the server only the public one. Deploying the
        # giver's copy anyway put every client's private key on every builder
        # it offloads to -- measured, as `builder-secondpc-auspc/builder-key-auspc`
        # owned by `builder` on auspc, which sshd never reads.
        #
        # `public` installs no deployed giver copy: the giver's host declares
        # the source only as an intermediary, which is enough to know its path
        # and read the `.pub` the generator writes beside it.
        giverReads = mkOption {
          type = types.enum [
            "secret"
            "public"
          ];
          default = "secret";
          description = "Whether the giver deploys its own copy of the secret, or only reads the public half written beside it.";
        };

        generator = mkOption {
          type = types.str;
          default = "random-hex-32";
          description = ''
            The named generator that mints it (../security/random-secrets.nix).
            The GIVER chooses, because the shape of the secret is its own
            requirement -- an OIDC client secret and a database password are
            both random bytes, a signing key is not.
          '';
        };

      };

      # ── WHAT THE HOLDING SIDE DOES WITH ITS COPY ──
      # The two sides are symmetric in the type and asymmetric in practice: the
      # giver writes config FROM the secret, the taker CONSUMES it, and how it
      taker.options = {
        # What the principal is FOR, where a name alone does not say. Rendered
        # as the kanidm service account's display name.
        description = mkOption {
          type = types.str;
          default = "";
        };

        # Units holding the secret in memory. A list, so it merges if the
        # holder's own config adds to it from elsewhere.
        restartUnits = mkOption {
          type = types.listOf types.str;
          default = [ ];
        };

        # Set when the holder reads the secret from its ENVIRONMENT rather than
        # from a path -- the gateway renders one env file per such holder. Null
        # for a holder that is handed a file, which is most of them.
        envPrefix = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
      };

      # ── NO `sides` FIELD: THE EDGE IS THE TWO SIDES ──
      # An earlier version carried `sides = { giver = "kanidm"; taker =
      # "nextcloud"; }`, which restated what resolution already knows -- a
      realise =
        {
          giver,
          taker,
          giverHost,
          # THE OTHER END. Every caller already passes it -- ../services/postgres.nix
          # as `takerHost = host`, ./capability.nix's `realiseOn` as the asking
          # host -- and it was simply not destructured, so `edgeKey` below read
          # an undefined variable.
          takerHost ? { },
          resolve,
          link,
          ...
        }:
        let
          accountOf =
            role: aspect:
            let
              u = resolve "user" aspect;
            in
            if u == null then
              throw "capability: the ${role} '${aspect.name}' of a credential gives no `user`, so there is no account to own its copy of the secret. Add `gives.user` to den.aspects.${aspect.name}."
            else
              {
                owner = u.name;
                inherit (u) mode;
              };

          # ── THE LINK, AS A FOLDER NAME ──
          # Both ends derive it identically, which is what makes them name one
          # file. The hosts, not the aspects: an aspect name is shared by every
          # host running it, so it cannot distinguish two links to one giver.
          edgeKey = "${takerHost.name or "unknown"}-${giverHost.name or "unknown"}";

          # THE TAKER GETS THE PLAIN NAME, because the scope is the taker's and
          # its own templates refer to the secret by that name
          #
          # WHICH PROVIDER, where the giver spans hosts. `giverHost` is the one
          # thing an edge could not say before: the giver's `gives` is evaluated
          # in the ASKER's scope, so a giver cannot name its own host, and the
          # aspect name is shared by every host running it.
          # ALWAYS THE GIVER'S HOST. Not the giver ASPECT, whose name every
          # host running it shares, so it could never tell two providers of
          # one capability apart.
          providerName =
            giverHost.name
              or (throw "capability: the credential `${link.secretName}` has no giver host to key it by. Realise it from a resolved candidate (`capability.realiseOn`), which carries one.");

          # The taker keeps the PLAIN name in its group, because its own
          # templates read it by that name (../services/donetick.nix's
          # `placeholders.db-password`). The giver's copy is told apart by its
          # provider.
          keyFor = role: if role == "taker" then link.secretName else "${link.secretName}-${providerName}";

          # ── A SIDE, AS SOMETHING A CLASS BODY CAN READ ──
          # `pathIn` is a function of `scoped` rather than a path, because a
          # deployed path needs the SCOPE's fixpoint.
          sideOf =
            role: account:
            account
            // {
              inherit (link) scope;
              key = keyFor role;
              pathIn = scoped: scoped.${link.scope}.secrets.${keyFor role}.path;
            };

          g = sideOf "giver" (accountOf "giver" giver);
          t = sideOf "taker" (accountOf "taker" taker);

          # ── THE CREDENTIAL ITSELF: ONE SOURCE, FILED UNDER THE LINK ──
          # Its folder is the link, so ../../lib/age-scoped.nix files it per
          # provider and agenix generates it once per unique path. A stated
          # source wins: an edge whose far end this tree does not configure has
          # a value that must match what is already there, and an existing file
          # is never regenerated.
          takerEntry = {
            inherit (t) owner mode;
            generator.script = link.generator;
            scope = "${edgeKey}/${link.scope}";
          }
          // lib.optionalAttrs ((link.rekeyFile or null) != null) { inherit (link) rekeyFile; };

          # ── THE GIVER'S COPY INHERITS FROM THE SCOPED VIEW ──
          # Not a second source: the same file, read through `secrets` in this
          # scope, so the two ends cannot name different files.
          giverEntry =
            { secrets, ... }:
            {
              inherit (secrets.${t.key}) rekeyFile;
              inherit (g) owner mode;
            };
        in
        {
          giver = g;
          taker = t;
          inherit (link) scope;

          # ── WHAT EACH HOST INSTALLS ──
          # `secrets` is both entries, for an edge whose two ends are one host
          # (postgres beside the service that asks it).
          #
          # Across hosts each end installs into its own `age.scoped`, and the
          # giver's inheritance only works if the scoped view it is handed
          # HOLDS the taker's entry. So the giver's host declares it too, as an
          # intermediary: there to inherit from, never deployed -- the shape
          # celler's signing key already uses (../celler/_lib.nix:
          # "agenix-rekey dedups the two definitions by file").
          secrets = {
            ${t.key} = takerEntry;
          }
          // lib.optionalAttrs (link.giverReads == "secret") { ${g.key} = giverEntry; };
          takerSecrets = {
            ${t.key} = takerEntry;
          };
          giverSecrets = {
            ${t.key} = takerEntry // {
              intermediary = true;
            };
          }
          // lib.optionalAttrs (link.giverReads == "secret") { ${g.key} = giverEntry; };
        };
    };

    # ── AN ACCOUNT, WHICH THE PROVIDER FURNISHES ──
    # "A user for a service is more of something a service provides type of
    # deal, its like an ip address, its something we *ask* for and that a service
    user = {
      meta.description = "A unix account a service furnishes, for whoever asks.";
      includes = [ caps.carried ];
      options = {
        name = mkOption {
          type = types.str;
          description = ''
            The service's account name -- what a provider names things after.
            ../services/postgres.nix derives the database and the role from it,
            because peer authentication checks the connecting unix user against
            the role.
          '';
        };

        # ── WHO READS A FILE, WHICH IS NOT ALWAYS WHO THE SERVICE IS ──
        # They coincide for a service with a unix account of its own and come
        # apart for anything whose secret is consumed through an age TEMPLATE:
        owner = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "The account that reads files furnished to this service. Null means `name`.";
        };
        group = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Its primary group, where the service has one.";
        };
        mode = mkOption {
          type = types.str;
          default = "0400";
          description = ''
            Default mode for a file this account is given to read. Here rather
            than on `credential` because how tightly a service's own secrets
            are held is a fact about the service -- one that unpacks an
            environment file as root wants something different from one that
            reads it as itself.
          '';
        };
      };
    };

    # ── AN OIDC REGISTRATION, which is a credential plus a name ──
    # A narrowing of `credential`, so anything that can read a shared secret can
    # read an OIDC client -- and the consumer asks for `oidc` rather than for a
    oidc = {
      meta.description = "A client registration at an identity provider, with the credential for it.";
      includes = [ caps.credential ];
      options = {
        clientId = mkOption {
          type = types.str;
          description = "The registration's id -- the asker's name, since the provider names it after whoever asked.";
        };
        issuer = mkOption {
          type = types.str;
          description = "Where the provider publishes its discovery document.";
        };
      };

      # ── WHAT THE TAKER HAS TO SAY, BECAUSE NOTHING ELSE CAN KNOW IT ──
      #
      # These existed nowhere and the take was therefore UNUSABLE: measured,
      # `den.capabilities.oidc` accepted no contribution and nothing in the tree
      # took it, so a consumer had no way to state its callback and
      # ../services/kanidm.nix's registration had no way to learn one. The
      # first aspect to write `takes.oidc` would have hit a missing required
      # `callback` on `gateway.oidcClients`.
      taker.options = {
        # THE ONE GENUINELY PER-APP FACT: every OIDC library picks its own
        # redirect path, and there is no deriving it from a name. Stated by the
        # taker, read by the provider -- which is the whole point of the edge
        # carrying both ends.
        callback = mkOption {
          type = types.str;
          default = "";
        };

        # What the provider's own app tile calls it. Defaults to the taking
        # aspect's name at the provider, so most consumers say nothing.
        displayName = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
      };
    };

    # ── AN ENDPOINT IS NOT A CARRIER, AND BOTH "GIVE HTTP" ──
    # This split was missing and it is the reason the public route did not work.
    # Two different kinds of thing furnish a link, and each knows only half of
    endpoint = {
      meta.description = "The service half of a link: port, scheme, publication. No address.";
      side = "endpoint";
      includes = [ caps.carried ];

      # A published endpoint HAS a name, composed from this side's `subdomain`
      # and the carrier's zone -- ./capability.nix's `composeLink` attaches it.
      # `takes`, not `includes`: an endpoint is not a kind of name.
      takes = [ caps.hostname ];

      # ── WHICH WAY THE BULK FLOWS, DECLARED BY THE TAKER ──
      # A TAKER OPTION, not a furnished one, and the distinction is load-bearing.
      # Direction is a property of the FLOW, not of what the giver offers: the
      taker.options.direction = mkOption {
        type = types.enum [
          "in"
          "out"
          "both"
        ];
        default = "in";
      };

      # ── WHICH OF THE GIVER'S PORTS THIS FLOW USES ──
      #
      # A take names a CAPABILITY, not one of its ports, and for resolution that
      # is right: the taker asks for `tcp` from a service and the resolver
      # answers with every port the giver furnishes, which is why
      # ../services/vban.nix can read `receiver.ports.vban.port` off the link.
      #
      # It stops being enough the moment the FIREWALL is derived from the taker
      # set rather than declared. Exposure is per-port -- Home Assistant's 8123
      # sits behind nginx on loopback while its HomeKit bridge is reachable from
      # the LAN -- and a cap-level consumer set cannot distinguish them. Deriving
      # at cap level would open 8123 on the LAN because a DIFFERENT port of the
      # same capability has a LAN consumer, which is wider than anything anybody
      # asked for.
      #
      # So a taker that wants one named port says so, and the derivation is
      # exact. Null is the honest default and means "the capability's only
      # port": a single-port giver -- ../services/openrgb.nix's `sdk`,
      # ./homeassistant/mosquitto.nix's `mqtt` -- is unambiguous and needs no
      # annotation, and a MULTI-port giver with a null take cannot be attributed,
      # which ../base/firewall.nix reports rather than guessing at.
      taker.options.port = mkOption {
        type = types.nullOr types.str;
        default = null;
      };
      # ── A SOCKET IS ONE PER HOST, AND A PUBLISHED NAME ONE PER ZONE ──
      # Both are resources rather than values, and both were unchecked. The
      # second claimant on a port gets EADDRINUSE at boot; the second claimant
      # on a subdomain gets whichever vhost the proxy happened to order first.
      exclusive = {
        ports = {
          scope = "host";
          claims =
            link: ctx:
            lib.mapAttrsToList (name: p: {
              inherit name;
              inherit (p) port;
              # ASKED PER PORT, and `bindAt` already resolves "stated wins,
              # otherwise derive from `expose`" -- so the choice is not made
              # twice. ./capability.nix's `bindAddressFor` is the one place
              # that rule lives.
              bind = p.bind // {
                address = ctx.bindAt name;
              };
              inherit (p) shared;
              inherit (p.meta) description;
            }) link.ports;
          key = c: toString c.port;

          # ONE DECLARATION IS ENOUGH. `||`, not `&&`: a group is declared
          # shared in ONE place -- the aspect that owns it -- and every other
          # speaker inherits that through the merge rather than restating it
          # at each use. The cost is stated plainly: a `shared` written by
          # mistake excuses every other claim on that number too, so it
          # belongs on the group and not on a service.
          shares = a: b: a.shared || b.shared;

          # EQUAL PORT IS NOT YET THE SAME SOCKET. A wildcard address covers
          # every address, so it collides with anything; two specific addresses
          # collide only with each other. Interfaces narrow it further, and an
          # EMPTY list is unrestricted rather than empty -- most services cannot
          # say which interface they bind, so treating empty as "no interfaces"
          # would make every pair of them look disjoint and find nothing.
          overlaps =
            a: b:
            let
              wild = x: x == null || x == "0.0.0.0" || x == "::" || x == "";
            in
            (wild a.bind.address || wild b.bind.address || a.bind.address == b.bind.address)
            && (
              a.bind.interfaces == [ ]
              || b.bind.interfaces == [ ]
              || lib.intersectLists a.bind.interfaces b.bind.interfaces != [ ]
            );

          describe =
            c:
            "`${c.name}` (${if c.bind.address == null then "*" else c.bind.address}:${toString c.port}${
              lib.optionalString (c.bind.interfaces != [ ]) " on ${lib.concatStringsSep "," c.bind.interfaces}"
            }${lib.optionalString c.shared ", shared"})";
        };

        # FLEET-SCOPED, because the zone is. Two hosts publishing the same
        # subdomain is the bug a per-host check cannot see, and moving a service
        # between hosts is exactly when it happens.
        subdomain = {
          scope = "fleet";
          claims = link: _: lib.optional ((link.subdomain or null) != null) { inherit (link) subdomain; };
          key = c: c.subdomain;
          describe = c: "`${c.subdomain}`";
        };
      };

      options = {
        # ── NAMED PORTS, EACH WITH ITS OWN `expose` ──
        # `port` IS AN ALIAS ONTO `ports` AS A SINGLETON, so there is exactly
        # one thing to read. A giver writes whichever is natural -- `port = 8123`
        ports = mkOption {
          type = types.attrsOf (
            types.coercedTo types.int (n: { port = n; }) (
              types.submodule {
                options = {
                  port = mkOption {
                    type = types.int;
                    description = "The number.";
                  };

                  # ── WHAT IT IS FOR, DECLARED BESIDE THE NUMBER ──
                  #
                  # A number alone cannot be reviewed. This tree carried `853`
                  # and `21115-21119` in a hand-written firewall list for as
                  # long as anyone remembers and nothing anywhere could say
                  # what served them, which is why they could not be moved.
                  #
                  # UNDER `meta`, which is where a capability already puts its
                  # own `meta.description` -- the same slot for the same kind of
                  # fact, so there is one place to look rather than a
                  # `description` that means documentation here and a furnished
                  # value there.
                  #
                  # NULLABLE rather than required: `ports.http = 8095` coerces
                  # a bare int and has nowhere to put one, so the gap is a
                  # REPORT (./capability.nix's
                  # `declared-ports-say-what-they-are`) instead of a type error
                  # that would reject the shorthand.
                  # ── A GROUP IS HELD BY EVERY PARTICIPANT, NOT BY ONE ──
                  # mDNS on 5353 and SSDP on 1900 are bound by every speaker on
                  # the box at once: the socket sets `SO_REUSEADDR`, joins the
                  # group, and each joiner gets its own copy of every group
                  # datagram. So two claims here are not two claimants fighting
                  # over one socket -- they are one shared thing, and the rule
                  # above MERGES them rather than reporting a conflict.
                  #
                  # DECLARED IN ONE PLACE. `shares` is an `||`, so the aspect
                  # that owns the group says `shared = true` once and every other
                  # speaker inherits it through the merge -- no flag repeated at
                  # each use site. ../network/multicast.nix is where ours live.
                  #
                  # NOT FOR TWO UNICAST DAEMONS THAT BOTH WANT ONE PORT. That is
                  # a real conflict -- the second bind gets `EADDRINUSE`, or with
                  # `SO_REUSEPORT` the kernel hands each datagram to exactly one
                  # of them -- and it has a real answer: give one its own network
                  # namespace so it binds a container's veth instead of the
                  # host's, and `overlaps` then sees two different addresses.
                  # ../network/container-bridge.nix is that carrier.
                  shared = mkOption {
                    type = types.bool;
                    default = false;
                    description = "Every participant binds this port at once -- a multicast group. Claims on it merge instead of colliding.";
                  };

                  meta = mkOption {
                    type = types.submodule {
                      options.description = mkOption {
                        type = types.nullOr types.str;
                        default = null;
                        example = "Proxy-DHCP: answers PXE clients that already hold a lease.";
                        description = "What this port is for, and who dials it.";
                      };
                    };
                    default = { };
                    description = "This port's own documentation.";
                  };

                  # ── EXPOSED THROUGH WHAT, NOT MERELY EXPOSED ──
                  #
                  # A LIST OF CARRIERS, by aspect name, and the empty list is
                  # the default: closed.
                  #
                  # It was a bool, and a bool cannot say the thing that
                  # matters. nixpkgs folds `networking.firewall.allowedTCPPorts`
                  # into a pseudo-interface called `default` whose rules are
                  # emitted with no `-i` match at all, so "exposed" meant
                  # exposed on every wire the host has -- the tailnet, the
                  # lan, and whatever the router forwards. There was no way to
                  # say "the tailnet may reach this and the LAN may not",
                  # which is the normal case for anything administrative.
                  #
                  # Each name is a carrier this host is attached to, and
                  # ../base/firewall.nix opens the port on the INTERFACE that
                  # carrier furnishes (`interface` on the link, beside its
                  # address). A carrier the host does not have, or one with no
                  # interface of its own, is an assertion rather than a
                  # silently dropped rule -- the previous shape failed that
                  # way and nobody noticed for as long as it existed.
                  # ── `true` MEANS EVERY WIRE THIS HOST HAS ──
                  # The old global behaviour, said honestly and in one word:
                  # `networking.firewall.allowedTCPPorts` was reachable from
                  # every wire because it carried no `-i`, and `expose = true`
                  # is that, rewritten to the list of carriers the host is
                  # actually attached to. Rarely what anything wants -- it is
                  # here so the wide case can be WRITTEN as wide rather than
                  # smuggled in through a global list nobody can attribute.
                  #
                  # `*` is the sentinel the coercion produces, and
                  # ./capability.nix's `exposeFor` is the one place it expands,
                  # against that host's own carriers -- so it can never name a
                  # wire the host has no attachment to.
                  expose = mkOption {
                    type = types.coercedTo types.bool (b: lib.optional b "*") (types.listOf types.str);
                    default = [ ];
                    example = [ "tailscale" ];
                    description = ''
                      Carriers this port is reachable over, by aspect name --
                      `lan-home`, `tailscale`. Empty means closed, which is
                      the default because every service here binds loopback on
                      purpose and an open port is the exception that has to
                      say so: a gated service reachable directly would route
                      around its gate.
                    '';
                  };

                  # ── WHERE IT BINDS, WHICH IS NOT WHERE IT IS DIALLED ──
                  # `address` on the `address` capability is what a CONSUMER
                  # dials, furnished by whichever carrier the two ends share. It
                  # is a property of the attachment between two hosts.
                  #
                  # This is the other thing entirely: the socket this service
                  # opens on its OWN box. Every service here binds loopback and
                  # is reached through nginx, and that was true only inside each
                  # service's own config -- so nothing could check that two of
                  # them had not claimed one socket, which is the one thing a
                  # port genuinely cannot share.
                  bind = mkOption {
                    type = types.submodule {
                      options = {
                        address = mkOption {
                          type = types.nullOr types.str;
                          default = null;
                          description = ''
                            The address it listens on, where the service binds
                            somewhere resolution cannot work out -- a container
                            veth, an access point's own link.

                            NULL MEANS DERIVE IT, which is the usual case:
                            ./capability.nix's `bindAddressFor` resolves this
                            port's own `expose` -- loopback when it names no
                            carrier, the wildcard when it names any, because a
                            same-host consumer still dials the loopback and a
                            socket cannot bind both. So one declaration decides
                            what the firewall opens, which interface it opens it
                            on, and what the service binds; the firewall is what
                            narrows reachability, not the bind.
                            A bind address is not the service's own fact.

                            `0.0.0.0` and `::` are stated wildcards, and collide
                            with any other claim on the same port.
                          '';
                        };

                        interfaces = mkOption {
                          type = types.listOf types.str;
                          default = [ ];
                          description = ''
                            Interfaces it is bound to, where the service can say
                            so -- an AP's own link, a container's veth. EMPTY
                            MEANS UNRESTRICTED, not none: most services cannot
                            express this and bind by address alone, so an empty
                            list has to mean "wherever that address lives" or
                            every such service would look mutually exclusive
                            with every other.
                          '';
                        };
                      };
                    };
                    default = { };
                    description = "The socket this port opens on its own host. Derived from its consumers unless stated.";
                  };
                };
              }
            )
          );
          default = { };
          example = {
            http = 8095;
            vban = {
              port = 6980;
              expose = [ "lan-home" ];
              bind.address = "0.0.0.0";
            };
          };
          description = "The endpoint's ports, by name, each with its own `expose`. `port` writes one named after the capability.";
        };

        port = mkOption {
          type = types.nullOr types.int;
          default = null;
          description = "Convenience for a single-port endpoint. Read `ports`, which this feeds under the capability's own name.";
        };

        # NO LINK-LEVEL `expose`. It was one answer for a whole leg, which is
        # the wrong granularity: Home Assistant's 8123 sits behind nginx while

        subdomain = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            The name this service is published under, WITHOUT the zone --
            `cache`, not `cache.ivymect.in`. The zone belongs to the carrier
            (../hosts/fleet.nix's `publicZone`), so a service that moves between
            hosts publishes under the new host's zone without being edited.
          '';
        };
      };
    };

    # ── A PROXY, WHICH IS A MECHANISM A SERVICE ASKS FOR ──
    # "gated and gateway should more be that the services needs some way to
    # proxy; nginx provides a proxy mechanism; and then it just is passed into
    proxy = {
      meta.description = "Something that publishes a service under a name and forwards to it.";
      includes = [ caps.carried ];

      # ── WHAT THE PROXY FURNISHES BACK ──
      # `gateway.services.<n>` is not only a surface a service declares INTO --
      # `url`, `localUrl` and `domain` are read back out of it by about ten call
      options = {
        url = mkOption {
          type = types.str;
          description = ''
            Where a caller reaches this service. Callers must use this rather
            than the upstream: a per-caller key is only swapped for the
            service's own on the way through the gate, so a loopback shortcut
            presents a key the service has never heard of.
          '';
        };

        localUrl = mkOption {
          type = types.str;
          description = ''
            Where it is reachable on the host itself, bypassing the gate. For
            the rare call that cannot carry a key -- an app validating a
            callback URL -- where going through the gate would fail for want of
            a credential. Prefer `url`: this leg is unauthenticated and
            unattributed.
          '';
        };

        domain = mkOption {
          type = types.str;
          description = "The published name, `<subdomain>.<zone>`, composed from the service's subdomain and the host's zone carrier.";
        };
      };

      taker.options = {
        # WHERE IT IS PUBLISHED, as a subdomain and not a URL. The zone comes
        # from the carrier (../network/cloudflare.nix), so a service that moves
        subdomain = mkOption {
          type = types.nullOr types.str;
          default = null;
        };

        # The path prefix, for several services under one name --
        # `arr.<zone>/sonarr` and friends, which is what each *arr app's own
        # `urlbase` supports and qBittorrent does not.
        subpath = mkOption {
          type = types.str;
          default = "/";
        };

        # WHERE TO FORWARD. The port alone, not a URL: the proxy and the service
        # are on one host by construction, so the scheme and the loopback
        # address are the proxy's to supply and nothing else can get them wrong.
        port = mkOption {
          type = types.nullOr types.int;
          default = null;
        };

        # Upstream for the cases a port cannot express -- a unix socket, or a
        # container on another address. An escape hatch, because a closed form
        # here would force a fork the first time something needs one.
        upstream = mkOption {
          type = types.nullOr types.str;
          default = null;
        };

        # Server-sent events and websockets, which a proxy must not buffer.
        websockets = mkOption {
          type = types.bool;
          default = false;
        };

        # NO `sso` OR `groups` HERE: they belong to `gated` below, which is a
        # narrowing of this. A plain proxy publishes and forwards; a gate is
        # something ON TOP of that.

        # Largest request body the proxy should accept, in bytes. Here rather
        # than as a literal in each vhost because it is the same fact the
        maxBodyBytes = mkOption {
          type = types.nullOr types.int;
          default = null;
        };
      };
    };

    # ── GATED IS A NARROWING OF PROXY ──
    # "gated should be an abstraction of proxy as well on top of it" -- so it is
    # not a separate mechanism beside proxying, it is proxying PLUS
    gated = {
      meta.description = "A proxied service whose callers are authenticated.";

      # ONE NARROWING: `proxy`, because a gate is something on top of
      # publishing, and a gate-capable proxy can therefore also just proxy.
      includes = [ caps.proxy ];

      # AND ONE COMPOSITION: a caller's key is an EDGE credential -- the caller
      # presents its own and the gate swaps it for the service's internal one on
      takes = [ caps.credential ];

      taker.options = {
        # NO `groups` HERE. It was declared as a MERGED contribution and documented as
        # the channel ./access.nix contributes household grants through, and it
        # could never have worked: the union fold flattens to a list and drops
        # the per-taker key, so the only reader's `g.groups.<svc>` selected an
        # attribute off a list and got its default in silence. Nothing wrote it
        # and nothing could read it. A service's gate groups are declared where
        # they are used -- `gated.<n>.extraGroups` on the service's own aspect
        # -- so a wrong shape is a type error rather than an empty list.

        # ── WHO MAY CALL THIS, WHICH IS GENERIC ──
        # Each caller gets its own key, so a request is attributable to its
        # caller rather than to "something with the API key", and revoking one
        clients = mkOption {
          type = types.listOf types.str;
          default = [ ];
        };

        # ── NAMED AFTER THE THING IT TURNS ON ──
        #
        # Was `sso`, which claimed too much: vikunja and donetick are real
        # OIDC clients and ./kanidm.nix provisions them -- they ARE single
        # sign-on, and `sso = false` on them read as the opposite. What they
        # decline is the FORWARD-AUTH PROXY in front of the browser leg, which
        # would hand the app one trusted identity for everybody and bounce a
        # login through two providers.
        #
        # So the option says which mechanism, not which goal: oauth2-proxy on
        # or off, and SSO happens either way.
        oauthProxy = mkOption {
          type = types.bool;
          default = true;
        };

        # ── TELL THE SERVICE WHO THE PERSON IS ──
        #
        # `oauthProxy` authenticates the browser leg and then throws the
        # identity away: the service sees a request that got past the gate and
        # nothing about who sent it. That is right for an app with no accounts
        # and wrong for one whose whole auth IS "trust the proxy's header" --
        # grocy's `ReverseProxyAuthMiddleware`, which looks up a user by the
        # name in that header and creates them if absent.
        #
        # So this says: forward the authenticated person's SHORT USERNAME to
        # the upstream. Off by default, because a service that does not want it
        # should not be handed an identity it might start trusting.
        #
        # The gate supplies the value and the name of the header
        # (../services/gateway.nix) -- a taker says only that it wants one,
        # exactly as it says `oauthProxy` without naming oauth2-proxy's
        # endpoints. Which claim it comes from is the gate's business too, and
        # it matters: `preferred_username` is the short name (`auscyber`),
        # which is what `kanidm.name` is and therefore what samba, mail and NSS
        # already call that person -- where oauth2-proxy's `user` claim is the
        # email or the subject, so a service keyed on it would invent a second
        # name for the same human.
        forwardUser = mkOption {
          type = types.bool;
          default = false;
        };
      };
    };

    # ── THE CHAIN, DECLARED FROM THE BOTTOM ──
    # "a celler server exposes caches, and then caches expose pushes and pulls."
    # Each level names its ancestors by destructuring them, so the chain reads
    # off the definitions and there is no list to keep in agreement with them.
    celler = {
      meta.description = "A celler server, which caches hang beneath.";
      includes = [ caps.carried ];
    };

    # Hangs beneath a celler, and its INSTANCES come from an aspect: one celler
    # carries several caches, and which ones is a fact about the deployment
    # rather than about caches. So `cache` is GIVEN -- ../services/celler-service.nix
    # furnishes `cache.main` -- while the flows below it are derived.
    cache =
      { celler, ... }:
      {
        meta.description = "A cache on one celler. Named, because a celler carries several.";
        includes = [ caps.carried ];
        inherit celler;

        options.public = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Readable without a credential. False by default, because a cache a
            consumer can read with no token is a decision rather than a
            fallback -- and it is what the consumer branches on: a non-public
            cache needs a netrc, and the netrc needs the edge's token.
          '';
        };
      };

    # ── THE FLOWS ARE DERIVED, NOT GIVEN ──
    # "pull and push are things that are automatically there." There is exactly
    # one pull per cache and every field of it follows from the cache and the
    # celler above, so nothing declares them: a cache gets its flows by
    # existing, and a second cache cannot be added without them.
    #
    # `takes` and not `includes` for http: a pull is not a kind of http
    # endpoint, and as a narrowing it would answer somebody's bare `takes.http`.
    pull =
      { cache, celler, ... }:
      {
        meta.description = "Reading from one cache. Derived from it.";
        includes = [ caps.carried ];
        takes = [ caps.http ];
        inherit cache celler;

        furnishes = {
          # A pull of cache `main` IS named `main`: the flow does not get an
          # identity of its own, it inherits the one thing that distinguishes it.
          name = cache.name or null;
          public = cache.public or false;
          carrier = cache.carrier or null;
        };

        options.public = mkOption {
          type = types.bool;
          default = false;
          description = "From the cache. What the consumer branches on to decide whether it needs a netrc.";
        };
      };

    push =
      { cache, celler, ... }:
      {
        meta.description = "Writing to one cache. Derived from it.";
        includes = [ caps.carried ];
        takes = [ caps.http ];
        inherit cache celler;

        furnishes = {
          name = cache.name or null;
          carrier = cache.carrier or null;
        };
      };

    # ── NGINX-SPECIFIC FACTS, UNDER THEIR OWN CAPABILITY ──
    # "if they are nginx facts, put them under a nested nginx capability."
    # The point of `proxy` is that a service says what IT needs -- a name, a
    nginx = {
      meta.description = "Facts only nginx can serve: the API key rewrite, the banner, the landing page.";
      includes = [ caps.carried ];

      taker.options = {
        # ../services/gateway.nix's own `api` submodule, handed over whole. Raw
        # because that submodule is the authority on its own shape and typing it
        api = mkOption {
          type = types.attrsOf types.anything;
          default = { };
        };

        banner = mkOption {
          type = types.nullOr types.str;
          default = null;
        };

        landing = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
      };
    };

    # ── THE PROTOCOLS, as narrowings ──
    tcp = {
      meta.description = "A routed stream connection.";
      # BOTH HALVES: the address from a carrier, the port from the service.
      includes = [
        caps.address
        caps.endpoint
      ];

      # ── IS SOMETHING LISTENING, WHICH EVERY NARROWING INHERITS ──
      # Declared once here rather than on each of `http`, `smtp`, `builder` and
      # the rest: `probesFor` walks the narrowing closure broadest-first, so
      # this runs for all of them, after the carrier's own probe and before
      # whatever the narrowing adds.
      #
      # NECESSARY AND NOT SUFFICIENT, measured: faggot.sh's tailnet address
      # answers a connect on 22 and still refuses ssh, because the connect
      # reaches tailscaled rather than sshd. A capability whose usability needs
      # more than a listening socket says so with its own probe on top.
      probe =
        { link, ... }:
        let
          one = (link.ports.default or (lib.head (lib.attrValues link.ports))).port;
        in
        "nc -z -w3 ${link.address} ${toString one}";
    };

    udp = {
      meta.description = "A routed datagram connection.";
      includes = [
        caps.address
        caps.endpoint
      ];
    };

    # ── HTTP, A NARROWING OF TCP ──
    # "we should consider websocket a superset of http a superset of tcp". So
    # http narrows tcp, inherits the address guarantee and the port, and adds
    http = {
      meta.description = "An HTTP endpoint. A narrowing of tcp.";

      # ── `either`, BECAUSE THE SCHEME IS PER HOP ──
      #
      # The origin speaks one protocol and a TLS-terminating carrier presents
      # another: celler serves `http://` on 8069 and the zone in front of it
      # answers `https://`. As `endpoint` the carrier's claim was STRIPPED by
      # `typeHalf` and the origin's scheme survived composition, so resolving
      # celler from the laptop produced `http://cache.ivymect.in` -- a URL
      # that is wrong about the one thing it is for.
      #
      # `either` lets a carrier state it; `typeHalf` keeps only keys a giver
      # actually furnished, so a wire that says nothing about the scheme still
      # leaves the endpoint's standing. The carrier wins when both speak,
      # which is the right way round: what the consumer dials is the last hop,
      # not the first.
      side = "either";

      includes = [ caps.tcp ];
      options.protocol = mkOption {
        type = types.str;
        default = "http://";
        description = "Scheme including `://`, so a URL is a concatenation rather than a format string.";
      };
    };

    # ── GRPC, A NARROWING OF TCP ──
    # Beside `http` and for the same reason: a consumer wanting a gRPC endpoint
    # is answered by a gRPC giver and never by a bare tcp one, so
    grpc = {
      meta.description = "A gRPC endpoint. A narrowing of tcp.";
      side = "endpoint";
      includes = [ caps.tcp ];
      options.protocol = mkOption {
        type = types.str;
        default = "";
        description = ''
          Scheme prefix, including any `://`. Empty for a bare `host:port`,
          which is what an OTLP exporter configured by address alone wants;
          `insecure://` for the ones that demand a URL and have no TLS.
        '';
      };
    };

    # INCLUDES NOTHING, and that is the whole content of the type. A consumer
    # asking for `unix` can only ever resolve to its own host's loopback,
    unix = {
      meta.description = "A local socket. Cannot leave the host, because it is not a narrowing of `address`.";
      includes = [ caps.carried ];
      options.socketDir = mkOption {
        type = types.str;
        description = "Directory holding the socket.";
      };
    };
  };
}
