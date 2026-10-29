{
  den,
  lib,
  ...
}:
# ── A CONTAINER IS AN ENTITY, DECLARED UNDER THE HOST IT RUNS ON ──
#
# Like a user: `den.hosts.<sys>.<h>.containers.<name>`, its own scope, its
# parent structural. That is what lets it resolve from where it actually is --
# a service in a container dials its host over the bridge, because the
# loopback stops at one entity and the container's `127.0.0.1` is itself.
#
# Mirrors den's own `host-to-users` (modules/policies/core.nix) and this
# tree's `member` kind (./household.nix), which recorded the gotchas:
#   * the kind needs `inherits = [ den.schema.conf ]` to be an entity with
#     `hasAspect`, and a `class`/`classes` for that to resolve;
#   * den does NOT pluralise a custom kind's children onto the parent, so
#     `containers` is declared here by hand, as den declares `users`.
let
  inherit (lib) mkOption types;
  inherit (den.lib.policy) resolve include provide;

  containerType =
    host:
    den.lib.schema.mkInstanceType {
      strict = true;
      extraModules = [
        (
          { name, config, ... }:
          {
            options = {
              # WHAT RUNS IT, which decides everything that differs: where its
              # content lands, which bridge it hangs off, how secrets get in.
              runtime = mkOption {
                type = types.enum (lib.attrNames runtimes);
                description = "`oci`: an image the host's docker runs, configured from the host. `nspawn`: a NixOS container, configured from inside.";
              };

              # A class, because `hasAspect` resolves per class and throws on
              # an entity with none. The host's: a container's content lands
              # in the host's configuration.
              class = mkOption {
                type = types.str;
                default = host.class;
                description = "The class this container's aspect tree resolves for -- its host's.";
              };

              # The machine it runs on, read by ./capability.nix's `machineOf`.
              # Structural rather than declared: it is the host it is listed
              # under.
              parent = mkOption {
                type = types.str;
                readOnly = true;
                default = host.name;
                description = "The host whose machine this container runs on.";
              };

              # Its own side of each wire, as a host states its own. A
              # container's differs from its host's: its end of the NAT bridge
              # is its own interface, not the bridge.
              attachments = mkOption {
                type = types.attrsOf (
                  types.submodule {
                    options = {
                      address = mkOption {
                        type = types.nullOr types.str;
                        default = null;
                        description = "Where other entities dial this one over that carrier.";
                      };
                      interface = mkOption {
                        type = types.nullOr types.str;
                        default = null;
                        description = "Where this entity hears that carrier.";
                      };
                    };
                  }
                );
                default = { };
                description = "This container's own attachment to each carrier.";
              };

              # The aspect that configures it, looked up by name exactly as a
              # host's is.
              aspect = mkOption {
                type = types.raw;
                default = den.aspects.${name} or { };
                defaultText = "den.aspects.<name>";
                description = "Aspect that configures this container.";
              };
            };

            # ITS SIDE OF THE BRIDGE ITS RUNTIME HANGS IT OFF, so no container
            # states one by hand.
            config.attachments = runtimes.${config.runtime}.attach host name;
          }
        )
      ];
    } den.schema.container;

  # ── THE TWO RUNTIMES ──
  # `aspect` joins the container's own scope; `attach` is its default side of
  # the bridge that aspect puts it on.
  runtimes = {
    oci = {
      aspect = den.aspects.oci-container;
      # docker hands the address out at runtime, so there is none to state.
      attach = _: _: { container-bridge.interface = lib.mkDefault "eth0"; };
    };
    nspawn = {
      aspect = den.aspects.nspawn-container;
      # Declared, so dialable: the next free address on the bridge, in name
      # order among this host's nspawn containers.
      attach =
        host: name:
        let
          bridge = den.aspects.nspawn-bridge;
          peers = lib.sort (a: b: a < b) (
            lib.attrNames (lib.filterAttrs (_: c: c.runtime == "nspawn") host.containers)
          );
          net = lib.init (lib.splitString "." (lib.head (lib.splitString "/" bridge.prefix)));
          index = lib.lists.findFirstIndex (n: n == name) 0 peers;
        in
        {
          nspawn-bridge = {
            interface = lib.mkDefault "eth0";
            address = lib.mkDefault (lib.concatStringsSep "." (net ++ [ (toString (index + 2)) ]));
          };
        };
    };
  };

  isNspawn = c: c.runtime == "nspawn";
  sharedOf = host: lib.filterAttrs (_: c: !(isNspawn c)) (host.containers or { });
  nspawnOf = host: lib.filterAttrs (_: isNspawn) (host.containers or { });

  # ── AN NSPAWN CONTAINER IS ITS OWN SYSTEM ──
  # Resolved as its own entity, as den's microvm guests are, and handed to the
  # host: its `nixos` content as the container's configuration, its `scoped`
  # content into the HOST's `age.scoped`, so the host decrypts its secrets and
  # the container only sees them bound in.
  treeOf = host: container: den.lib.resolveEntity "container" { inherit host container; };

  # One class run per container: its configuration's imports and, for
  # `capability.has`, its membership come from the same result.
  runs = lib.mapAttrs (
    _: h:
    lib.mapAttrs (_: c: {
      id = c.id_hash;
      run = den.lib.aspects.resolveWithPaths c.class (treeOf h c);
    }) (nspawnOf h)
  ) (lib.concatMapAttrs (_: hs: hs) den.hosts);

  systemOf =
    host: container:
    let
      tree = treeOf host container;
    in
    [
      (provide {
        class = host.class;
        path = [
          "containers"
          container.name
          "config"
        ];
        module = _: { inherit (runs.${host.name}.${container.name}.run) imports; };
      })
      # `age.scoped` is attrs of submodules, so the class modules are shifted
      # under it here: each scope collected as a deferred module and imported
      # into the host's scope of that name.
      (provide {
        class = host.class;
        module =
          args:
          let
            byScope =
              (lib.evalModules {
                modules = (den.lib.aspects.resolve "scoped" tree).imports ++ [
                  { freeformType = lib.types.lazyAttrsOf lib.types.deferredModule; }
                ];
                specialArgs = removeAttrs args [
                  "config"
                  "options"
                ];
              }).config;
          in
          {
            age.scoped = lib.mapAttrs (_: m: _: { imports = [ m ]; }) (removeAttrs byScope [ "_module" ]);
          };
      })
    ];

  scopeOf = container: lib.defaultTo container.name (container.aspect.secretScope or null);

  # ── WHAT A CONTAINER HANDS ITS HOST ──
  # `provides.to-hosts` anywhere in the container's static include closure.
  # den compiles `to-hosts` into a `{ host, user }` policy, which a container
  # scope never satisfies, so it is delivered here instead -- by the host, at
  # the host's own scope, with the container as the argument.
  toHostsOf =
    container:
    let
      named = a: builtins.isAttrs a && a ? name;
      closure = builtins.genericClosure {
        startSet =
          map
            (a: {
              key = a.name;
              inherit a;
            })
            (
              lib.filter named [
                container.aspect
                runtimes.${container.runtime}.aspect
              ]
            );
        operator =
          x:
          map (a: {
            key = a.name;
            inherit a;
          }) (lib.filter named (x.a.includes or [ ]));
      };
    in
    # Named, so the policy firing again from a user or container scope is
    # the same include rather than a second copy.
    map (x: den.lib.aspects.fx.contentUtil.applyProvide x.a.provides.to-hosts { inherit container; }) (
      lib.filter (x: (x.a.provides or { }) ? to-hosts) closure
    );
in
{
  # Each nspawn container's membership, from its own run, for `capability.has`.
  den.lib.container.pathSets = lib.mapAttrs (
    _: rs:
    lib.listToAttrs (
      lib.mapAttrsToList (
        _: x:
        lib.nameValuePair x.id (den.lib.aspects.fx.identity.flattenPathSetByScope x.run.pathSetByScope)
      ) rs
    )
  ) runs;

  den.schema.container = {
    parent = "host";
    inherits = [ den.schema.conf ];
    # ITS OWN LOOPBACK, which a container genuinely has. den injects
    # `den.default` -- where `loopback` comes from for a host -- only into the
    # host, user and home kinds, so a new kind starts with none; and the rest
    # of `den.default` is host configuration a container should not get.
    includes = [
      den.aspects.loopback
      # The class routes `den.default` would have brought, without its host
      # configuration: a container's `overlays` and `os` content still lands.
      den.policies.overlays-to-_overlays
      den.policies.os-to-host
      ({ container, ... }: runtimes.${container.runtime}.aspect)
    ];
  };

  # ── OCI: THE HOST RUNS IT, SO ITS CONTENT IS THE HOST'S ──
  # Its scope is shared into the host's configuration, which is where
  # `virtualisation.oci-containers` lives. Secrets are decrypted by the host's
  # agenix and handed in: the scope's `env` template as its environment, every
  # other template mounted read-only at the path it has on the host. Raw
  # secrets stay on the host; a template is what crosses.
  den.aspects.oci-container = {
    includes = [ den.aspects.container-bridge ];
    provides.to-hosts.includes = [ den.aspects.container-bridge ];
    nixos =
      { container, scoped, ... }:
      let
        templates = (scoped.${scopeOf container} or { }).templates or { };
        mounted = lib.attrValues (removeAttrs templates [ "env" ]);
      in
      {
        virtualisation.oci-containers.backend = lib.mkDefault "docker";
        virtualisation.oci-containers.containers.${container.name} = {
          # Every tcp port it gives, on the machine's loopback: where
          # ../network/container-bridge.nix tells the machine to dial it.
          ports = lib.mapAttrsToList (_: p: "127.0.0.1:${toString p}:${toString p}") (
            den.lib.capability.portsFor container container.aspect "tcp"
          );
          environmentFiles = lib.optional (templates ? env) templates.env.path;
          volumes = map (t: "${t.path}:${t.path}:ro") mounted;
        };
      };
  };

  # ── NSPAWN: A NIXOS SYSTEM OF ITS OWN, ON A DECLARED ADDRESS ──
  # The host declares `containers.<name>` on the bridge at the address the
  # container's attachment states; secrets are still decrypted by the host and
  # every template of the scope is bind-mounted read-only at its host path.
  # The bridge on the machine, once however many containers hang off it.
  den.aspects.nspawn-host.nixos =
    let
      bridge = den.aspects.nspawn-bridge;
    in
    {
      networking.bridges.${bridge.bridgeInterface}.interfaces = [ ];
      networking.interfaces.${bridge.bridgeInterface}.ipv4.addresses = [
        {
          address = bridge.gatewayAddress;
          prefixLength = lib.toInt (lib.last (lib.splitString "/" bridge.prefix));
        }
      ];
      # Out through the machine, so a container reaches what the host does.
      networking.nat = {
        enable = true;
        internalInterfaces = [ bridge.bridgeInterface ];
      };
    };

  den.aspects.nspawn-container = {
    includes = [
      den.aspects.nspawn-bridge
      # Declares `_overlays` for content that sets it; the nixpkgs is the host's.
      den.aspects.overlays
    ];

    # What the HOST declares: the container on the bridge, and the scope's
    # templates bound in at the paths they have on the host, which is also
    # what `scoped` reads inside.
    provides.to-hosts =
      { container, ... }:
      {
        includes = [
          den.aspects.nspawn-bridge
          den.aspects.nspawn-host
        ];
        nixos =
          {
            config,
            pkgs,
            scoped,
            ...
          }:
          let
            bridge = den.aspects.nspawn-bridge;
            templates = (scoped.${scopeOf container} or { }).templates or { };
            prefixLength = lib.last (lib.splitString "/" bridge.prefix);
          in
          {
            containers.${container.name} = {
              autoStart = true;
              privateNetwork = true;
              hostBridge = bridge.bridgeInterface;
              localAddress = "${container.attachments.nspawn-bridge.address}/${prefixLength}";
              bindMounts = lib.mapAttrs' (
                _: t:
                lib.nameValuePair (toString t.path) {
                  hostPath = toString t.path;
                  isReadOnly = true;
                }
              ) templates;
              config = {
                # The host's view, whose paths the bind mounts above make true
                # in here too.
                _module.args = lib.mapAttrs (_: lib.mkForce) {
                  inherit scoped;
                  inherit (config.age) secrets templates;
                };
                networking.defaultGateway = bridge.gatewayAddress;
                # The host's instance, overlays already applied: a second
                # nixpkgs per container is most of its eval.
                nixpkgs.pkgs = lib.mkForce pkgs;
                nixpkgs.overlays = lib.mkForce [ ];
                documentation.enable = false;
              }
              # The host decrypts; nothing in here holds a key.
              // lib.optionalAttrs (den.lib.capability.has container den.aspects.agenix-rekey) {
                age.enable = lib.mkForce false;
              };
            };
          };
      };
  };

  den.schema.host =
    { config, ... }:
    {
      options.containers = mkOption {
        type = types.attrsOf (containerType config);
        default = { };
        description = "Containers running on this host, each its own entity.";
      };
    };

  # ── HOST → CONTAINERS, AS `host-to-users` IS HOST → USERS ──
  # `resolve.shared` gives each container its own scope whose content is part
  # of the host's configuration; an entity-named key on the host's aspect is
  # included into that scope, as den does for a user.
  den.policies.host-to-containers =
    { host, ... }:
    let
      aspect = host.aspect;
      forwarded = lib.genAttrs (aspect.__providesForwarded or [ ]) (_: true);
      hasChild = name: builtins.isAttrs aspect && aspect ? ${name} && !(forwarded ? ${name});
    in
    lib.concatMap (
      container:
      [ (resolve.shared { inherit container; }) ]
      ++ lib.optional (hasChild container.name) (include aspect.${container.name})
    ) (lib.attrValues (sharedOf host));

  # ── WHAT EACH CONTAINER HANDS ITS HOST ──
  # Its own policy because an include beside a `resolve` lands in the scopes
  # that resolve opens, and only at the host's own scope: a policy fires
  # wherever `host` is bound, so a user or container scope would deliver it
  # again.
  den.policies.containers-to-host =
    {
      host,
      user ? null,
      container ? null,
      ...
    }:
    lib.optionals (user == null && container == null) (
      map include (lib.concatMap toHostsOf (lib.attrValues (host.containers or { })))
      ++ lib.concatMap (systemOf host) (lib.attrValues (nspawnOf host))
    );

  # A separate module only because `den.schema.host` is already defined above
  # as a function, and one attrset cannot also define a path beneath it.
  imports = [
    {
      den.schema.host.includes = [
        den.policies.host-to-containers
        den.policies.containers-to-host
      ];
    }
  ];
}
