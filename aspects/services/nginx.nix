{
  lib,
  den,
  ...
}:
let
  nginxVhosts =
    {
      class,
      aspect-chain,
    }:
    den.batteries.forward {
      each = lib.singleton true; # single forward; item ignored
      fromClass = _: "vhosts"; # the custom class you write into
      intoClass = _: "nixos"; # services.nginx.virtualHosts is a NixOS option
      intoPath = _: [
        "services"
        "nginx"
        "virtualHosts"
      ];
      fromAspect = _: lib.head aspect-chain;
      adaptArgs = lib.id;
    };
in
{
  den.aspects.nginx = {
    includes = [
      nginxVhosts
      den.aspects.packages.nginx-otel-module
    ];
    nixos =
      { pkgs, lib, ... }:
      {
        security.acme = {
          acceptTerms = true;
          defaults.email = "ivyp@outlook.com.au";
          defaults.dnsProvider = "cloudflare";
        };

        services.nginx = {
          enable = true;
          recommendedProxySettings = true;
          recommendedTlsSettings = true;
          recommendedOptimisation = true;

          # The one thing that can't live in the module itself
          # (packages/_nginx-otel-module.nix has the rest: cmake options,
          # extra buildInputs): cmake's setup-hook silently swaps nginx's
          # OWN configurePhase for a plain `cmake .` the moment `cmake` is
          # anywhere in its build inputs -- which it now is, via the
          # module's own buildInputs, folded in by nginx's `mapModules
          # "buildInputs"`. That hook's guard
          # (`-z "${dontUseCmakeConfigure-}" -a -z "${configurePhase-}"`)
          # is checked while environment/setup-hooks are being sourced,
          # before any phase (including a module's `preConfigure`) has run
          # -- so only an attr on nginx's OWN derivation is visible in time
          # to stop it; nginx isn't cmake-based, it just needed cmake
          # sitting in its inputs for this one module's sake.
          package = pkgs.nginx.overrideAttrs (_: {
            dontUseCmakeConfigure = true;
          });

          # nginx-otel has no static build path (see
          # packages/_nginx-otel-module.nix, which also carries the cmake
          # options and extra buildInputs its `--add-dynamic-module` build
          # needs) -- `additionalModules` is what nixpkgs' PR #537190 added
          # dynamic-module support for.
          additionalModules = with pkgs.nginxModules; [ otel ];

          # Traces every request nginx handles to tempo, loopback and
          # plaintext like kanidm's and ncps's OTLP export.
          commonHttpConfig = ''
            otel_exporter {
              endpoint 127.0.0.1:4317;
            }
            otel_service_name nginx;
            otel_trace on;
            otel_trace_context propagate;
            # Default is just the matched location's name, which is a
            # useless "/" for every gated/vhost proxy_pass location here
            # (see gateway.nix) -- domain + method + path tells spans apart
            # instead. $host leads rather than trails: one nginx fronts
            # every vhost here, so which domain a span belongs to is the
            # first thing worth seeing in a trace list, not something to
            # dig for in attributes.
            otel_span_name "$host $request_method $uri";
            # Also as its own attribute -- span NAME is free-text and only
            # good for reading, not for filtering/grouping a search by
            # domain the way an attribute is.
            otel_span_attr "http.host" "$host";
            # Who made this request, covering both ways the gateway
            # recognises a caller (see gateway.nix): a human session, where
            # oauth2-proxy's own nginx integration sets $email via
            # auth_request_set once SSO passes, and a machine/API-key
            # caller, where $gw_caller is the name gateway.nix's own
            # `gateway/callers.map` resolves that key to. Exactly one of
            # the two is non-empty on any given authenticated request; both
            # are empty on a request that never authenticated at all (a
            # public location, or one that got a 401/403 before either
            # variable was set).
            otel_span_attr "user.email" "$email";
            otel_span_attr "user.caller" "$gw_caller";
          '';
        };

        # agenix restarts a secret's `restartUnits` -- nginx included, for
        # every gateway map/template -- before it chowns the freshly
        # decrypted files to their configured owner (chown deliberately
        # waits on NixOS's own `users`/`groups` activation finishing first,
        # in case the owner is being created in this same switch). nginx's
        # own config-test pre-start hook can lose that race and fail to
        # open a template that's still root-only, and systemd's bare
        # defaults (StartLimitBurst=5 in 10s) don't give it enough
        # attempts to outlast the gap -- observed taking up to ~2 minutes
        # on a busy switch. This makes it self-heal instead of dying
        # start-limit-hit.
        # nginx's own module already sets Restart=always/RestartSec=10s and
        # its own startLimit*; mkForce all three rather than a plain
        # override, or eval fails on the conflicting definitions. Shorter
        # RestartSec + a bigger budget fits more attempts into the same
        # (or a longer) window.
        systemd.services.nginx.serviceConfig.RestartSec = lib.mkForce "2s";
        systemd.services.nginx.startLimitIntervalSec = lib.mkForce 120;
        systemd.services.nginx.startLimitBurst = lib.mkForce 30;
      };
  };
}
