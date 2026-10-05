# A celler server as a NixOS container, built into a single OCI image by
# nix-oci (../services/celler-container.nix). Reachable on the tailnet as
# `celler2` -- which is what consumers use -- and publicly through a cloudflared
# tunnel at cache2.ivymect.in.
#
# Its only runtime input is its ssh host key, mounted at /keys. Setup, once:
#   ssh-keygen -t ed25519 -N "" -C celler2 -f ~/.ssh/celler2
#   # ~/.ssh/celler2.pub -> `hostPublicKey` on den.hosts.x86_64-linux.celler2 below
#   nix run .#gen-secrets   # mints ts_authkey off the shared tailscale OAuth
#                           # client and cf_token off the cloudflare API token,
#                           # creating the tunnel as it goes
#   nix run .#rekey
#   nix run .#sync-tunnels -- celler2   # CNAME for cache2.ivymect.in
{
  den,
  inputs,
  lib,
  cellerCapability,
  ...
}:
let
  host = "celler2";
  key = "/keys/ssh_host_ed25519_key";
  tunnelHostname = "cache2.ivymect.in";
  # The default in ../base/celler/server.nix's `expose`; named here because the
  # tunnel's ingress has to point at the same port.
  cellerPort = 8080;
in
{
  den.hosts.x86_64-linux.${host} = {
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP1gJPPu6Z4iOtWWYLBulSwmCYouYd8k26bHNlt0WEtf celler2";

    # ── NO TUN, SO NO INTERFACE, SAID WHERE THE CARRIER CAN READ IT ──
    #
    # `services.tailscale.interfaceName` below is this same fact in this host's
    # CONFIG, and the capability layer cannot read that -- ../network/tailscale.nix:
    # "reading it back off the host's config would mean evaluating that host ...
    # several GB for one string". So the entity carries it too, and the carrier
    # furnishes `userspace-networking` rather than `tailscale0` for this host.
    #
    # No `address`: a `100.x` is assigned by the coordination server and is not
    # knowable at eval time, which is why the tailnet carrier addresses its
    # members by MagicDNS name instead.
    attachments.tailscale.interface = "userspace-networking";
    # Still a nixosSystem, so den and deploy tooling see an ordinary host;
    # nix-oci re-evaluates the same modules into the image.
    instantiate =
      args:
      inputs.nixpkgs.lib.nixosSystem args
      // {
        ociModules = args.modules ++ [ { _module.args = args.specialArgs or { }; } ];
      };
  };

  # ── celler2 IS A CELLER, SAID SO ──
  # No `takes.proxy`: this container publishes through its own cloudflared
  # ingress below, not through an nginx that is not on this box. Everything else
  # is the same shape secondpc's celler has, at the port this one serves.
  den.aspects.celler-container = cellerCapability {
    port = cellerPort;
    subdomain = "cache2";
  };

  den.aspects.${host} = {
    includes = [
      den.aspects.celler-server._.nixos
      # THE CACHE THIS CONTAINER SERVES, DECLARED. Its `celler-caches` record
      # already said `main` -- by falling back to `[ "main" ]` for a host that
      # declares no cache aspect -- so the fact was real and only the
      # declaration was missing, which the capability chain cannot infer: a
      # consumer asking for this celler's cache `main` finds nothing to resolve
      # against unless an aspect here gives it.
      #
      # That gap is what dropped the laptop's and auspc's pushes the moment
      # their hand-written `celler-use` became a take. It has no signing key in
      # ../base/celler/celler-keys.json yet, which is why `caches` keeps the
      # key-derived fallback and does not throw.
      #
      # `cache-main` and NOT `celler-main`: the latter also brings
      # `den.aspects.celler`, whose `expose.port` is 8069 and conflicts with the
      # 8080 this container serves on.
      den.aspects.cache-main

      # AND THE CELLER ITSELF, which is the root the cache hangs beneath. Its
      # own declaration rather than `den.aspects.celler`, for that same port:
      # the capability half is parametric (../base/celler/server.nix) so this
      # states the number it actually serves on.
      den.aspects.celler-container

      # ── ON THE TAILNET, SAID SO ──
      #
      # The header has always claimed "reachable on the tailnet as `celler2`",
      # and the preStart below really does run `tailscale up`. But membership in
      # den is INCLUSION -- ../network/tailscale.nix: "a host that includes
      # `den.aspects.tailscale` is on the tailnet, and there is nothing to
      # declare and nothing that can disagree with reality". Without this line
      # reality disagreed: `providersFor celler2 "tcp"` was
      # `[ celler-container, loopback ]` with NO segment, so `linksBetween`
      # found no route to this host from anywhere -- while `resolveChain` still
      # offered `pull/main/celler@celler2` to all eleven hosts.
      den.aspects.tailscale
    ];

    # This container serves a cache; it does not consume one. The fleet-wide
    # default (../base/celler/push.nix) would have it substitute from secondpc
    # and mint itself a token to do so, which for a single-purpose image means
    # a credential and a netrc it never reads -- and it would reach secondpc
    # over the public internet to get there.
    excludes = [ den.aspects.celler-default ];

    secrets = _: {
      # The two fleet credentials the generators below mint from, under the
      # names those generators look for (`auth` and `api`). Declaring them
      # again is the point rather than a duplicate: agenix-rekey keys on the
      # file, so these are the same two secrets ../network/tailscale.nix and
      # ../network/cloudflare.nix declare for the systems that include those
      # aspects -- which this container does not, having no boot-time daemon
      # and no nginx to put behind a tunnel.
      #
      # `intermediary`, so neither reaches the image: only the per-host key
      # and the connector token minted from them do.
      auth = {
        rekeyFile = ../../secrets/static/tailscale_auth.age;
        intermediary = true;
      };
      api = {
        rekeyFile = ../../secrets/static/cloudflare_api.age;
        intermediary = true;
      };

      # NO `ts_authkey` HERE ANY MORE: `den.aspects.tailscale` is included
      # above, and it declares this same secret in its own scope. Declaring it
      # here too made one logical secret into two -- identical generator and
      # settings, so identical content hash, differing only in the scope
      # directory:
      #
      #   secrets/generated/celler2/celler2/ts_authkey-4d017270.age   (this one)
      #   secrets/generated/celler2/tailscale/ts_authkey-4d017270.age (the aspect's)
      #
      # agenix requires every DECLARED secret's rekeyFile to exist, so keeping
      # both would mean minting and rekeying two keys for one tailnet node.

      # Likewise minted from the account API token, which creates the tunnel
      # too. See ../network/cloudflare.nix. Not `den.aspects.cloudflare-tunnel`:
      # that one derives its ingress from nginx, and this container has
      # cellerd and nothing else -- the ingress below is written by hand for
      # the same reason.
      cf_token = den.lib.cloudflare.tunnelSecret { tunnel = host; };
    };

    templates.cloudflared = { secrets, ... }: {
      dependencies.cf_token = secrets.cf_token;
      content = { placeholders, ... }: ''
        TUNNEL_TOKEN=${placeholders.cf_token}
      '';
    };

    nixos =
      {
        config,
        options,
        pkgs,
        scoped,
        ...
      }:
      let
        # SCOPE-AWARE, because the secrets are no longer all in one scope:
        # `ts_authkey` belongs to `den.aspects.tailscale` now, and
        # lib/age-scoped.nix keys `age.secrets` by `<scope>/<name>`.
        decryptIn =
          scope: name: "${lib.getExe pkgs.rage} -d -i ${key} ${config.age.secrets."${scope}/${name}".file}";
        decrypt = decryptIn host;
        tailscale = config.services.tailscale.package;

        # The tunnel is `config_src: "local"` (../network/cloudflare.nix), so
        # the connector takes its routes from here rather than from the
        # account. One rule, because this container serves one thing.
        cloudflaredConfig = (pkgs.formats.yaml { }).generate "cloudflared.yml" {
          ingress = [
            {
              hostname = tunnelHostname;
              service = "http://localhost:${toString cellerPort}";
            }
            { service = "http_status:404"; }
          ];
        };
      in
      {
        config = lib.mkMerge [
          {
            boot.isContainer = true;
            networking.hostName = host;

            age.identityPaths = [ key ];

            # No tun device or NET_ADMIN in a container. Inbound tailnet
            # connections are forwarded to localhost, which is how the
            # tailscale address reaches cellerd.
            services.tailscale = {
              enable = true;
              interfaceName = "userspace-networking";
              # `scoped.tailscale`, not `scoped.${host}`: the secret is the
              # aspect's now, see the note where this host's `secrets` are.
              authKeyFile = scoped.tailscale.secrets.ts_authkey.path;
              extraUpFlags = [ "--hostname=${host}" ];
            };

            services.cellerd.expose = {
              port = cellerPort;
              cloudflared = {
                hostname = tunnelHostname;
                environmentFile = scoped.${host}.templates.cloudflared.path;
              };
              consumeVia = "tailscale";
            };
          }

          # Inside nix-oci's evaluation there is no systemd and no activation:
          # the image runs cellerd's unit as its entrypoint, so that is where
          # the secrets get decrypted and tailscaled/cloudflared start
          # alongside it.
          # `options.oci ? container`, not `options ? oci`: the plain
          # nixosSystem evaluation also has an `oci` (diskSize/efi, from the
          # image modules), so the coarser test let this block through to a
          # config with no nix-oci in it.
          (lib.optionalAttrs (options.oci or { } ? container) {
            oci.container.extraPackages = [
              pkgs.rage
              tailscale
              pkgs.cloudflared
            ];
            systemd.services.cellerd.preStart = lib.mkBefore ''
              CELLER_SERVER_TOKEN_RS256_SECRET_BASE64="$(${lib.getExe pkgs.rage} -d -i ${key} ${
                config.age.secrets."celler/signing_key".file
              })"
              export CELLER_SERVER_TOKEN_RS256_SECRET_BASE64

              mkdir -p /var/lib/tailscale /run/tailscale
              ${tailscale}/bin/tailscaled \
                --state=/var/lib/tailscale/tailscaled.state \
                --socket=/run/tailscale/tailscaled.sock \
                --tun=userspace-networking &
              until [ -S /run/tailscale/tailscaled.sock ]; do sleep 0.2; done
              ${tailscale}/bin/tailscale --socket=/run/tailscale/tailscaled.sock up \
                --authkey="$(${decryptIn "tailscale" "ts_authkey"})" \
                --advertise-tags=${lib.escapeShellArg den.lib.tailscale.nodeTag} \
                --hostname=${host}

              TUNNEL_TOKEN="$(${decrypt "cf_token"})" \
                ${lib.getExe pkgs.cloudflared} tunnel \
                  --no-autoupdate --config ${cloudflaredConfig} run &
            '';
          })
        ];
      };
  };
}
