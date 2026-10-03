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
    # Still a nixosSystem, so den and deploy tooling see an ordinary host;
    # nix-oci re-evaluates the same modules into the image.
    instantiate =
      args:
      inputs.nixpkgs.lib.nixosSystem args
      // {
        ociModules = args.modules ++ [ { _module.args = args.specialArgs or { }; } ];
      };
  };

  den.aspects.${host} = {
    includes = [ den.aspects.celler-server._.nixos ];

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
        rekeyFile = ../../secrets/tailscale_auth.age;
        intermediary = true;
      };
      api = {
        rekeyFile = ../../secrets/cloudflare_api.age;
        intermediary = true;
      };

      # Minted from the fleet's shared OAuth client at `gen-secrets` time --
      # there is no per-host authkey to write by hand. See ../network/tailscale.nix.
      ts_authkey = den.lib.tailscale.authKeySecret host;

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
        decrypt = name: "${lib.getExe pkgs.rage} -d -i ${key} ${config.age.secrets."${host}/${name}".file}";
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
              authKeyFile = scoped.${host}.secrets.ts_authkey.path;
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
                --authkey="$(${decrypt "ts_authkey"})" \
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
