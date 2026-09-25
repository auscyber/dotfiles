# A celler server as a NixOS container, built into a single OCI image by
# nix-oci (../services/celler-container.nix). Reachable on the tailnet as
# `celler2` -- which is what consumers use -- and publicly through a cloudflared
# tunnel at cache2.ivymect.in.
#
# Its only runtime input is its ssh host key, mounted at /keys. Setup, once:
#   ssh-keygen -t ed25519 -N "" -C celler2 -f ~/.ssh/celler2
#   # ~/.ssh/celler2.pub -> hosts/_registry.nix as celler2.hostPublicKey
#   agenix edit aspects/hosts/celler2/ts_authkey.age
#   agenix edit aspects/hosts/celler2/cf_token.age
#   nix run .#rekey
{
  den,
  inputs,
  lib,
  ...
}:
let
  registry = import ./_registry.nix;
  host = "celler2";
  key = "/keys/ssh_host_ed25519_key";
in
{
  den.hosts.x86_64-linux.${host} =
    lib.optionalAttrs (registry.${host} ? hostPublicKey) { inherit (registry.${host}) hostPublicKey; }
    // {
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

    secrets = {
      ts_authkey.rekeyFile = ./celler2/ts_authkey.age;
      cf_token.rekeyFile = ./celler2/cf_token.age;
    };

    templates.cloudflared =
      { secrets, ... }:
      {
        dependencies.cf_token = secrets.cf_token;
        content =
          { placeholders, ... }:
          ''
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
      in
      {
        config = lib.mkMerge [
          {
            boot.isContainer = true;
            networking.hostName = host;
            system.stateVersion = "25.11";

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
              cloudflared = {
                hostname = "cache2.ivymect.in";
                environmentFile = scoped.${host}.templates.cloudflared.path;
              };
              consumeVia = "tailscale";
            };
          }

          # Inside nix-oci's evaluation there is no systemd and no activation:
          # the image runs cellerd's unit as its entrypoint, so that is where
          # the secrets get decrypted and tailscaled/cloudflared start
          # alongside it.
          (lib.optionalAttrs (options ? oci) {
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
                --authkey="$(${decrypt "ts_authkey"})" --hostname=${host}

              TUNNEL_TOKEN="$(${decrypt "cf_token"})" \
                ${lib.getExe pkgs.cloudflared} tunnel --no-autoupdate run &
            '';
          })
        ];
      };
  };
}
