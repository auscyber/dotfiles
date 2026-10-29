{
  den,
  lib,
  accessLib,
  ...
}:
# SplitPro (shared expenses). A container, because upstream ships a Next.js +
# Prisma app whose standalone build is not worth reproducing in nix for one
# service -- but the image is pulled into the store by nvfetcher
# (../../packages/splitpro.nix) rather than fetched at activation, so it is
# pinned by digest and a deploy never reaches a registry.
#
# It authenticates against kanidm through NextAuth's generic OIDC provider, so
# `oauthProxy = false` here too. NextAuth derives the callback path from the provider
# ID, which is `lowercase(OIDC_NAME)` -- so `OIDC_NAME=kanidm` is what makes the
# callback `/api/auth/callback/kanidm`, and it has to match `originUrl`.
#
# pg_cron is a hard requirement upstream (recurring expenses and cache cleanup),
# and it is the one thing here that reaches outside this file: it needs a
# `shared_preload_libraries` entry and names exactly ONE database server-wide.
# Both go through ./postgres.nix's quirk so the collision is visible rather than
# the last writer winning.
let
  # THE PORT, ONCE: `gives.tcp.ports.http` below is what ../proxy-nginx.nix
  # resolves the upstream from, and the container publishes the same number to
  # the host's loopback. They were the same 3000 in two places, and the second
  # one used to be implicit in `--network=host`.
  port = 3000;

  # The one container entity running this, across the fleet.
  self =
    lib.findFirst (c: c.name == "splitpro") (throw "splitpro: no host declares `containers.splitpro`")
      (
        lib.concatMap (h: lib.attrValues (h.containers or { })) (
          lib.attrValues (lib.concatMapAttrs (_: hs: hs) den.hosts)
        )
      );

  # Where postgres is, dialled from the container entity.
  db =
    container:
    let
      d = den.lib.capability.dial "tcp" { name = "postgres"; } container;
    in
    "${d.address}:${toString d.port}";
in
{
  den.aspects.splitpro = {
    # A CONTAINER ENTITY (`den.hosts.<sys>.secondpc.containers.splitpro`,
    # runtime `oci`), so the host's gateway, postgres and restic serve it from
    # the host, and its wire to them is the runtime's bridge.
    includes = [
      den.aspects.agenix-rekey
      den.aspects.packages.splitpro
    ];

    # What access this service exposes, for ../persons/<name>.nix to be granted.
    # Collected by ./access.nix, which turns a grant into a kanidm group and
    # adds that group to this service's own `gated` groups.

    takes.gated = {
      oauthProxy = false;
    };

    # ── THE ACCOUNT, FURNISHED ──
    #
    # A container, so there is no unix user of its own on this host: the
    # environment file is rendered by agenix as ROOT and handed to the container,
    # and the database ROLE is `splitpro` (see the `DATABASE_URL` below). So the
    # account this furnishes is what the role is named after and what owns this
    # side's copy of the edge's password -- root, because root is who reads it.
    #
    # ../base/capability-types.nix's `credential` resolves ownership through
    # this, which is why it has to exist: without it the realisation throws
    # rather than defaulting, since an owner falling back silently is a secret
    # readable by nothing that needs it.
    gives =
      { self, ... }:
      {
        # SplitPro has no role model -- you are in or you are out -- so the
        # one level is the whole character of a grant here.
        service = _: {
          access = {
            levels = [ "member" ];
            defaultLevel = "member";
          };
        };

        # A TILE, BECAUSE A GRANT IS MEANT TO BE VISIBLE. ../households/
        # ivymectin.nix grants this to every resident, and ./homepage.nix
        # derives a tile's audience from those same grants -- so without a
        # widget here the grant authorised people and showed them nothing.
        #
        # No `href`: this is published through the gateway, so ./homepage.nix
        # derives the url from `gateway.services.<n>.url`. Stating one is for
        # a plain vhost (../hosts/secondpc/plex.nix says why).
        homepage-widget = _: {
          group = "Productivity";
          icon = "splitwise.svg";
          description = "Splitting bills";
        };

        # WHAT IT LISTENS ON, so ../proxy-nginx.nix can ASK rather than
        # assume: the upstream is resolved by composing this with the carrier
        # the two ends share. Not exposed -- the gate is the way in.
        tcp = _: {
          name = self.name;
          ports.http = {
            inherit port;
            meta.description = "Splitpro's Next.js server, published on the host's loopback by its container runtime; the gate dials it there.";
          };
        };

        user = _: {
          # The database ROLE, which `DATABASE_URL` below names.
          name = "splitpro";
          # A CONTAINER, so no unix account of its own here -- and the edge's
          # password is consumed through an age TEMPLATE, which agenix renders
          # as root. So root owns this side's copy while the role is still
          # `splitpro`.
          owner = "root";
          carrier = self.name;
        };
      };

    takes.postgres = {
      # The container's uid is the image's, not the host's, so peer auth over
      # the socket cannot work -- it authenticates the CONNECTING process's
      # uid. TCP with a password is the honest option.
      tcp = true;

      # UNIONED into the server's extension list and
      # `shared_preload_libraries`; `settings` is server-GLOBAL, so a second
      # taker naming a different `cron.database_name` is a throw rather than a
      # merge (../base/capability-types.nix's `exclusive`).
      extensions = [ "pg_cron" ];
      preload = [ "pg_cron" ];
      settings."cron.database_name" = "splitpro";
      # The backend-prefixed unit the oci-containers module generates.
      services = [ "docker-splitpro" ];
    };

    # The expense rows are in postgres and covered by ./restic.nix's dump of
    # every provisioned database. These are the receipt IMAGES, which exist
    # nowhere else -- the bind mount below is already "in one predictable place
    # for backups", and this is the backup it was placed there for.
    backup = _: {
      name = "splitpro";
      paths = [ "/var/lib/splitpro/uploads" ];
    };

    secrets = {
      oidc-client-secret = {
        owner = "kanidm";
        restartUnits = [ "kanidm.service" ];
        generator.script = "random-hex-32";
      };

      db-password.generator.script = "random-hex-32";

      # NextAuth signs its session JWTs with this. Upstream's own instruction is
      # `openssl rand -base64 32`, so base64 rather than hex.
      nextauth-secret.generator.script = "random-base64-32";
    };

    # The container reads its whole configuration from here, which keeps the db
    # password and both secrets out of the store and out of `docker inspect`.
    # The zone, read from the aspect that declares it -- see ./donetick.nix
    # for why neither `content` nor this body can reach `config.gateway.domain`.
    # Dialled from this container's own entity, where the process runs. A
    # template body in a container's scope is handed no entity at all, so it
    # is found by name.
    templates = {
      env =
        { secrets, ... }:
        {
          dependencies = {
            inherit (secrets) oidc-client-secret db-password nextauth-secret;
          };
          content =
            { placeholders, ... }:
            let
              domain = den.aspects.publish-ivymect-in.domain;
            in
            ''
              DATABASE_URL=postgresql://splitpro:${placeholders.db-password}@${db self}/splitpro
              NEXTAUTH_SECRET=${placeholders.nextauth-secret}
              NEXTAUTH_URL=https://splitpro.${domain}
              OIDC_NAME=kanidm
              OIDC_CLIENT_ID=splitpro
              OIDC_CLIENT_SECRET=${placeholders.oidc-client-secret}
              OIDC_WELL_KNOWN_URL=https://auth.${domain}/oauth2/openid/splitpro/.well-known/openid-configuration
            '';
        };
    };

    # `kanidmClients.<name>`: the shared OIDC shape (short username, scope
    # maps, landing page, the domain) is defaulted by `gateway.oidcClients` in
    # ./gateway.nix, so this is only what differs. The name is written once
    # here; keying it off the aspect is not possible (see that option's note),
    # and keying it off the service entity waits on `{ service, ... }` binding.
    kanidmClients.splitpro = {
      displayName = "SplitPro";
      callback = "/api/auth/callback/kanidm";
      # No `groupsName`: SplitPro has no role model -- you are in or you are
      # out, and the scope map is the whole gate.
      # ── AND WHO MAY HAVE A TOKEN AT ALL, WHICH IS THE GRANT ──
      #
      # `scopes` REPLACES the `media-users` default (./gateway.nix), so this
      # client issues a token only to the groups a grant on splitpro produces.
      # That is the narrowing the comment above already claimed: "the scope map
      # is itself the gate" was true of a scope EVERY client shared, so being
      # in `media-users` bought all of them at once and a grant on one bought
      # nothing.
      #
      # ./access.nix owns the spelling and answers with the GRANTED levels
      # rather than the declared ones -- a scope map naming a group nobody was
      # granted is a group kanidm never provisioned, and the provisioning run
      # fails on it.
      scopes = accessLib.groupsFor "splitpro";
    };

    nixos =
      { pkgs, ... }:
      {
        virtualisation.oci-containers.containers.splitpro = {
          # `image` must spell the same name:tag the tarball carries or the
          # backend pulls over the network regardless -- the lesson already
          # written down in ./prowlarr.nix.
          imageFile = pkgs.splitpro-image;
          image = "docker.io/ossapps/splitpro:${pkgs.splitpro-image.imageTag}";

          # Environment, published port and network come from its runtime
          # (../base/container.nix): the `env` template, every `tcp` port on
          # the loopback, docker's default bridge.
          #
          # Receipts live here. A bind mount rather than a docker volume so the
          # data is in one predictable place for backups.
          volumes = [ "/var/lib/splitpro/uploads:/app/uploads" ];

          # Migrations run inside the image's own entrypoint (`prisma migrate
          # deploy` before the node server starts), so the only ordering that
          # matters is that the database and its role password exist first --
          # which the unit override below asserts.
        };

        systemd.tmpfiles.rules = [
          "d /var/lib/splitpro       0750 root root -"
          "d /var/lib/splitpro/uploads 0750 root root -"
        ];
      };
  };
}
