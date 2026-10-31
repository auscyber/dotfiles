{
  den,
  lib,
  caps,
  rootPath,
  ...
}:
# NFSv4 over mutual TLS (RPC-with-TLS, `xprtsec=mtls`). One client certificate
# per (client, server) link, minted the way builders.nix mints its ssh keys;
# the server presents one certificate of its own.
let
  nfsPort = 2049;

  serverCertFile = host: rootPath + "/secrets/generated/${host.name}/nfs-server/nfs-server.crt";

  readPem = p: lib.optionalString (builtins.pathExists p) (builtins.readFile p);

  sidecar =
    config: r:
    lib.removeSuffix ".age" (toString config.age.secrets."${r.scope}/${r.taker.key}".rekeyFile)
    + ".crt";

  realiseLink =
    {
      taker,
      server,
      client,
    }:
    den.lib.capability.realise "nfs" {
      giver = den.aspects.nfs-server;
      inherit taker;
      host = server;
      peer = client;
      giverHost = server;
      takerHost = client;
    };

  tlshd =
    {
      pkgs,
      section,
      credentials,
      truststore,
    }:
    {
      system.services.tlshd = {
        imports = [ pkgs.ktls-utils.services.default ];
        tlshd.settings.${section} = {
          "x509.certificate" = "/run/credentials/tlshd.service/certificate";
          "x509.private_key" = "/run/credentials/tlshd.service/private_key";
          "x509.truststore" = toString truststore;
        };
        systemd.service.serviceConfig.LoadCredential = lib.mapAttrsToList (n: p: "${n}:${p}") credentials;
      };
      boot.kernelModules = [ "tls" ];
    };
in
{
  den.capabilities.nfs = {
    meta.description = "An NFSv4 export over mutual TLS. A narrowing of tcp carrying a per-link client certificate.";
    side = "endpoint";
    includes = [
      caps.tcp
      caps.credential
    ];

    options.serverCertificate = lib.mkOption {
      type = lib.types.str;
      description = "PEM of the certificate the server presents; the client's tlshd truststore.";
    };

    # Keyed by path, mounted at the same path on the client: ClusterPlex and
    # anything like it needs the two sides to agree on paths.
    taker.options.mounts = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.enum [
          "ro"
          "rw"
        ]
      );
      default = { };
    };
  };

  den.aspects.agenix-rekey.age.generators.x509_pair =
    {
      lib,
      file,
      pkgs,
      secret,
      ...
    }:
    let
      settings = secret.settings or { };
      cn = settings.cn or (lib.removeSuffix ".age" (baseNameOf file));
      san = settings.san or [ ];
    in
    ''
      dir=$(mktemp -d)
      trap 'rm -rf "$dir"' EXIT
      ${pkgs.openssl}/bin/openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 \
        -nodes -days 36500 -subj ${lib.escapeShellArg "/CN=${cn}"} \
        ${
          lib.optionalString (
            san != [ ]
          ) "-addext ${lib.escapeShellArg "subjectAltName=${lib.concatStringsSep "," san}"}"
        } \
        -keyout "$dir/key" -out "$dir/crt" >&2 2>&1
      cat "$dir/crt" > ${lib.escapeShellArg (lib.removeSuffix ".age" file + ".crt")}
      cat "$dir/key"
    '';

  den.aspects.nfs-server = {
    includes = [ den.aspects.agenix-rekey ];
    reach = "household";

    secrets =
      { host, ... }:
      {
        nfs-server = {
          generator.script = "x509_pair";
          settings = {
            cn = host.name;
            san = [
              "DNS:${host.name}"
            ]
            ++ map (a: "IP:${a}") (
              lib.filter (a: a != null) (lib.mapAttrsToList (_: a: a.address or null) (host.attachments or { }))
            );
          };
        };
      };

    gives =
      {
        self,
        host ? { },
        ...
      }:
      {
        user = _: {
          name = "root";
          group = "root";
          carrier = self.name;
        };

        tcp = _: {
          name = self.name;
          ports.nfs = {
            port = nfsPort;
            meta.description = "NFSv4, TLS-only (xprtsec=mtls); mounted by household hosts that take `nfs`.";
          };
        };

        nfs =
          {
            peer ? { },
            ...
          }:
          {
            name = self.name;
            port = nfsPort;
            ports.nfs.meta.description = "NFSv4 over mutual TLS.";
            scope = "nfs-${peer.name or "unknown"}-${host.name or "unknown"}";
            secretName = "nfs-client";
            generator = "x509_pair";
            giverReads = "public";
            serverCertificate = readPem (serverCertFile host);
          };
      };

    nixos =
      {
        config,
        pkgs,
        scoped,
        host,
        ...
      }:
      let
        edges = map (
          e:
          let
            d = den.lib.capability.dial "nfs" (e.aspect.takes.nfs or { }) e.host;
          in
          {
            inherit e;
            address = if d == null then null else e.host.attachments.${d.carrier.name or ""}.address or null;
            r = realiseLink {
              taker = e.aspect;
              server = host;
              client = e.host;
            };
          }
        ) (den.lib.capability.takerEdgesReaching den.aspects.nfs-server "nfs" host);

        reachable = lib.filter (x: x.address != null) edges;

        exportsByPath = lib.zipAttrs (
          lib.concatMap (
            x:
            lib.mapAttrsToList (path: access: {
              ${path} = "${x.address}(${access},sync,no_subtree_check,root_squash,xprtsec=mtls)";
            }) (x.e.aspect.takes.nfs.mounts or { })
          ) reachable
        );

        truststore = pkgs.writeText "nfs-client-certs.pem" (
          lib.concatMapStrings (x: readPem (sidecar config x.r)) edges
        );
      in
      lib.mkMerge [
        {
          warnings = map (
            x:
            "nfs-server: ${x.e.name} on ${x.e.host.name} has no address on the carrier it reaches ${host.name} over, so it gets no export."
          ) (lib.filter (x: x.address == null) edges);

          age.scoped = lib.mkMerge (map (x: { ${x.r.scope}.secrets = x.r.giverSecrets; }) edges);

          services.nfs.server = {
            enable = true;
            exports = lib.concatStrings (
              lib.mapAttrsToList (
                path: clients: "${path} ${lib.concatStringsSep " " (lib.unique clients)}\n"
              ) exportsByPath
            );
          };
          services.nfs.settings.nfsd = {
            vers3 = false;
            "vers4.0" = false;
          };
        }
        (tlshd {
          inherit pkgs truststore;
          section = "authenticate.server";
          credentials = {
            certificate = pkgs.writeText "nfs-server.crt" (readPem (serverCertFile host));
            private_key = scoped.nfs-server.secrets.nfs-server.path;
          };
        })
      ];
  };

  den.aspects.nfs-client.nixos =
    {
      config,
      pkgs,
      scoped,
      host,
      ...
    }:
    let
      links = lib.concatMap (
        taker:
        let
          d = den.lib.capability.dial "nfs" (taker.takes.nfs or { }) host;
        in
        lib.optional (d != null) {
          inherit taker d;
          r = realiseLink {
            inherit taker;
            server = d.host;
            client = host;
          };
        }
      ) (lib.attrValues (den.lib.capability.takersOn host "nfs"));

      servers = lib.unique (map (l: l.d.host.name) links);
      first = lib.head links;
    in
    lib.mkIf (links != [ ]) (
      lib.mkMerge [
        {
          assertions = [
            {
              assertion = lib.length servers == 1;
              message = "nfs-client: ${host.name} takes nfs from ${lib.concatStringsSep ", " servers}; tlshd presents one client certificate, so only one server is supported.";
            }
          ];

          boot.supportedFilesystems = [ "nfs" ];

          age.scoped = lib.mkMerge (map (l: { ${l.r.scope}.secrets = l.r.takerSecrets; }) links);

          fileSystems = lib.mkMerge (
            map (
              l:
              lib.mapAttrs (path: access: {
                device = "${l.d.address}:${path}";
                fsType = "nfs4";
                options = [
                  "xprtsec=mtls"
                  access
                  "x-systemd.automount"
                  "noauto"
                  "nofail"
                  "_netdev"
                  "x-systemd.idle-timeout=600"
                ];
              }) (l.taker.takes.nfs.mounts or { })
            ) links
          );
        }
        (tlshd {
          inherit pkgs;
          section = "authenticate.client";
          truststore = pkgs.writeText "nfs-server-cert.pem" first.d.link.serverCertificate;
          credentials = {
            certificate = pkgs.writeText "nfs-client.crt" (readPem (sidecar config first.r));
            private_key = first.r.taker.pathIn scoped;
          };
        })
      ]
    );
}
