{
  den,
  lib,
  ...
}:
let
  celler = import ./_lib.nix { inherit lib; };
  inherit (den.lib.policy) pipe;

  # A scope's tokens: one per server it declared on `celler-use`, scoped to
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
      celler-caches,
      celler-use,
      ...
    }:
    let
      dir = age.rekey.generatedSecretsDir;
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
in
{
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

  # Every host substitutes from secondpc's main cache; a host that should not
  # excludes this aspect.
  den.aspects.celler-default.celler-use.secondpc.pull = [ "main" ];
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
    ];

    secrets = tokenSecrets;

    templates =
      {
        secrets,
        celler-caches,
        celler-use,
        ...
      }:
      let
        used = celler.use celler-caches celler-use;
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
        celler-caches,
        celler-use,
        ...
      }:
      let
        used = celler.use celler-caches celler-use;
        pushing = builtins.filter (c: c.push != [ ]) used;
        urls = lib.concatMap (c: map (cache: "${c.endpoint}/${cache}") (builtins.attrNames c.keys)) used;
        token = c: scoped.celler-host.secrets.${tokenOf c.name}.path;
        build-hook = pkgs.writeTextFile {
          name = "build-hook";
          executable = true;
          destination = "/bin/build-hook";
          text =
            # sh
            ''
              #!/bin/sh
              set -eu
              set -f # disable globbing
              export IFS=' '
              export PATH="$PATH:/nix/var/nix/profiles/default/bin:${pkgs.celler-client}/bin:${pkgs.ts}/bin"
              ${lib.concatMapStrings (c: ''
                celler login ${c.name} ${c.endpoint} "$(cat ${token c})"
              '') pushing}
              echo "Uploading paths" $OUT_PATHS
              if [[ -n "''${OUT_PATHS:-}" ]]; then
              	export TS_MAXFINISHED=1000
              	export TS_SLOTS=10
              ${lib.concatMapStrings (
                c:
                lib.concatMapStrings (cache: ''
                  printf "%s" "$OUT_PATHS" | xargs ts celler push ${c.name}:${cache}
                '') c.push
              ) pushing}
              fi
            '';
          meta.mainProgram = "build-hook";
        };
      in
      {
        nix.settings = lib.mkMerge [
          {
            substituters = urls;
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
    ];

    secrets = tokenSecrets;

    homeManager =
      {
        pkgs,
        config,
        scoped,
        celler-caches,
        celler-use,
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
                endpoint = "${c.endpoint}"
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
