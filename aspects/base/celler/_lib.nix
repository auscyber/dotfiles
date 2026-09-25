{ lib }:
rec {
  # The servers and caches a scope declared on `celler-use`, picked out of
  # every server's `celler-caches` record. A declaration is
  #   { <server> = [ <cache> ... ]; }                     pull and push
  #   { <server> = { pull = [ ... ]; push = [ ... ]; }; }
  # and every aspect's declarations merge. Each result is the server's record
  # with `pull` (everything it may read, pushes included), `push`, and `keys`
  # narrowed to `pull`. Naming a server that does not exist is an error.
  use =
    celler-caches: celler-use:
    let
      norm =
        v:
        if builtins.isList v then
          {
            pull = v;
            push = v;
          }
        else
          {
            pull = [ ];
            push = [ ];
          }
          // v;
      merged = builtins.zipAttrsWith (
        _: vs:
        let
          ns = map norm vs;
          push = lib.unique (lib.concatMap (n: n.push) ns);
        in
        {
          inherit push;
          pull = lib.unique (lib.concatMap (n: n.pull) ns ++ push);
        }
      ) celler-use;
      servers = builtins.listToAttrs (map (c: lib.nameValuePair c.name c) celler-caches);
    in
    lib.mapAttrsToList (
      name: want:
      let
        server =
          servers.${name}
            or (throw "celler-use: no celler server `${name}` (servers: ${toString (builtins.attrNames servers)})");
      in
      server
      // want
      // {
        keys = lib.getAttrs (builtins.filter (c: server.keys ? ${c}) want.pull) server.keys;
      }
    ) merged;

  # Every server signs its tokens with its own RS256 key, generated once and
  # kept master-encrypted at secrets/generated/<server>/. The server deploys
  # it; a consumer declares the same secret as an intermediary so its token
  # generators can depend on it -- agenix-rekey dedups the two definitions by
  # file, since the generator is identical.
  signingKey = server: {
    rekeyFile = ../../../secrets/generated + "/${server}/celler-signing-key.age";
    generator = {
      tags = [ "celler_signing_key" ];
      script =
        { pkgs, ... }:
        "${lib.getExe pkgs.openssl} genrsa -traditional 4096 | ${pkgs.coreutils}/bin/base64 -w0";
    };
  };

  # A consumer's token for one server, scoped to what it declared for it. A
  # generated secret is only minted when its file is missing, so the scope is
  # part of the file name: changing it mints a fresh token.
  tokenSecret =
    {
      dir,
      sub,
      server,
      signingKey,
    }:
    let
      scope = { inherit (server) pull push; };
      hash = builtins.substring 0 8 (builtins.hashString "sha256" (builtins.toJSON scope));
    in
    {
      rekeyFile = dir + "/celler_token-${server.name}-${hash}.age";
      generator = {
        tags = [ "celler_token" ];
        dependencies.signing_key = signingKey;
        script = cellerTokenScript (scope // { inherit sub; });
      };
    };

  urlHost = url: builtins.head (builtins.match "[a-z]+://([^:/]+).*" url);

  # Settings every celler server shares: the `cellerd` class content of
  # `den.aspects.celler-server`. Changing chunking makes existing chunks
  # unreusable (different cutpoints), hurting dedup until re-uploaded.
  sharedSettings = {
    jwt = { };
    chunking = {
      nar-size-threshold = 64 * 1024;
      min-size = 16 * 1024;
      avg-size = 64 * 1024;
      max-size = 256 * 1024;
    };
  };

  # `celleradm make-token` reads only [jwt]; database/storage only have to parse.
  tokenConfig =
    pkgs:
    (pkgs.formats.toml { }).generate "celler-token.toml" (
      sharedSettings
      // {
        database.url = "sqlite::memory:";
        storage = {
          type = "local";
          path = "/var/empty";
        };
      }
    );

  # Mint a scoped celler JWT with an agenix-rekey generator. The server's RS256
  # signing key is handed in as the `signing_key` dependency and decrypted with
  # the master identity at `agenix generate` time, so it never leaves the admin
  # host -- only the resulting token is rekeyed onto the target.
  cellerTokenScript =
    {
      sub,
      pull ? [ "main" ],
      push ? [ "main" ],
      validity ? "10y",
    }:
    {
      pkgs,
      decrypt,
      deps,
      ...
    }:
    let
      patternArgs = flag: lib.concatMapStringsSep " " (p: "${flag} ${lib.escapeShellArg p}");
    in
    ''
      export CELLER_SERVER_TOKEN_RS256_SECRET_BASE64="$(${decrypt} ${lib.escapeShellArg deps.signing_key.file})"
      ${lib.getExe' pkgs.celler "celleradm"} -f ${tokenConfig pkgs} make-token \
        --sub ${lib.escapeShellArg sub} \
        --validity ${lib.escapeShellArg validity} \
        ${patternArgs "--pull" pull} \
        ${patternArgs "--push" push}
    '';
}
