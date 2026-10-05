{ lib }:
rec {
  # The servers and caches a scope declared on `celler-use`, picked out of
  # every server's `celler-caches` record. A declaration is
  #   { <server> = [ <cache> ... ]; }                     pull and push
  #   { <server> = { pull = [ ... ]; push = [ ... ]; via = "tailscale"; }; }
  # and every aspect's declarations merge. `via` picks which of the server's
  # addresses to reach it at (virtualHost, cloudflared, tailscale); without it,
  # the server's own `consumeVia`. Each result is the server's record with
  # `pull` (everything it may read, pushes included), `push`, `endpoint` at the
  # chosen address, and `keys` narrowed to `pull`. Naming a server that does not exist is an error.
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
          via = lib.findFirst (v: v != null) null (map (n: n.via or null) ns);
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
      // {
        inherit (want) pull push;
        keys = lib.getAttrs (builtins.filter (c: server.keys ? ${c}) want.pull) server.keys;
      }
      // lib.optionalAttrs (want.via != null) {
        endpoint =
          server.addresses.${want.via}
            or (throw "celler-use: ${name} is not reachable via ${want.via} (addresses: ${toString (builtins.attrNames server.addresses)})");
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

  # A consumer's token for one server, scoped to what it declared for it.
  #
  # The scope is declared as `generator.derivedFrom`, NOT baked into the file
  # name. Upstream agenix-rekey only mints a generated secret when its file is
  # MISSING, so the old shape put an 8-char hash of the scope in the name --
  # changing what a consumer wanted changed the name, which made the file
  # missing, which minted a fresh token. That worked, but it meant the file name
  # carried a value nobody could read back, and it left an orphan on disk every
  # time a scope changed.
  #
  # ../../../patches/agenix-rekey/derivedFrom.patch exists precisely for this:
  # "regenerate a secret when the things it is a function of change, not only
  # when its file is missing". So the name is stable and the SCOPE is the
  # trigger, which is what it always meant.
  tokenSecret =
    {
      dir,
      sub,
      server,
      signingKey,
    }:
    let
      # WHAT THIS TOKEN IS A FUNCTION OF, written once. The file name and the
      # re-mint stamp both READ it, so neither restates a fact the other owns.
      #
      # `scope` is what the consumer declared for this server; `sub` is who the
      # token is for; `server` is whose RS256 key signs it. Change any of the
      # three and the old token is the wrong token.
      derivedFrom = {
        inherit (server) pull push;
        inherit sub;
        server = server.name;

        # ── AND THE KEY THAT SIGNS IT ──
        # Without this a re-minted signing key left every existing token in
        # place, still signed by the dead key, with nothing to trigger
        # regeneration: `derivedFrom` decided the stamp, the stamp decided the
        # filename, the file existed, so agenix had nothing to do. The only
        # symptom would be a 403 at run time -- the exact failure `missingKeys`
        # in ./server.nix exists to prevent, arriving by the other route.
        #
        # THE SECRET ITSELF, not a hash of it. `derivedFrom` already knows the
        # difference: ../../../patches/agenix-rekey/derivedFrom.patch's
        # `isSecretRef` splits the entries, hashing a secret from its .age file
        # with `sha256sum` when the script RUNS and plain values at eval time.
        #
        # Run time is what makes it correct rather than merely sufficient: the
        # key is generated earlier in the same invocation, so the token's stamp
        # sees the key that was just minted. Hashing it at eval time -- which is
        # what a `builtins.hashFile` here would do -- stamps the key's PREVIOUS
        # contents, so a fresh tree needs a second run to converge.
        #
        # A rekey re-mints every token, since age encryption is nondeterministic
        # and re-encrypting identical plaintext changes the bytes. Over-minting
        # a token is cheap; a token signed by a key nobody holds is not.
        inherit signingKey;
      };
    in
    {
      # NO `rekeyFile`. ../../../patches/agenix-rekey/derivedFrom.patch derives
      # the path itself, and it appends a stamp hash of `derivedFrom`:
      #
      #   secrets/generated/auspc/celler-host/celler_token_secondpc-fffda948.age
      #   secrets/generated/auspc/celler-host/celler_token_celler2-60cca104.age
      #
      # So the old hand-written name was not merely restating the secret's own
      # name -- its 8-char scope hash was REIMPLEMENTING the patch's stamp, badly:
      # a hash of `{ pull; push; }` only, so two servers sharing a scope and a
      # subject collided, and every scope change orphaned a file nobody collected.
      #
      # The two differing stamps above are the evidence that `server` belongs in
      # `derivedFrom`: without it auspc's secondpc and celler2 tokens compute the
      # same stamp, and each is signed with a DIFFERENT RS256 key.
      generator = {
        tags = [ "celler_token" ];
        dependencies.signing_key = signingKey;
        inherit derivedFrom;
        script = cellerTokenScript {
          inherit (derivedFrom) pull push sub;
        };
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

      # A FAILED DECRYPT IS NOT AN EMPTY KEY. `$(...)` swallows rage's exit
      # status, and celler's `read_non_empty_var` treats empty as UNSET -- so an
      # undecryptable signing key surfaced as a panic about TOML that has nothing
      # wrong with it, three layers from the cause. Length only: the value is the
      # secret, and stderr because stdout IS the generated secret.
      if [ -z "$CELLER_SERVER_TOKEN_RS256_SECRET_BASE64" ]; then
        echo "celler-token: decrypting ${deps.signing_key.file} produced nothing." >&2
        echo "  The file exists, so this is a DECRYPT failure, not a missing secret:" >&2
        echo "  the master identity cannot read it. Check rage's own error above." >&2
        exit 1
      fi

      ${lib.getExe' pkgs.celler "celleradm"} -f ${tokenConfig pkgs} make-token \
        --sub ${lib.escapeShellArg sub} \
        --validity ${lib.escapeShellArg validity} \
        ${patternArgs "--pull" pull} \
        ${patternArgs "--push" push}
    '';
}
