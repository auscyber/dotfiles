{
  den,
  lib,
  ...
}:
# One restic repository on Backblaze B2, many services, and no service file that
# has to know what the others asked for.
#
# The same shape as ./postgres.nix, for the same reason: a service aspect
# declares which of its state is irreplaceable on the `backup` QUIRK and
# includes this aspect, and the collection below turns every declaration into
# ONE `services.restic.backups` entry --
#
#   * the union of every entry's `paths`     -- one snapshot, one repo lock
#   * the union of every entry's `exclude`   -- checked, see `checkExcludes`
#   * a `pg_dump` per provisioned database   -- see `databases`
#   * a consistent copy of each `sqlite` db  -- see `prepare`
#   * one timer and one retention policy     -- server-global, so `settings`
#                                               throws on disagreement
#
# ONE snapshot rather than one per service: restic dedups across the whole
# repository either way, but a single snapshot means a single lock, a single
# `forget --prune` and a single thing to check after a bad night. The cost is
# that `exclude` and the schedule become global, which is exactly what the two
# throws below make visible.
#
# --- what is backed up, and what deliberately is not ---
#
# The test is "can nix rebuild it". Everything in the store, every rendered
# config, every provisioned kanidm client, every postgres ROLE (`ensureUsers`
# plus ../services/postgres.nix's role-password unit) comes back from this repo
# and a deploy, so none of it is here. What is here is what only exists because
# somebody typed it or used the service:
#
#   postgres     every provisioned database, as a plain-SQL dump
#   hass         .storage (areas and floors live ONLY there), the UI-authored
#                automations/scripts/scenes, esphome device YAML, the Matter
#                fabric, Music Assistant's provider instances, mosquitto hashes
#   audiobookshelf  its sqlite db: accounts and per-user listening progress
#   vikunja      task attachments
#   splitpro     receipt uploads
#   kanidm       its own online_backup output: credentials and passkeys
#
# NOT the media libraries under /mnt/hdd (TV, Movies, Music, AudioBooks,
# Podcasts, torrents). They are terabytes, they are the one thing on this box
# that is re-acquirable -- the whole servarr stack exists to acquire them again
# from a list -- and B2 charges for both the storage and the egress of a
# restore. Local durability for those is the ZFS pool they already live on.
# What is NOT re-acquirable about a library is the *arr apps' own view of it
# (quality profiles, indexer keys, history), which is small; that is a
# `backup = _: { ... }` on ./media/_servarr.nix away and deliberately left for
# a separate change rather than smuggled in here.
#
# NOT /mnt/hdd/nextcloud either, and this one is not a judgement about value:
# ./nextcloud.nix already rclone-syncs it to B2 on its own timer with a
# versioned `--backup-dir`. Backing the same bytes up twice would double the
# bill. Its DATABASE is not covered by that job, and is covered here.
#
# --- restoring ---
#
#   restic-b2 snapshots                  # the wrapper `createWrapper` installs
#   restic-b2 restore latest --target /restore
#   # databases, one at a time, into the cluster nix already provisioned:
#   sudo -u postgres psql -d hass < /restore/var/backup/restic/postgres-hass.sql
#
# The repository password is NOT in this repository's secrets alone -- see
# `secrets` below for why that matters.
let
  inherit (den.lib.policy) pipe;

  # Made by hand in the B2 dashboard. Bucket names are unique across ALL
  # Backblaze accounts, not just this one, so `restic init` may refuse this and
  # the name has to change -- same situation (and the same hand-made bucket) as
  # ./nextcloud.nix's `b2Bucket`.
  bucket = "ivymectin-restic";

  # MUST MATCH THE BUCKET'S REGION. B2 puts a bucket in exactly one region and
  # its S3-compatible endpoint names that region: this is the "Endpoint" field
  # on the bucket's own page in the dashboard, minus the scheme. Pinned here
  # only because the S3 path is the one restic recommends -- see `repository`
  # below, which is also where the consequence of getting it wrong is written.
  endpoint = "s3.us-west-004.backblazeb2.com";

  # Where dumps and sqlite copies are staged, snapshotted from, and removed
  # again. On the root pool, which is the small SSD: a plain-SQL nextcloud dump
  # is the biggest thing that lands here, and this is the one line to change if
  # that ever stops fitting.
  staging = "/var/backup/restic";

  # Both USER-SUPPLIED, and declared only when the file is actually there --
  # see the `secretPath` note in the nixos body.
  b2EnvSrc = ../../secrets/static/backblaze_app.age;
  passwordSrc = ../../secrets/static/restic_password.age;
  haveB2 = builtins.pathExists b2EnvSrc;
  havePassword = builtins.pathExists passwordSrc;

  # Quirk emissions arrive as a LIST of raw values -- there is no submodule to
  # carry defaults -- so normalise here, once, exactly as ./postgres.nix does.
  norm =
    e:
    let
      name =
        e.name
          or (throw "backup quirk: an entry has no `name`; it is the restic tag and how every error below names the aspect at fault");
    in
    {
      inherit name;
      paths = e.paths or [ ];
      exclude = e.exclude or [ ];
      sqlite = e.sqlite or [ ];
      settings = e.settings or { };
    };

  # Computed from every entry's own declaration, so an entry setting one through
  # `settings` would either lose its own value or silently overwrite the union.
  collectorOwned = [
    "paths"
    "exclude"
    "dynamicFilesFrom"
    "backupPrepareCommand"
    "backupCleanupCommand"
    "extraBackupArgs"
    "repository"
    "passwordFile"
    "environmentFile"
  ];

  # A conflicting setting is a throw, not a merge -- the same call
  # ./postgres.nix makes about `cron.database_name`. There is ONE repository,
  # ONE timer and ONE retention policy here, so two services quietly disagreeing
  # about how often or how long is the failure this aspect exists to prevent.
  mergeSettings =
    entries:
    lib.mapAttrs (_: v: v.value) (
      lib.foldl' (
        acc: e:
        lib.foldlAttrs (
          acc': k: v:
          if lib.elem k collectorOwned then
            throw "backup quirk: '${e.name}' sets `settings.${k}`, which this collector computes from every entry's own declaration -- declare it on the entry instead"
          else if acc' ? ${k} && acc'.${k}.value != v then
            throw "backup quirk: setting '${k}' is claimed by '${acc'.${k}.by}' and '${e.name}' with different values; the repository has one timer and one retention policy, so only one aspect may set it"
          else
            acc'
            // {
              ${k} = {
                value = v;
                by = e.name;
              };
            }
        ) acc e.settings
      ) { } entries
    );

  # restic takes ONE `--exclude-file` for the WHOLE snapshot, so an exclude is
  # global even though one service declared it. A relative pattern matches any
  # path COMPONENT of every path in the snapshot (`cache` would drop
  # /var/lib/hass/cache and music-assistant's cache and anything else called
  # that), and an absolute one outside the declaring entry's own paths is
  # reaching into somebody else's data. Both are the cross-service footgun that
  # a per-service `exclude` invites, so both are refused here rather than
  # discovered as a hole in a restore.
  checkExcludes =
    e:
    map (
      p:
      if !(lib.hasPrefix "/" p) then
        throw "backup quirk: '${e.name}' excludes '${p}', which is not absolute -- restic matches a relative pattern against any path component in the snapshot, including other services'"
      else if !(lib.any (root: p == root || lib.hasPrefix "${root}/" p) e.paths) then
        throw "backup quirk: '${e.name}' excludes '${p}', which is under none of its own paths (${lib.concatStringsSep ", " e.paths}); one --exclude-file covers the whole snapshot, so it would silently drop another service's data"
      else
        p
    ) e.exclude;

  # Staged filename for a sqlite source, flat so two services' `data.db` cannot
  # collide.
  sqliteName = db: "sqlite-" + lib.replaceStrings [ "/" ] [ "-" ] (lib.removePrefix "/" db);
in
{
  den.quirks.backup.description = "state a service cannot have rebuilt for it, for the restic repository to carry off the box";

  den.policies.backup-sources = _: pipe.from "backup" [ (pipe.collectAll (_: true)) ];

  den.aspects.restic = {
    includes = [
      den.aspects.agenix-rekey
      den.policies.backup-sources
    ];

    # NOT `den.aspects.postgres`. `databases` below reads the cluster's
    # `ensureDatabases`, which is empty and harmless on a host that has no
    # postgres -- pulling a database server onto a host that only wanted its
    # files backed up would be the tail wagging the dog.

    secretSettings = {
      # The generated units are `restic-backups-<name>`, never `restic`, so the
      # scope's name would infer nothing -- and inference has to stay OFF
      # rather than be pointed at a unit name: both secrets are read by root in
      # the unit's preStart, and nixpkgs sets `restartIfChanged = false` on it,
      # so restarting a oneshot is not the right response to a rotation either
      # (the next timer run picks the new value up). Same escape hatch
      # ../base/celler/server.nix uses, documented in ../security/age-scope.nix.
      service = null;
    };

    # ISSUED, not chosen, and therefore `rekeyFile` rather than
    # `generator.script`: Backblaze mints the key pair and no generator can
    # produce it.
    #
    # ONE .age file holding the whole env file, not two secrets plus a template.
    # B2 hands out the key id together with the application key and will not
    # show the key again afterwards, so the two halves share a lifecycle and are
    # always rotated as a pair -- one file is one thing to re-enter, with
    # nothing that can drift between them. It also sidesteps the template
    # machinery, which matters here: a template implicitly depends on every
    # secret in its scope, so a template reaching into ANOTHER scope closes a
    # loop that surfaces only when secrets are GENERATED (./postgres.nix's
    # `rolePasswords` note and ../security/age-scope.nix spell that out). There
    # is nothing to render, so there is no template.
    #
    # `backblaze_app.age` is the RESTRICTED application key, scoped to the one
    # bucket -- deliberately not `backblaze_master.age`, which is the account
    # master key and can delete buckets and read the whole account. restic needs
    # only listBuckets/listFiles/readFiles/writeFiles on this bucket.
    #
    # Its two lines are AWS-named, because this uses the S3-compatible endpoint
    # (see `repository`). B2's keyID is the access key id and its applicationKey
    # is the secret access key: the same credential pair the native backend
    # would read as `B2_ACCOUNT_ID`/`B2_ACCOUNT_KEY`, under other names.
    #
    #   AWS_ACCESS_KEY_ID=<keyID>
    #   AWS_SECRET_ACCESS_KEY=<applicationKey>
    #
    # The repository password is `rekeyFile` too, and that is a deliberate
    # refusal of `random-hex-32`. A generated secret's only plaintext copy is
    # the one encrypted to the age master identities -- and this fleet has
    # already lost a master identity once (../security/agenix-rekey.nix's
    # `masterIdentities` note, ../security/recover-master.sh). A backup is the
    # last-resort recovery path, so it must not be undecryptable by exactly the
    # event it would be recovering from. Chosen by a human, kept in 1Password
    # next to ./backup-master.pub's private half, and then committed here.
    #
    # Conditional on the source existing: see `secretPath` below.
    secrets =
      lib.optionalAttrs haveB2 { b2-env.rekeyFile = b2EnvSrc; }
      // lib.optionalAttrs havePassword { password.rekeyFile = passwordSrc; };

    nixos =
      {
        config,
        pkgs,
        lib,
        backup,
        scoped,
        ...
      }:
      let
        entries = map norm (lib.flatten backup);

        declaredPaths = lib.unique (lib.concatMap (e: e.paths) entries);
        excludes = lib.unique (lib.concatMap checkExcludes entries);
        sqlites = lib.unique (lib.concatMap (e: e.sqlite) entries);
        tags = lib.unique (map (e: e.name) entries);

        # Every database ../services/postgres.nix provisioned, dumped. NOT
        # declared per service on this quirk, even though that would be
        # symmetrical: a database that exists holds state by definition,
        # `ensureDatabases` IS the complete list (that is the whole point of
        # that quirk), and taking it from there is what covers ./nextcloud.nix's
        # and ./homeassistant/default.nix's databases without either file
        # having to mention backups -- which matters, because a service aspect
        # that forgot to declare its database here would lose it silently.
        #
        # A dump, not /var/lib/postgresql: a file-level copy of a running
        # cluster is torn, and even an intact one cannot be restored across a
        # postgres major (the pinned `postgresql_17` note in ./postgres.nix).
        # `pg_dump` is one MVCC snapshot per database, so each file is
        # self-consistent; the SET of them is not mutually consistent, which is
        # fine because these are independent applications.
        #
        # Plain SQL, not `--format=custom`: custom is zlib-compressed, so every
        # byte of every run differs and restic's dedup gets nothing on the part
        # of the snapshot that changes most.
        #
        # No `pg_dumpall --globals-only`: roles and their grants come from
        # `ensureUsers`, and their passwords from ./postgres.nix's
        # `postgresql-role-passwords` unit, so the globals are rebuilt by a
        # deploy.
        databases = lib.optionals config.services.postgresql.enable config.services.postgresql.ensureDatabases;

        staged = databases != [ ] || sqlites != [ ];

        # `age.secrets.<n>.file` is an ASSERTION that both the static source and
        # its rekeyed copy exist (agenix-rekey modules/agenix-rekey.nix), and
        # `system.build.toplevel` forces it -- so declaring a secret whose
        # secrets/static/*.age is not there yet takes down the whole host's
        # build, not just this unit. Hence the two sources are declared only
        # once they exist, and until then the unit is pointed at the path they
        # WILL have, with a systemd condition standing in. That is the same
        # "configured unconditionally, skipped at runtime until the credentials
        # exist" shape ./nextcloud.nix's B2 job uses, and for the same reason.
        secretPath =
          name: have:
          if have then scoped.restic.secrets.${name}.path else "${config.age.secretsDir}/restic/${name}";
        envPath = secretPath "b2-env" haveB2;
        passwordPath = secretPath "password" havePassword;

        pgDump = lib.getExe' config.services.postgresql.package "pg_dump";
        runuser = lib.getExe' pkgs.util-linux "runuser";
        sqlite3 = lib.getExe' pkgs.sqlite "sqlite3";

        # systemd runs this through `pkgs.writeScript`, which adds no shebang of
        # its own, so give it one rather than relying on bash's ENOEXEC
        # fallback.
        #
        # `rm -rf` FIRST: `backupCleanupCommand` already runs on postStop (so it
        # runs after a failure too), but a staging directory that survived a
        # crash would otherwise be snapshotted as if it were today's dump.
        #
        # A postgres dump is allowed to fail the whole backup: it means the
        # cluster is not answering, which is worth a red unit. A missing sqlite
        # file is not -- a service that has never started has no database yet,
        # and that must not cost the other fifteen paths their snapshot.
        prepare = ''
          #!${pkgs.runtimeShell}
          set -euo pipefail
          umask 077
          rm -rf ${staging}
          install -d -m 0700 ${staging}
          ${lib.concatMapStringsSep "\n" (db: ''
            ${runuser} -u postgres -- ${pgDump} --format=plain ${lib.escapeShellArg db} \
              > ${staging}/postgres-${db}.sql
          '') databases}
          ${lib.concatMapStringsSep "\n" (db: ''
            if [ -e ${lib.escapeShellArg db} ]; then
              ${sqlite3} ${lib.escapeShellArg db} '.timeout 30000' \
                ${lib.escapeShellArg "VACUUM INTO '${staging}/${sqliteName db}'"}
            else
              echo "restic: ${db} does not exist, not in this snapshot" >&2
            fi
          '') sqlites}
        '';

        # Every dump in here is a full copy of somebody's data, so it does not
        # outlive the run. postStop, so a failed backup does not leave it either.
        cleanup = ''
          #!${pkgs.runtimeShell}
          set -euo pipefail
          rm -rf ${staging}
        '';

        # `dynamicFilesFrom` rather than `paths`, which is the difference
        # between "one service has not been started yet" and "nothing was backed
        # up tonight": restic given a nonexistent path in --files-from fails the
        # snapshot, and these paths include several systemd StateDirectories
        # that only exist once their unit has run. Missing ones are announced on
        # stderr, which is the journal, so this is skipping-with-a-record rather
        # than skipping silently.
        #
        # The residual risk is real and worth naming: a path that DISAPPEARS is
        # reported in the same breath as one that has not appeared yet, so the
        # journal is the only place that distinguishes them.
        filesFrom = ''
          #!${pkgs.runtimeShell}
          set -u
          for p in ${lib.escapeShellArgs (declaredPaths ++ lib.optional staged staging)}; do
            if [ -e "$p" ]; then
              printf '%s\n' "$p"
            else
              echo "restic: $p does not exist, not in this snapshot" >&2
            fi
          done
        '';
      in
      {
        services.restic.backups = lib.mkIf (entries != [ ]) {
          b2 = {
            # B2's S3-COMPATIBLE endpoint rather than restic's native `b2:`
            # backend, and not on a hunch -- restic 0.19.1's own documentation
            # (doc/030_preparing_a_new_repo.rst, "Backblaze B2") opens that
            # section with a warning saying exactly this:
            #
            #   Due to issues with error handling in the current B2 library that
            #   restic uses, the recommended way to utilize Backblaze B2 is by
            #   using its S3-compatible API. [...] This is expected to work
            #   better than using the Backblaze B2 backend directly.
            #
            # ./nextcloud.nix reaches the opposite conclusion for its rclone job
            # and is right to: rclone's `b2` backend is a different
            # implementation with its own error handling, and it hands B2 its
            # own SHA1 per object. The warning is about the `blazer` library
            # restic links, not about B2.
            #
            # The cost is `endpoint` above -- the region is pinned here, where
            # the native backend would have derived it from the credentials. A
            # wrong one is a first run that cannot find the bucket: loud, and a
            # one-line fix.
            #
            # ALSO REQUIRED, on the bucket and not expressible from here: the
            # lifecycle rule "Keep only the last version of the file". restic's
            # S3 backend HIDES the files it no longer needs rather than deleting
            # them, and B2 keeps every hidden version indefinitely by default --
            # so without that rule `forget --prune` frees nothing and the bill
            # grows forever while the backups look healthy. Same restic warning,
            # second paragraph.
            repository = "s3:${endpoint}/${bucket}/${config.networking.hostName}";
            passwordFile = passwordPath;
            environmentFile = envPath;

            # First run creates the repository. Cheap afterwards: the module
            # emits `restic cat config || restic init`.
            initialize = true;

            exclude = excludes;
            dynamicFilesFrom = filesFrom;
            backupPrepareCommand = prepare;
            backupCleanupCommand = cleanup;

            extraBackupArgs = [
              # Honour CACHEDIR.TAG, which several of these state directories
              # drop into their own caches.
              "--exclude-caches"
              "--tag auto"
            ]
            ++ map (t: "--tag ${t}") tags;

            # 14 daily covers "I noticed last week"; the weeklies and
            # monthlies cover the failure mode this is really for, which is
            # corruption or a bad delete that nobody sees for a month. Bounded
            # at 3 years because these are dumps and configs, not an archive.
            #
            # `forget --prune` runs in the same unit after the backup, so
            # retention is applied daily rather than drifting until somebody
            # remembers.
            pruneOpts = [
              "--keep-daily 14"
              "--keep-weekly 8"
              "--keep-monthly 12"
              "--keep-yearly 3"
            ];

            # `runCheck` is left off deliberately: `restic check` re-reads the
            # whole index every run, which on B2 is paid for per request. A
            # periodic `restic-b2 check --read-data-subset=1/30` belongs on its
            # own slower timer, and is not here yet.

            timerConfig = {
              OnCalendar = "03:15";
              # Spreads the B2 upload off the hour, and off whatever else in
              # the fleet wakes up at 03:15.
              RandomizedDelaySec = "45m";
              # A box that was off at 03:15 backs up when it comes back,
              # rather than skipping the day.
              Persistent = true;
            };
          }
          // mergeSettings entries;
        };

        systemd.services."restic-backups-b2" = lib.mkIf (entries != [ ]) {
          # Skipped, not failed, while the credentials do not exist -- see
          # `secretPath`. Two DIFFERENT condition types on purpose: systemd ORs
          # repeated conditions of the same type, so two `ConditionPathExists`
          # would let either file alone satisfy both. `FileNotEmpty` on the
          # password additionally catches a zero-byte file, which restic would
          # otherwise happily accept as a password and lock a new repository
          # with.
          unitConfig = {
            ConditionPathExists = envPath;
            ConditionFileNotEmpty = passwordPath;
          };

          # The prepare command dumps through the unix socket, so the cluster has
          # to be up. Ordering only -- on a running host it already is, and a
          # `requires` would turn a postgres that is deliberately down into a
          # failed backup of everything else.
          after = lib.optional (databases != [ ]) "postgresql.target";
        };
      };
  };
}
