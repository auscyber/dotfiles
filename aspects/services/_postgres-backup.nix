# PARKED, not deleted. The leading underscore keeps this out of the loader's
# auto-import (the convention ./media/_servarr.nix relies on), so it is inert.
#
# Why: PG17 native incremental backup (summarize_wal + pg_basebackup
# --incremental + pg_combinebackup) is wanted, but AFTER the service-relation
# model is cleaned up and moved onto the gen libraries -- a backup that depends
# on the postgres service relation should be built against the final shape of
# that relation, not the placeholder. The agent writing this was stopped
# mid-task, so treat the contents as unfinished: there was an unused binding at
# line 3 and it was never gate-verified end to end.
#
# To resume: drop the underscore, finish it, and re-check the invariants.
{
  den,
  lib,
  ...
}:
# PostgreSQL 17 native INCREMENTAL base backups, chained, verified, and handed
# to ../services/restic.nix to ship off the box.
#
# ../services/restic.nix already takes a `pg_dump` of every provisioned
# database. That is a LOGICAL copy and it is a full copy every night: its size
# is the size of the data, not the size of the change, so the nightly upload
# grows with the database forever. This aspect adds the mechanism postgres
# itself provides for sending only what moved -- `pg_basebackup --incremental`,
# new in 17 -- which ships only the BLOCKS whose LSN advanced since the
# previous backup in the chain.
#
#   full        pg_basebackup --format=plain --pgdata=<m>
#   incremental pg_basebackup --format=plain --pgdata=<m> \
#                 --incremental=<prev>/backup_manifest
#   restore     pg_combinebackup <full> <incr>... -o <datadir>
#
# THE LOGICAL DUMP STAYS, and not out of timidity. A base backup is a PHYSICAL
# copy of the data directory: it restores only into the SAME postgres major
# (the `postgresql_17` pin in ./postgres.nix spells out that a major bump here
# is a dump-and-restore, not an activation). So the two copies answer two
# different questions -- the chain answers "yesterday's rows, cheaply", the
# dump answers "the rows, on whatever postgres we are running next year". Only
# one of them survives `postgresql_17` -> `postgresql_18`.
#
# What the dump should NOT be is nightly, now that this exists. Reducing it is
# a change to ./restic.nix (`databases`/`prepare` are that collector's own,
# and `backupPrepareCommand` is in its `collectorOwned` list, so it cannot be
# reached from here) and is reported rather than smuggled in. See
# `theDumpShouldBeWeekly` below for the exact patch.
#
# --- the chain, and why this one cannot be left unrestorable ---
#
# An incremental backup is worthless without every ancestor back to its full:
# upstream says so in as many words ("PostgreSQL has no built-in mechanism to
# figure out which backups are still needed as a basis for restoring later
# incremental backups. You must keep track of the relationships between your
# full and incremental backups on your own" -- doc/src/sgml/backup.sgml,
# "Making an Incremental Backup"). Three rules keep that bookkeeping honest,
# and they are the whole design:
#
#  1. A MEMBER IS NOT A MEMBER UNTIL IT IS VERIFIED. `pg_verifybackup` runs on
#     every member while it is still in the staging directory, and only a
#     member that passed is moved into the chain and given its `.ok` marker. An
#     incremental is only ever taken against a member that has `.ok`, so a bad
#     member can never become somebody's ancestor. A member found without `.ok`
#     is a crashed run and is deleted -- it can only ever be the newest, so
#     deleting it leaves a verified PREFIX, which is still a legal chain.
#
#  2. `CHAIN` NAMES THE CHAIN, and is written atomically, after the new member
#     is in place and before anything is deleted. So whatever instant a reader
#     (or a restic snapshot) catches this directory in, the members `CHAIN`
#     names are all present and all verified. That file is the "keep track of
#     the relationships on your own" that upstream asks for, and the restore
#     runbook at the bottom of this file reads it rather than guessing.
#
#  3. ONE CHAIN ON DISK, EVERY SNAPSHOT SELF-CONTAINED. A new full does not
#     start a second chain next to the old one: it is published first, then
#     `CHAIN` is rewritten, then everything `CHAIN` does not name is removed.
#     Peak disk is therefore one chain plus one full, not two chains -- and,
#     the part that matters, EVERY restic snapshot contains one complete
#     chain. No snapshot depends on another snapshot, so no `forget --prune`
#     policy in ./restic.nix can orphan anything. That is the answer to "how
#     do you expire old chains without orphaning a chain a newer incremental
#     still needs": the question cannot arise, because a chain never spans
#     snapshots.
#
# If a single incremental is lost or corrupted, everything after it is
# unrestorable and the newest usable restore point is its parent. That is
# inherent to the mechanism, which is why rule 1 verifies before chaining and
# why `fullEvery` is a week rather than a month: a 7-member chain has six
# things that can go wrong, a 30-member chain has twenty-nine.
#
# --- why plain and not tar ---
#
# `--format=plain`, deliberately, even though the sqlbak-style recipe writes
# `--format=tar`:
#
#   * `pg_verifybackup` in 17 REFUSES a tar backup -- "The backup must be
#     stored in the 'plain' format; a 'tar' format backup can be checked after
#     extracting it" (doc/src/sgml/ref/pg_verifybackup.sgml). Verification is
#     not optional here (rule 1), and verifying tar would mean extracting a
#     full copy of the cluster every night, which is the opposite of the point.
#     `pg_combinebackup` documents no tar input either.
#
#   * restic dedups by content-defined chunking. Plain gives it one file per
#     1GB relation segment, so a changed 8kB block dirties the one chunk it
#     lands in and every other file in the tree is byte-identical to last
#     night's and resolves to blobs the repository already has. One `base.tar`
#     would also dedup -- CDC is built for shifted streams -- but restic would
#     have to READ AND CHUNK the entire tar every run, so the nightly cost
#     would scale with the size of the cluster instead of with the size of the
#     change. The user's complaint is exactly that scaling.
#
#   * And nothing is compressed, for the reason ./restic.nix already gives
#     about `pg_dump --format=custom`: a compressed stream has no bytes in
#     common with yesterday's, so dedup gets nothing on the part of the
#     snapshot that changes most.
#
# --- what this needs from the server ---
#
#   summarize_wal = on   contributed below. Without it `--incremental` has no
#                        WAL summaries to diff against and fails.
#   wal_level >= replica postgres 17's DEFAULT is `replica`, and nothing in
#                        this tree lowers it, so there is nothing to set. Only
#                        `minimal` is incompatible ("The server cannot be
#                        started with summarize_wal=on if wal_level is set to
#                        minimal"), and `minimal` would already have broken
#                        ./restic.nix's assumptions too.
#   max_wal_senders >= 2 one sender for the backup, one for `--wal-method=stream`
#                        (pg_basebackup.sgml). The default is 10 and nothing
#                        here sets it.
#   a replication entry  in pg_hba, and this one IS missing by default -- see
#     in pg_hba           `services.postgresql.authentication` below.
#   wal_summary_keep_time the default 10 days needs no change. An incremental
#                        needs summaries covering only the span from its
#                        PARENT's start LSN to its own -- one day here, not the
#                        length of the chain -- so 10 days is ten times the
#                        margin required. `fullEvery` is about restore cost and
#                        fragility, not about summary retention.
let
  # Where the chain lives, and the one line to change if the root SSD stops
  # being the right place for it. Peak usage is one full copy of the cluster's
  # data directory plus the week's incrementals plus one more full while a
  # weekly rotation is in flight -- so budget a little over 2x the cluster.
  # ./restic.nix's `staging` makes the same call about the same disk.
  root = "/var/backup/postgres";

  # Published, verified members. THIS is what restic snapshots.
  chainDir = "${root}/chain";

  # Staging. Deliberately NOT under `chainDir`: a member is written here and
  # verified here, so a restic run that overlaps this one cannot snapshot a
  # half-written backup. It is the reason `backup.paths` below points at
  # `chainDir` and not at `root`.
  workDir = "${root}/work";

  # Where members leaving the chain are moved before being deleted, so the
  # removal is a rename out of the snapshotted tree followed by an unlink
  # outside it.
  trashDir = "${root}/trash";

  # A full backup at most this many days old, so the chain spans a week.
  fullEvery = 7;

  # Second guard on chain length, independent of the timer: if the unit runs
  # more than once a day (a `Persistent=true` catch-up, a manual
  # `systemctl start`), the age check alone would let the chain grow.
  chainMax = 7;
in
{
  # The chain belongs to the cluster, so it is attached to the cluster's aspect
  # rather than to a host: wherever ./postgres.nix provisions a database, this
  # follows. Written from HERE rather than as an edit to that file, which is
  # the same seam ./homeassistant/prom-device.nix uses
  # (`den.aspects.homeassistant.includes`). `includes` is a `listOf`, so this
  # concatenates with that file's own list instead of replacing it.
  den.aspects.postgres.includes = [ den.aspects.postgres-backup ];

  den.aspects.postgres-backup = {
    includes = [
      # The off-box half. The `backup` quirk emitted below is only collected if
      # this aspect's `den.policies.backup-sources` is active, and a chain that
      # never leaves the box is not a backup. ./restic.nix deliberately does
      # NOT include ./postgres.nix (a host that wanted its files backed up
      # should not get a database server); this is the other direction, where
      # the dependency is real.
      den.aspects.restic
    ];

    # ../services/restic.nix's `backup` quirk. Field names are that file's
    # `norm`: name / paths / exclude / sqlite / settings.
    #
    # `chainDir`, not `root`: `workDir` and `trashDir` hold half-written and
    # about-to-be-deleted copies, and neither belongs in a snapshot. Nothing is
    # excluded, because nothing that should not be snapshotted is under the one
    # path declared -- which is also what keeps this entry clear of
    # ./restic.nix's `checkExcludes`, whose whole point is that one
    # `--exclude-file` covers every service's paths.
    #
    # No `sqlite` and no `databases`: this is a physical copy, not a logical
    # one, and ./restic.nix derives the logical dumps from
    # `ensureDatabases` by itself.
    backup = _: {
      name = "postgres-basebackup";
      paths = [ chainDir ];
    };

    nixos =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      let
        pg = config.services.postgresql.package;
        pgBasebackup = lib.getExe' pg "pg_basebackup";
        pgVerifybackup = lib.getExe' pg "pg_verifybackup";
        runuser = lib.getExe' pkgs.util-linux "runuser";

        # nixpkgs patches postgres' compiled-in socket directory to
        # /run/postgresql (pkgs/servers/sql/postgresql/patches/
        # socketdir-in-run-13+.patch), so this IS the default -- it is passed
        # explicitly anyway so that a stray PGHOST in the unit's environment
        # cannot redirect the connection to TCP, where there is no replication
        # entry in pg_hba and no peer auth to satisfy it.
        socketDir = "/run/postgresql";

        script = ''
          set -euo pipefail
          umask 077

          root=${root}
          chain=${chainDir}
          work=${workDir}
          trash=${trashDir}

          install -d -m 0700 "$root" "$chain"
          rm -rf "$work" "$trash"
          # pg_basebackup writes as postgres and creates the member directory
          # itself, so the staging PARENT is what has to be postgres-owned.
          install -d -o postgres -g postgres -m 0700 "$work"

          # Member names sort chronologically, which is what lets the chain be
          # reconstructed from the directory listing alone and what lets a new
          # full be published BEFORE the old chain is removed (rule 3): the two
          # cannot collide on a name.
          list_members() {
            find "$chain" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort
          }

          # The chain = the newest *-full and every member after it. Each
          # *-full resets the accumulator, so what survives to END is exactly
          # that run.
          cur_chain() {
            list_members | awk '/-full$/ { n = 0 } { a[++n] = $0 } END { for (i = 1; i <= n; i++) print a[i] }'
          }

          # Rule 1. No `.ok` means the run that wrote it died or its
          # verification failed. Nothing is ever chained onto such a member, so
          # it can only be the newest one, and dropping it leaves a verified
          # prefix.
          for m in $(list_members); do
            if [ ! -e "$chain/$m.ok" ]; then
              echo "pg-basebackup: $m has no .ok marker (crashed or rejected run); discarding it" >&2
              rm -rf "$chain/$m"
            fi
          done

          # Markers whose member is gone: a run that died between the two steps
          # at the bottom of this script. Harmless, but they would accumulate
          # one per crash and be snapshotted forever.
          for k in $(find "$chain" -mindepth 1 -maxdepth 1 -name '*.ok' -printf '%f\n'); do
            if [ ! -d "$chain/$(basename "$k" .ok)" ]; then
              rm -f "$chain/$k"
            fi
          done

          take_full=1
          members=$(cur_chain)
          if [ -n "$members" ]; then
            full=$(printf '%s\n' "$members" | head -n1)
            count=$(printf '%s\n' "$members" | wc -l)
            age=$(( ( $(date +%s) - $(stat -c %Y "$chain/$full.ok") ) / 86400 ))
            if [ "$count" -lt ${toString chainMax} ] && [ "$age" -lt ${toString fullEvery} ]; then
              take_full=0
            else
              echo "pg-basebackup: chain is $count member(s), $age day(s) old; starting a new one" >&2
            fi
          fi

          stamp=$(date -u +%Y%m%dT%H%M%SZ)

          # --checkpoint=fast rather than the default `spread`: spread waits for
          # up to checkpoint_timeout before the copy even starts, and this runs
          # at 02:00 on an otherwise idle box, so the I/O spike is free and the
          # unit does not sit holding a wal sender and a temporary replication
          # slot for five minutes doing nothing.
          #
          # --wal-method=stream (the default, spelled out) makes each member
          # self-contained: the WAL written DURING that member's copy lands in
          # its own pg_wal/, so the combined output can reach a consistent state
          # with no WAL archive. There is no archive -- see the runbook.
          run_base() {
            m="$1"
            shift
            ${runuser} -u postgres -- ${pgBasebackup} \
              --host=${socketDir} \
              --pgdata="$work/$m" \
              --format=plain \
              --wal-method=stream \
              --checkpoint=fast \
              --verbose \
              "$@"
          }

          if [ "$take_full" = 0 ]; then
            parent=$(printf '%s\n' "$members" | tail -n1)
            member="$stamp-incr"
            if ! run_base "$member" --incremental="$chain/$parent/backup_manifest"; then
              # The expected cause is missing WAL summaries: summarize_wal only
              # just came on, or the summarizer fell behind, or
              # wal_summary_keep_time expired the range. A full backup is always
              # legal, so degrade to one rather than fail -- loudly, because a
              # fallback happening every night means summarization is broken and
              # the whole point of this aspect is gone.
              echo "pg-basebackup: incremental against $parent FAILED; WAL summaries for its range are probably missing (summarize_wal / wal_summary_keep_time). Falling back to a FULL backup." >&2
              rm -rf "$work/$member"
              take_full=1
            fi
          fi

          if [ "$take_full" = 1 ]; then
            member="$stamp-full"
            run_base "$member"
          fi

          # Rule 1, the gate. A rejected member is deleted and the unit goes
          # red; the chain on disk is untouched and still restorable, so the
          # cost of a red night is one lost restore point, never a lost chain.
          #
          # This is the one step that cannot be checked from eval: 17's
          # pg_verifybackup is documented for plain backups and says nothing
          # about incremental ones. If it turns out to reject an incremental
          # outright, this degrades to a full backup every night with a red unit
          # every night -- wasteful and loud, which is the right direction to
          # fail in.
          if ! ${pgVerifybackup} "$work/$member"; then
            echo "pg-basebackup: pg_verifybackup REJECTED $member; discarding it. $chain/CHAIN still names a complete, verified chain." >&2
            rm -rf "$work"
            exit 1
          fi

          mv "$work/$member" "$chain/$member"
          : > "$chain/$member.ok"

          # Rule 2. Written after the new member is in place and before anything
          # is removed, and renamed into position so a reader never sees half a
          # list. Everything a restore needs to know is in this file.
          cur_chain > "$chain/.CHAIN.new"
          mv "$chain/.CHAIN.new" "$chain/CHAIN"

          # Rule 3. Only now, with CHAIN naming a complete chain, does anything
          # get thrown away -- and by a rename OUT of the snapshotted tree
          # first, so the unlinking happens where restic is not looking.
          install -d -m 0700 "$trash"
          keep=$(cat "$chain/CHAIN")
          for m in $(list_members); do
            if ! printf '%s\n' "$keep" | grep -qxF -- "$m"; then
              echo "pg-basebackup: $m is no longer in the chain; removing it" >&2
              mv "$chain/$m" "$trash/"
              rm -f "$chain/$m.ok"
            fi
          done
          rm -rf "$trash" "$work"
        '';
      in
      {
        # THE CHANNEL: the `nixos` class, not ./postgres.nix's `postgres` quirk,
        # and that is a forced choice rather than a preference. That quirk's
        # `norm` does `inherit (d) database` -- a hard requirement -- so there
        # is no settings-only emission: contributing `summarize_wal` through it
        # would mean inventing a database that exists only to carry a server
        # setting, which `ensureDatabases`, `ensureUsers` and ./restic.nix's
        # nightly `pg_dump` would then all take seriously.
        #
        # `services.postgresql.settings` is a submodule with a freeform
        # `attrsOf`, so NixOS merges this with that file's `settings` by
        # ATTRIBUTE and neither definition replaces the other. The exclusivity
        # the quirk provides by throwing is not lost either, just relocated: a
        # second aspect setting `summarize_wal` to something else is "defined
        # multiple times" at eval, which fails just as closed.
        #
        # Checked against what is actually claimed today -- `cron.database_name`
        # (./splitpro.nix, through the quirk), plus nixpkgs' own `jit`,
        # `log_*`, `listen_addresses`, `port`, `hba_file`, `ident_file` and
        # ./postgres.nix's `shared_preload_libraries`. No collision.
        #
        # Reload, not restart: summarize_wal is settable in postgresql.conf and
        # picked up by the summarizer, so the first incremental after it comes
        # on may still fail for want of summaries covering its range. The
        # fallback in the script above turns that into one extra full backup
        # instead of a failure.
        services.postgresql.settings.summarize_wal = true;

        # nixpkgs' default pg_hba is `local all postgres peer map=postgres`,
        # `local all all peer` and two `host all all` lines -- and `all` in the
        # DATABASE field does not match a PHYSICAL replication connection, which
        # is the only kind pg_basebackup makes. So without this line
        # pg_basebackup is refused outright ("no pg_hba.conf entry for
        # replication connection from host [local]") however super a user it
        # connects as.
        #
        # `peer` and not scram, for the reason ./postgres.nix gives for the
        # socket generally: the OS user is postgres, the role is postgres, and
        # peer auth checks exactly that -- so there is no password anywhere and
        # nothing to rotate. No `map=` is needed, unlike nixpkgs' `local all
        # postgres` line, because the two names already agree.
        #
        # Position does not matter: no other record in this file matches
        # `replication`, so this is the only candidate wherever it lands among
        # the `mkAfter` blocks. `mkAfter` matches what ./postgres.nix does.
        services.postgresql.authentication = lib.mkAfter ''
          local replication postgres peer
        '';

        systemd.services = lib.mkMerge [
          {
            postgres-basebackup = {
              description = "PostgreSQL incremental base backup chain";

              # Ordering only, never `requires` -- ./restic.nix's note on the
              # same point: a postgres that is deliberately down should not be
              # turned into a dependency failure. If it is down and this runs,
              # pg_basebackup fails and the unit goes red, which is the honest
              # outcome. Nothing is skipped quietly.
              after = [ "postgresql.target" ];

              path = [
                pkgs.coreutils
                pkgs.findutils
                pkgs.gawk
                pkgs.gnugrep
              ];

              serviceConfig = {
                Type = "oneshot";
                # Root: it manages root-owned directories under /var/backup and
                # drops to postgres with runuser for the backup itself, which is
                # the shape ./restic.nix's `prepare` already uses and works for
                # the same reason (peer auth on the socket).
                User = "root";
                # systemd disables the start timeout for Type=oneshot, and a
                # pg_basebackup wedged on a dead connection would then hold a
                # temporary replication slot and a wal sender forever. Six hours
                # is far past any honest run on this hardware.
                TimeoutStartSec = "6h";
              };

              inherit script;
            };
          }

          # 02:00 against ./restic.nix's 03:15 + up to 45m of
          # RandomizedDelaySec leaves well over an hour of headroom, so the two
          # do not normally meet -- but they DO meet when both timers catch up
          # together after the box was off (`Persistent = true` on both). An
          # `After=` only orders units inside one transaction, which is exactly
          # that case, so it is worth the line.
          #
          # Guarded, and `systemd.services` as a whole rather than
          # `systemd.services."restic-backups-b2"`, because a conditional
          # definition at the deeper path would still CONTRIBUTE THE ATTRIBUTE
          # NAME and conjure an empty unit on a host that has no restic.
          (lib.mkIf (config.services.restic.backups ? b2) {
            "restic-backups-b2".after = [ "postgres-basebackup.service" ];
          })
        ];

        systemd.timers.postgres-basebackup = {
          description = "PostgreSQL incremental base backup chain";
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = "02:00";
            # A box that was off at 02:00 backs up when it returns rather than
            # skipping the day -- and an overdue full is then taken by the age
            # check in the script, not by the calendar.
            Persistent = true;
            # No RandomizedDelaySec, unlike ./restic.nix's: the headroom in
            # front of that job is the point, and spreading this one would eat
            # it.
            RandomizedDelaySec = 0;
          };
        };

        # /var/backup is shared with ./restic.nix's staging; 0700 root so a
        # local user cannot read a copy of every row in the cluster.
        systemd.tmpfiles.settings.postgres-basebackup = {
          "/var/backup"."d" = {
            user = "root";
            group = "root";
            mode = "0700";
          };
          ${root}."d" = {
            user = "root";
            group = "root";
            mode = "0700";
          };
        };
      };
  };

  # --- RESTORE RUNBOOK ------------------------------------------------------
  #
  # The chain is physical, so it restores ONLY into postgresql 17. For a
  # cross-major restore use ./restic.nix's `postgres-<db>.sql` dumps instead.
  #
  # 1. Get the chain back. Every snapshot contains one complete chain, so the
  #    newest snapshot is always a legal starting point:
  #
  #      restic-b2 snapshots --tag postgres-basebackup
  #      restic-b2 restore latest --target /restore
  #      cd /restore/var/backup/postgres/chain
  #
  # 2. `CHAIN` lists the members, oldest first, and is the ONLY thing to trust:
  #    a snapshot caught mid-rotation can also hold members of the previous
  #    chain, and mixing the two is the one way to produce a corrupt result.
  #
  #      cat CHAIN
  #      # 20261004T020000Z-full
  #      # 20261005T020013Z-incr
  #      # ...
  #
  # 3. Re-verify before trusting it. These were verified when taken; this
  #    checks the round trip through B2 as well:
  #
  #      for m in $(cat CHAIN); do pg_verifybackup "$m"; done
  #
  # 4. Merge. Oldest first -- pg_combinebackup's argument order IS the chain
  #    order, and it refuses a chain whose manifests do not line up:
  #
  #      systemctl stop postgresql
  #      mv /var/lib/postgresql/17 /var/lib/postgresql/17.bad
  #      sudo -u postgres pg_combinebackup $(cat CHAIN) -o /var/lib/postgresql/17
  #
  # 5. Recover. `--wal-method=stream` made each member self-contained, so the
  #    WAL needed to reach a consistent state is already in the output's
  #    pg_wal/ and simply starting the server replays it from `backup_label`:
  #
  #      systemctl start postgresql
  #
  #    The `recovery.signal` + `restore_command` route is the PITR one, and it
  #    is NOT available here: nothing archives WAL, so there is no archive for
  #    a restore_command to read and no way to land between two members. The
  #    restore granularity is therefore ONE MEMBER -- 24 hours -- which is the
  #    same RPO ./restic.nix already has and is a deliberate scope boundary,
  #    not an oversight. Closing it means `archive_command`/`archive_library`
  #    writing into this same directory and is a separate change.
  #
  # 6. If step 3 or 4 rejects a member, every member after it is gone and the
  #    restore point is its parent. Truncate CHAIN at the last good member and
  #    run step 4 again; an older snapshot is the other option, and holds an
  #    older but independently complete chain.
  #
  # --- theDumpShouldBeWeekly ------------------------------------------------
  #
  # ./restic.nix dumps every database on every run, and that is now the biggest
  # avoidable thing in the nightly upload: this chain is the daily restore path
  # and the dump's only remaining job is surviving a postgres MAJOR upgrade,
  # which is not a daily event. Not applied here -- `databases` and `prepare`
  # belong to that collector and `backupPrepareCommand` is in its
  # `collectorOwned` list, so there is no seam -- so, as a patch:
  #
  #   in the `let` of ../services/restic.nix:
  #   +  # Weekly, not nightly. ./postgres-backup.nix's incremental chain is the
  #   +  # daily postgres restore path and ships only changed blocks; a plain-SQL
  #   +  # dump of every database is a FULL copy and the fastest-growing thing in
  #   +  # this snapshot. It is still taken, because it is the only copy that
  #   +  # survives a postgres major (./postgres.nix's `postgresql_17` note) --
  #   +  # just not seven times a week. `--keep-weekly 8` keeps eight of them.
  #   +  dumpStamp = "/var/lib/restic-b2/last-pg-dump";
  #   +  dumpEveryDays = 7;
  #
  #   in `prepare`, wrapping the `databases` loop:
  #   -  ${lib.concatMapStringsSep "\n" (db: ''
  #   -    ${runuser} -u postgres -- ${pgDump} ...
  #   -  '') databases}
  #   +  ${lib.optionalString (databases != [ ]) ''
  #   +    if [ ! -e ${dumpStamp} ] \
  #   +       || [ -n "$(find ${dumpStamp} -mtime +${toString (dumpEveryDays - 1)})" ]; then
  #   +      install -d -m 0700 "$(dirname ${dumpStamp})"
  #   +      ${lib.concatMapStringsSep "\n" (db: ''
  #   +        ${runuser} -u postgres -- ${pgDump} --format=plain ${lib.escapeShellArg db} \
  #   +          > ${staging}/postgres-${db}.sql
  #   +      '') databases}
  #   +      touch ${dumpStamp}
  #   +    else
  #   +      echo "restic: logical dumps are weekly (see dumpEveryDays); not in this snapshot" >&2
  #   +    fi
  #   ''}
  #
  # The cost of that patch, and it has to be written down next to it: six
  # snapshots in seven then contain no .sql, so a cross-major restore means
  # finding the newest snapshot that has one. The stamp lives outside `staging`
  # on purpose -- `prepare` wipes `staging` on every run.
  #
  # Also worth revisiting there, now that a PHYSICAL copy is the primary
  # restore path: `runCheck` is off, so nothing ever re-reads the repository.
  # A `restic-b2 check --read-data-subset=1/30` on its own slower timer was
  # already noted as missing in that file and matters more now -- restic dedups
  # across snapshots, so one lost pack file can damage many chains at once even
  # though no chain depends on another snapshot.
}
