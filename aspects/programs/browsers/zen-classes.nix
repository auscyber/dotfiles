{
  inputs,
  lib,
  den,
  ...
}:
let
  # Programmatically-derived pin set, regenerated from the Obsidian vault by
  # `nix run .#update-zen-classes` (see ./gen-zen-classes.sh). Read purely at eval.
  classes = builtins.fromJSON (builtins.readFile ./zen-classes.json);

  # The existing Zen "Uni" space + "Classes" folder are referenced by uuid; we
  # attach class pins as children, never redeclaring the space/folder itself.
  uniSpace = "a82108ff-512d-43d2-af2b-25f8aacb8026";
  classFolder = "cfaf0e66-4af3-443d-a2ad-b7884ff8da30";
  uniContainerId = 2;
in
{
  # `nix run .#update-zen-classes` — refresh aspects/programs/browsers/zen-classes.json
  # from the Obsidian vault (current active-term subjects + those starting within a week).
  perSystem =
    { pkgs, lib, ... }:
    {
      apps.update-zen-classes = {
        type = "app";
        program = lib.getExe (
          pkgs.writeShellApplication {
            name = "update-zen-classes";
            runtimeInputs = with pkgs; [
              coreutils
              jq
              yq-go
              util-linux
              gnugrep
              gnused
              findutils
            ];
            text = builtins.readFile ./gen-zen-classes.sh;
          }
        );
      };
    };

  den.aspects.zen-classes = {
    # Only the study laptop carries classes (same role gate as study.zen-browser in zen.nix).
    study.homeManager =
      {
        pkgs,
        config,
        lib,
        ...
      }:
      let
        profile = config.zen.profileName;
        isDarwin = pkgs.stdenv.hostPlatform.isDarwin;

        # Authoritative applier for this DB-based Zen build: reconcile the "Classes"
        # folder in places.sqlite from the same committed JSON. Skips while Zen runs.
        reconciler = pkgs.writeShellApplication {
          name = "zen-classes-reconcile";
          runtimeInputs = with pkgs; [
            sqlite
            coreutils
          ];
          text = ''
            profdir="$HOME/Library/Application Support/zen/Profiles/${profile}"
            db="$profdir/places.sqlite"
            lock="$profdir/.parentlock"
            folder='{${classFolder}}'
            space='{${uniSpace}}'
            json='${./zen-classes.json}'

            [ -f "$db" ] || { echo "zen-classes: no places.sqlite; skip."; exit 0; }
            if /usr/bin/lsof "$lock" >/dev/null 2>&1 || /usr/bin/pgrep -x zen >/dev/null 2>&1; then
              echo "zen-classes: Zen is running; close it and rebuild to update Uni -> Classes pins."
              exit 0
            fi

            now_ms="$(date +%s%3N)"
            now_s="$(date +%s)"

            # Replace the "Classes" folder's children with the committed class list.
            # SQLite reads the JSON itself (json_each + readfile), so there is no string
            # building and titles/urls can never break the SQL. Columns/positions match
            # the live schema: (uuid,title,url,container_id=2,workspace,position=key+1,
            # is_essential=0,is_group=0,parent_uuid=NULL,created,updated,0,0,NULL,folder).
            {
              echo "BEGIN IMMEDIATE;"
              echo "INSERT INTO zen_pins_changes(uuid,timestamp) SELECT uuid,$now_s FROM zen_pins WHERE folder_parent_uuid='$folder' ON CONFLICT(uuid) DO UPDATE SET timestamp=excluded.timestamp;"
              echo "DELETE FROM zen_pins WHERE folder_parent_uuid='$folder';"
              echo "INSERT INTO zen_pins (uuid,title,url,container_id,workspace_uuid,position,is_essential,is_group,parent_uuid,created_at,updated_at,edited_title,is_folder_collapsed,folder_icon,folder_parent_uuid) SELECT '{' || (je.value ->> 'uuid') || '}', je.value ->> 'title', je.value ->> 'url', 2, '$space', je.key + 1, 0, 0, NULL, $now_ms, $now_ms, 0, 0, NULL, '$folder' FROM json_each(CAST(readfile('$json') AS TEXT)) AS je;"
              echo "INSERT INTO zen_pins_changes(uuid,timestamp) SELECT uuid,$now_s FROM zen_pins WHERE folder_parent_uuid='$folder' ON CONFLICT(uuid) DO UPDATE SET timestamp=excluded.timestamp;"
              echo "COMMIT;"
              echo "PRAGMA wal_checkpoint(TRUNCATE);"
            } | sqlite3 "$db"

            echo "zen-classes: reconciled Uni -> Classes pins."
          '';
        };
      in
      {
        # (1) Declarative pins via the zen-browser module (keeps zen-sessions.jsonlz4 in sync).
        programs.zen-browser.profiles.${profile} = {
          pins = lib.listToAttrs (
            lib.imap0 (
              i: e:
              lib.nameValuePair e.subjectId {
                title = e.title;
                id = e.uuid;
                url = e.url;
                container = uniContainerId;
                workspace = uniSpace;
                folderParentId = classFolder;
                position = 1000 + i;
                isEssential = false;
              }
            ) classes
          );
          # profile-global; keep false so unrelated pins (authentik, GitHub CLI) survive.
          pinsForce = false;
        };

        # (2) Reconcile places.sqlite (Darwin only) after HM writes everything.
        home.activation = lib.mkIf isDarwin {
          zen-classes-reconcile = inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] (
            lib.getExe reconciler
          );
        };
      };
  };
}
