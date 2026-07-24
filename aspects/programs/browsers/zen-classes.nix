{
  lib,
  den,
  ...
}:
let
  # Programmatically-derived pin set, regenerated from the Obsidian vault by
  # `nix run .#update-zen-classes` (see ./gen-zen-classes.sh). Read purely at eval.
  classes = builtins.fromJSON (builtins.readFile ./zen-classes.json);

  # The Zen "Uni" space + "Classes" folder. The zen-browser module owns the
  # folder (declared as a group pin) and its class children in one place, so
  # every id is consistent and Zen renders them straight from the session file
  # the module writes (zen-sessions.jsonlz4). No places.sqlite poking.
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
        config,
        lib,
        ...
      }:
      let
        profile = config.zen.profileName;

        # One child pin per subject, nested under the "Classes" folder via
        # folderParentId. Each carries its own workspace/container (flat form:
        # top-level pins don't inherit from the folder pin).
        classPins = lib.listToAttrs (
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
      in
      {
        # Declarative pins via the zen-browser module. It writes the "Classes"
        # folder and its children into zen-sessions.jsonlz4; Zen must be closed
        # during `home-manager switch` (the activation script needs exclusive
        # access to that file — it skips while Zen is running).
        programs.zen-browser.profiles.${profile} = {
          pins = {
            # The module owns the folder too, so its id matches the children's
            # folderParentId (previously the folder was created by hand in Zen
            # with a different internal id, which is why pins never rendered).
            Classes = {
              id = classFolder;
              title = "Classes";
              isGroup = true;
              editedTitle = true;
              workspace = uniSpace;
              position = 999;
            };
          }
          // classPins;
          # profile-global; keep false so unrelated pins (authentik, GitHub CLI)
          # in other folders are left untouched rather than force-removed.
          pinsForce = false;
        };
      };
  };
}
