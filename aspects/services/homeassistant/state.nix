{ den, ... }:
# Where Home Assistant and the four ex-add-ons keep their state, and which of
# it leaves the box. Both halves in one file because they are two answers to
# the same question, and because `backup` is a single quirk value -- splitting
# it per-add-on would have two files defining one attribute, which den's
# one-level content merge resolves by replacement rather than by union.
{
  den.aspects.homeassistant = {
    # The recorder's backend. Declared on the quirk rather than by poking
    # `services.postgresql.ensureDatabases` directly: that option is a LIST, so
    # every service that touched it had to know what the others had already put
    # there (../nextcloud.nix's comment used to spell that coupling out).
    # Socket peer auth, so there is no password -- the unix user is `hass`.
    # ./core.nix's `recorder.db_url` is the other end.
    # THE DATABASE IS NAMED AFTER THE ACCOUNT, NOT THE ASPECT -- `hass`, where
    # the aspect is `homeassistant`. That gap is why ../postgres.nix's
    # `gives.postgres` resolves the asker's `user` capability rather than using
    # its name: deriving from the aspect name would have quietly asked for a
    # database called `homeassistant` that nothing connects to.
    #
    # Socket peer auth, so there is no password; ./core.nix's `recorder.db_url`
    # is the other end.
    takes.postgres.services = [ "home-assistant" ];

    # What HA and the four ex-add-ons keep on disk, for ../restic.nix.
    #
    #   .storage/            the registries. AREAS AND FLOORS LIVE ONLY HERE --
    #                        they have no YAML form and no database row, so
    #                        losing this directory loses the layout of the house
    #                        along with every device, entity, person and
    #                        long-lived access token.
    #   automations.yaml &c  what the UI editors write, which ./core.nix's
    #                        `!include`s read back.
    #   themes/              same, via `!include_dir_merge_named`.
    #
    # `/var/lib/private/...` for matter-server and music-assistant; esphome is
    # NOT one of them any more, see its entry below.
    # NOT `/var/lib/<name>`: both units run with `DynamicUser`, so
    # `/var/lib/<name>` is a SYMLINK into /var/lib/private -- and restic does
    # not follow symlinks, so it would faithfully store a link and nothing
    # behind it. ../restic.nix's `filesFrom` tests `[ -e "$p" ]`, which a
    # symlink passes, so this fails as a snapshot that succeeds with nothing in
    # it rather than as an error.
    backup =
      { config, ... }:
      let
        cfg = config.services.home-assistant.configDir;
      in
      {
        name = "hass";
        paths = [
          cfg
          # ESPHome's device YAML. Hand-written, not in this repo, and the
          # firmware of every node in the house is compiled from it.
          #
          # `/var/lib/esphome`. This said `/var/lib/private/esphome` because
          # the module WAS `DynamicUser`; on the locked nixpkgs it is not --
          # `stateDir = "/var/lib/esphome"` with a static `users.users.esphome`,
          # changed upstream because DynamicUser's symlink broke platformio
          # (nixpkgs#339557). So the snapshot pointed at a path that does not
          # exist and captured nothing, which is this file's own warned-about
          # failure mode turned on itself.
          "/var/lib/esphome"
          # The Matter fabric's operational credentials. Losing this means
          # re-commissioning every Matter device by hand.
          "/var/lib/private/matter-server"
          # Music Assistant's provider INSTANCES, which ./default.nix's header
          # and ../audiobookshelf.nix's both explain cannot be declared -- they
          # are typed into its UI and stored only here.
          "/var/lib/private/music-assistant"
          # The mosquitto password hashes ./mosquitto.nix tells you to make
          # with `mosquitto_passwd`. Nothing else knows them, and the broker
          # fails closed without them.
          "/var/lib/mosquitto-passwords"
        ];
        exclude = [
          # pip's runtime dependency tree. The package set that actually runs
          # comes from the store.
          "${cfg}/deps"
          # Cached TTS audio, regenerated on demand.
          "${cfg}/tts"
          # HA's own backup tarballs: already-compressed copies of what the rest
          # of this entry snapshots live, so they dedup badly and add nothing.
          "${cfg}/backups"
          # The bundled sqlite recorder, superseded by postgres above -- stale
          # if it exists at all, and a file-level copy of a live sqlite db is a
          # torn one regardless.
          "${cfg}/home-assistant_v2.db*"
          # ESPHome's platformio tree: toolchains and object files, every byte
          # re-derivable from the YAML beside it. Moves with the path above --
          # ../restic.nix refuses an exclude that is not under one of this
          # entry's own paths.
          "/var/lib/esphome/.esphome"
        ];
      };
  };
}
