# A systemd-tmpfiles equivalent for nix-darwin.
#
# nix-darwin has no tmpfiles mechanism (confirmed: no `systemd.*` options exist
# at all), so this compiles `auscybernix.tmpfiles.settings` -- the same schema
# NixOS feeds to systemd-tmpfiles -- into a nix-darwin activation script.
#
# ## Why this hooks `postActivation` and not a script of its own
#
# nix-darwin's `system.activationScripts` is `attrsOf` and `internal = true`, but
# the top-level runner (`modules/system/activation-scripts.nix`) interpolates a
# *hardcoded* list of script names in a fixed order. Defining a fresh
# `system.activationScripts.tmpfiles.text` therefore type-checks, evaluates, and
# then **never runs** -- nothing references it. Only three names in that list are
# documented as customisable, all defaulting to `""`:
#
#   preActivation    -- runs FIRST, before `groups` and `users`
#   extraActivation  -- runs after `checks`/`createRun`, still before `groups`/`users`
#   postActivation   -- runs LAST, after `groups`, `users`, `etc`, `launchd`, ...
#
# We default to `postActivation` because `chown` needs its target user/group to
# exist, and on darwin those are created by the `groups`/`users` activation steps
# that both earlier hooks precede. `phase` is exposed for rules that must land
# before a launchd daemon starts (which happens at `launchd`, before
# `postActivation`) -- at the cost of not being able to chown to a
# nix-darwin-managed user.
#
# ## Fidelity
#
# Activation runs as root and only at `darwin-rebuild` time -- there is no boot
# hook and no periodic timer. So: creation, mode and ownership are faithful;
# `age`-based clean-up is NOT implemented and raises an eval error rather than
# being silently dropped.
{
  config,
  lib,
  ...
}:
let
  tmpfiles = import ../../../lib/tmpfiles.nix { inherit lib; };
  inherit (tmpfiles) isUnset;

  cfg = config.auscybernix.tmpfiles;
  entries = tmpfiles.flatten cfg.settings;

  # tmpfiles.d(5) types this backend understands. Anything else is an eval error:
  # a rule that silently does nothing is worse than one that fails loudly, since
  # the caller believes a directory has been created.
  #
  #   d  create directory                     D  like d (see caveat below)
  #   f  create file if absent                F  create file, truncating it
  #   z  apply mode/ownership to existing     Z  ditto, recursively
  #
  # `D` normally also empties the directory at boot; with no boot hook to run it,
  # we treat it as `d` rather than emptying it on every rebuild -- silently
  # deleting a user's data on `darwin-rebuild switch` would be far worse than
  # under-implementing the type.
  creates = [
    "d"
    "D"
    "f"
    "F"
  ];
  adjustsOnly = [
    "z"
    "Z"
  ];
  supported = creates ++ adjustsOnly;

  recursive = e: e.type == "Z";

  # chown accepts "user:", ":group" and "user:group"; we only emit it when at
  # least one side is set, so a rule with neither leaves ownership untouched.
  ownerSpec =
    e:
    lib.optionalString (!isUnset e.user) e.user + ":" + lib.optionalString (!isUnset e.group) e.group;

  mkEntry =
    e:
    let
      p = lib.escapeShellArg e.path;
      r = lib.optionalString (recursive e) " -R";
      perms =
        lib.optionalString (!isUnset e.mode) "chmod${r} ${e.mode} ${p}\n"
        + lib.optionalString (!isUnset e.user || !isUnset e.group) "chown${r} ${ownerSpec e} ${p}\n";
    in
    if lib.elem e.type creates then
      # `install -d` would collapse mkdir+chmod+chown, but it does not exist with
      # consistent flags across the coreutils/BSD split on the activation PATH,
      # and it cannot express "leave mode alone". Keep the steps separate.
      (
        if e.type == "d" || e.type == "D" then
          "mkdir -p ${p}\n"
        else
          "mkdir -p ${lib.escapeShellArg (builtins.dirOf e.path)}\n"
          + (if e.type == "F" then ": > ${p}\n" else "[ -e ${p} ] || : > ${p}\n")
      )
      + perms
    else
      # z/Z adjust an existing path and must not create one.
      lib.optionalString (perms != "") ''
        if [ -e ${p} ]; then
        ${perms}fi
      '';

  # Group the generated fragments by their source config name, so the script
  # reads like the `settings` attrset that produced it.
  byConfig = lib.groupBy (e: e.config) entries;

  script = lib.concatStringsSep "\n" (
    lib.mapAttrsToList (name: es: ''
      # --- ${name} ---
      ${lib.concatStrings (map mkEntry es)}'') byConfig
  );

  badTypes = lib.filter (e: !(lib.elem e.type supported)) entries;
  agedEntries = lib.filter (e: !(isUnset e.age)) entries;
in
{
  options.auscybernix.tmpfiles = {
    settings = tmpfiles.settingsOption;

    phase = lib.mkOption {
      type = lib.types.enum [
        "preActivation"
        "extraActivation"
        "postActivation"
      ];
      default = "postActivation";
      description = ''
        Which nix-darwin activation hook the generated script is appended to.

        Defaults to `postActivation`, the only one that runs after the `groups`
        and `users` activation steps and can therefore `chown` to a
        nix-darwin-managed user or group.

        Use `preActivation` for paths a launchd daemon needs at startup -- the
        `launchd` step runs before `postActivation` -- but note that chowning to
        a user nix-darwin has not created yet will fail there.
      '';
    };
  };

  config = lib.mkIf (entries != [ ]) {
    assertions = [
      {
        assertion = badTypes == [ ];
        message = ''
          auscybernix.tmpfiles: the darwin backend does not implement tmpfiles type(s):
          ${lib.concatMapStringsSep "\n" (e: "  ${e.type}  ${e.path}  (in ${e.config})") badTypes}
          Supported here: ${lib.concatStringsSep " " supported}.
        '';
      }
      {
        assertion = agedEntries == [ ];
        message = ''
          auscybernix.tmpfiles: `age` is not implemented on darwin -- there is no
          systemd-tmpfiles timer to run the clean-up. Set age = "-" and, if you
          need expiry, declare a `launchd.daemons.<name>` job that prunes the path.
          Offending rule(s):
          ${lib.concatMapStringsSep "\n" (e: "  ${e.path} age=${e.age} (in ${e.config})") agedEntries}
        '';
      }
    ];

    # coreutils and gnugrep lead the activation PATH (nix-darwin's
    # `activationPath`), so mkdir/chmod/chown here are the GNU ones and take the
    # same flags the NixOS side would.
    system.activationScripts.${cfg.phase}.text = ''
      # auscybernix.tmpfiles
      ${script}
    '';
  };
}
