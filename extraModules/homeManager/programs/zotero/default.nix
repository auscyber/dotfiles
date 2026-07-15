{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  cfg = config.programs.zotero;
  modulePath = [
    "programs"
    "zotero"
  ];
  # Zotero keeps profiles.ini next to a Profiles/ dir on darwin, and under
  # ~/.zotero/zotero on Linux (not ~/.zotero, which is the vendor dir).
  linuxConfigPath = ".zotero/zotero";
  darwinConfigPath = "Library/Application Support/Zotero";
  mkFirefoxModule = import "${inputs.home-manager.outPath}/modules/programs/firefox/mkFirefoxModule.nix";
in
{
  imports = [
    (mkFirefoxModule {
      inherit modulePath;
      name = "Zotero";
      wrappedPackageName = "zotero";
      unwrappedPackageName = "zotero";
      platforms = {
        linux = {
          vendorPath = ".zotero";
          configPath = linuxConfigPath;
        };
        darwin = {
          configPath = darwinConfigPath;
          darwinDefaultsId = "org.zotero.zotero";
        };
      };
    })
  ];

  config = lib.mkIf cfg.enable {
    # mkFirefoxModule points <profile>/extensions at
    # "${env}/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}",
    # the Firefox vendor layout. Zotero has no such dir: it scans
    # <profile>/extensions directly for <addon-id>.xpi, which is where
    # fetchZoteroAddon installs (share/zotero/extensions). Override the link.
    #
    # Dropping xpis into that dir is necessary but not sufficient: Zotero (like
    # Gecko) only *loads* add-ons recorded in <profile>/extensions.json, the
    # XPIDatabase cache. A profile with no such record leaves the scanned xpis
    # either ignored or parked as disabled sideloads. So we also generate an
    # extensions.json that registers each pinned add-on as active, deriving its
    # metadata (id, version, compat range, ...) from the xpi's own manifest.json.
    home.file = lib.mkMerge (
      lib.flip lib.mapAttrsToList cfg.profiles (
        _: profile:
        let
          hasExtensions = profile.extensions.packages != [ ];

          extensionsEnvPkg = pkgs.buildEnv {
            name = "hm-zotero-extensions";
            paths = profile.extensions.packages;
          };

          # Where the profile's extensions actually resolve at runtime. path and
          # rootURI in extensions.json must point here (the symlinked profile
          # location), matching what Zotero records for a file-installed add-on.
          profileDir = "${config.home.homeDirectory}/${cfg.profilesPath}/${profile.path}";

          # schemaVersion mirrors what the running Zotero last wrote; a mismatch
          # triggers a DB migration that would try to rewrite this read-only file.
          extensionsJson =
            pkgs.runCommand "zotero-extensions-json"
              {
                nativeBuildInputs = [
                  pkgs.unzip
                  pkgs.jq
                ];
              }
              ''
                extdir=${lib.escapeShellArg "${extensionsEnvPkg}/share/zotero/extensions"}
                profileDir=${lib.escapeShellArg profileDir}
                ts=1700000000000

                # One add-on record. `.` is the xpi's manifest.json; the rest come
                # in as --args. active/userDisabled/foreignInstall are what make
                # Zotero treat the add-on as installed-and-enabled rather than a
                # pending sideload. strictCompatibility is off so a pinned add-on
                # whose declared max lags the running Zotero still loads.
                cat > entry.jq <<'JQEOF'
                {
                  id: .applications.zotero.id,
                  syncGUID: $syncGUID,
                  version: .version,
                  type: "extension",
                  loader: null,
                  updateURL: (.applications.zotero.update_url // null),
                  installOrigins: null,
                  manifestVersion: (.manifest_version // 2),
                  optionsURL: null,
                  optionsType: null,
                  optionsBrowserStyle: true,
                  aboutURL: null,
                  defaultLocale: {
                    name: (.name // .applications.zotero.id),
                    description: (.description // null),
                    creator: (.author // null),
                    homepageURL: (.homepage_url // null),
                    developers: null,
                    translators: null,
                    contributors: null
                  },
                  visible: true,
                  active: true,
                  userDisabled: false,
                  appDisabled: false,
                  embedderDisabled: false,
                  installDate: ($ts | tonumber),
                  updateDate: ($ts | tonumber),
                  applyBackgroundUpdates: 1,
                  path: $path,
                  skinnable: false,
                  sourceURI: null,
                  releaseNotesURI: null,
                  softDisabled: false,
                  foreignInstall: false,
                  strictCompatibility: false,
                  locales: [],
                  targetApplications: [
                    {
                      id: "zotero@zotero.org",
                      minVersion: (.applications.zotero.strict_min_version // "6.999"),
                      maxVersion: (.applications.zotero.strict_max_version // "*")
                    }
                  ],
                  targetPlatforms: [],
                  signedState: 0,
                  signedTypes: [],
                  signedDate: null,
                  seen: true,
                  dependencies: [],
                  incognito: "spanning",
                  userPermissions: { permissions: [], origins: [], data_collection: [] },
                  optionalPermissions: { permissions: [], origins: [], data_collection: [] },
                  requestedPermissions: { permissions: [], origins: [], data_collection: [] },
                  icons: (.icons // {}),
                  iconURL: null,
                  blocklistAttentionDismissed: false,
                  blocklistState: 0,
                  blocklistURL: null,
                  startupData: null,
                  hidden: false,
                  installTelemetryInfo: { source: "about:addons", method: "install-from-file" },
                  recommendationState: null,
                  rootURI: $rootURI,
                  location: "app-profile"
                }
                JQEOF

                n=0
                for xpi in "$extdir"/*.xpi; do
                  [ -e "$xpi" ] || continue
                  id=$(basename "$xpi" .xpi)
                  path="$profileDir/extensions/$id.xpi"
                  # jar: URIs are URLs, so encode the one path char Zotero's own
                  # darwin profile ("Application Support") contains: the space.
                  encoded=$(printf '%s' "$path" | sed 's/ /%20/g')
                  rootURI="jar:file://$encoded!/"
                  # Stable per-id GUID (sync is unused, but the field is expected).
                  guid="{$(printf '%s' "$id" | md5sum | sed -E 's/^(.{8})(.{4})(.{4})(.{4})(.{12}).*/\1-\2-\3-\4-\5/')}"

                  unzip -p "$xpi" manifest.json \
                    | jq \
                        --arg syncGUID "$guid" \
                        --arg path "$path" \
                        --arg rootURI "$rootURI" \
                        --arg ts "$ts" \
                        -f entry.jq \
                    > "entry-$n.json"
                  n=$((n + 1))
                done

                if [ "$n" -eq 0 ]; then
                  echo '{"schemaVersion":37,"addons":[]}' > "$out"
                else
                  jq -s '{ schemaVersion: 37, addons: . }' entry-*.json > "$out"
                fi
              '';
        in
        {
          "${cfg.profilesPath}/${profile.path}/extensions" = lib.mkIf hasExtensions (
            lib.mkForce {
              source = "${extensionsEnvPkg}/share/zotero/extensions";
              recursive = true;
              force = true;
            }
          );

          # Read-only, so Zotero can't add/remove/update add-ons through its UI;
          # the pinned set here is the source of truth and is reasserted on every
          # home-manager switch.
          "${cfg.profilesPath}/${profile.path}/extensions.json" = lib.mkIf hasExtensions {
            source = extensionsJson;
            force = true;
          };
        }
      )
    );

    # Zotero itself is not from nixpkgs here (Homebrew cask on darwin), and
    # wrapFirefox is linux-only. Setting package = null makes mkFirefoxModule
    # manage the profile only; the darwinDefaultsId assertion covers this.
    programs.zotero.package = lib.mkDefault (
      if pkgs.stdenv.hostPlatform.isDarwin then
        null
      else
        pkgs.wrapFirefox (pkgs.zotero.overrideAttrs (attrs: {
          passthru = (attrs.passthru or { }) // {
            applicationName = "Zotero";
            binaryName = "zotero";
          };
          pname = "zotero";
          dontFixup = false;
        })) { }
    );
  };
}
