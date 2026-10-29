{
  den,
  lib,
  config,
  ...
}:
let
  inherit (den.lib.policy) route;
  inherit (lib) types mkOption;

  files = {
    main = "main.js";
    manifest = "manifest.json";
    styles = "styles.css";
  };
  filesOf =
    p:
    [
      "main"
      "manifest"
    ]
    ++ lib.optional p.styles "styles";
  sourceName = id: file: "obsidian-${id}-${file}";

  pluginType = types.submodule {
    options = {
      repo = mkOption {
        type = types.strMatching "[^/]+/[^/]+";
        description = "GitHub `owner/repo` whose releases carry the plugin's loose files.";
      };
      styles = mkOption {
        type = types.bool;
        default = true;
        description = "Whether the release ships a `styles.css`.";
      };
      tagPrefix = mkOption {
        type = types.str;
        default = "";
      };
      src = mkOption {
        type = types.attrsOf (types.either types.str types.bool);
        default = { };
        description = "Replaces the default nvchecker `src.github = repo` when set.";
      };
      enable = mkOption {
        type = types.bool;
        default = true;
      };
      versionCheck = mkOption {
        type = types.bool;
        default = true;
        description = "Fail the build when `manifest.json`'s version differs from the release tag.";
      };
      settings = mkOption {
        type = types.attrsOf types.anything;
        default = { };
        description = "Enforced: merged over the plugin's writable `data.json` on every switch.";
      };
      defaults = mkOption {
        type = types.attrsOf types.anything;
        default = { };
        description = "Seeded: written into `data.json` only where the key is absent, then left to the UI.";
      };
    };
  };

  nvfetcherSources = lib.concatMapAttrs (
    id: p:
    lib.listToAttrs (
      map (file: {
        name = sourceName id file;
        value = {
          src =
            if p.src != { } then
              p.src
            else
              { github = p.repo; } // lib.optionalAttrs (p.tagPrefix != "") { prefix = p.tagPrefix; };
          fetch.url = "https://github.com/${p.repo}/releases/download/${p.tagPrefix}$ver/${files.${file}}";
        };
      }) (filesOf p)
    )
  );

  buildPlugin =
    pkgs: sources: id: p:
    let
      src = file: sources.${sourceName id file};
      main = src "main";
    in
    pkgs.stdenvNoCC.mkDerivation {
      pname = "obsidian-plugin-${id}";
      inherit (main) version;
      dontUnpack = true;
      nativeBuildInputs = [ pkgs.jq ];
      installPhase = ''
        runHook preInstall
        ${lib.concatMapStrings (file: ''
          install -Dm444 ${(src file).src} "$out/${files.${file}}"
        '') (filesOf p)}
        id=$(jq -er .id "$out/manifest.json")
        version=$(jq -er .version "$out/manifest.json")
        if [ "$id" != ${lib.escapeShellArg id} ]; then
          echo "obsidian plugin: manifest id '$id' != '${id}'" >&2
          exit 1
        fi
        if ${lib.boolToString p.versionCheck} && [ "$version" != ${lib.escapeShellArg main.version} ]; then
          echo "obsidian plugin ${id}: manifest version '$version' != release '${main.version}'" >&2
          exit 1
        fi
        runHook postInstall
      '';
      passthru.manifestId = id;
      meta.platforms = lib.platforms.all;
    };
in
{
  options.obsidian.plugins = mkOption {
    type = types.attrsOf pluginType;
    default = { };
  };

  config.obsidian.plugins = config.flake.obsidianPlugins or { };
  config.nvfetcher.sources = nvfetcherSources config.obsidian.plugins;

  config.den.lib.obsidian = {
    inherit pluginType nvfetcherSources buildPlugin;
  };

  config.den.classes.obsidian-plugins.description =
    "Obsidian community plugins, fetched via nvfetcher";

  config.den.policies.obsidian-plugins-to-flake = _: [
    (route {
      fromClass = "obsidian-plugins";
      intoClass = "flake";
      path = [
        "flake"
        "obsidianPlugins"
      ];
      collectSubtree = true;
    })
  ];
  config.den.schema.flake-system.includes = [ den.policies.obsidian-plugins-to-flake ];

  config.den.policies.obsidian-plugins-to-homeManager = _: [
    (route {
      fromClass = "obsidian-plugins";
      intoClass = "homeManager";
      path = [
        "obsidian"
        "plugins"
      ];
      collectSubtree = true;
    })
  ];
  config.den.aspects.obsidian = {
    includes = [ den.policies.obsidian-plugins-to-homeManager ];

    obsidian-plugins = {
      templater-obsidian = {
        repo = "silentvoid13/Templater";
        settings = {
          templates_folder = "Uni/_Templates";
          startup_templates = [
            "Uni/_Templates/Update weeks.md"
            "Uni/_Templates/Sync Collections.md"
            "Uni/_Templates/Sync Todoist.md"
          ];
        };
      };
      dataview = {
        repo = "blacksmithgu/obsidian-dataview";
        # The 0.5.70 release ships a manifest still saying 0.5.68.
        versionCheck = false;
        settings = {
          enableDataviewJs = true;
          enableInlineDataviewJs = true;
          refreshInterval = 2500;
        };
      };
      obsidian-git.repo = "vinzent03/obsidian-git";
      editing-toolbar.repo = "pkm-er/obsidian-editing-toolbar";
      table-editor-obsidian.repo = "tgrosinger/advanced-tables-obsidian";
      omnisearch = {
        repo = "scambier/obsidian-omnisearch";
        settings = {
          useCache = true;
          ignoreDiacritics = true;
        };
      };
      sqlite-db = {
        repo = "stfrigerio/sqliteDB";
        styles = false;
      };
      tag-wrangler = {
        repo = "pjeby/tag-wrangler";
        styles = false;
      };
      notion-bases.repo = "bgarciamoura/obsidian-notion-bases-plugin";
      pretty-properties.repo = "anareaty/pretty-properties";
      colored-bases-properties.repo = "rafjaf/obsidian-colored-bases-properties";
      dataview-properties = {
        repo = "mara-li/obsidian-dataview-properties";
        styles = false;
      };
      obsidian-meta-bind-plugin.repo = "mprojectscode/obsidian-meta-bind-plugin";
      js-engine.repo = "mprojectscode/obsidian-js-engine-plugin";
      # The bare semver tags are the Obsidian builds; `zt-*` is the Zotero xpi, `res-*` schemas.
      zotlit = {
        repo = "aidenlx/zotlit";
        src = {
          github_tag = "aidenlx/zotlit";
          include_regex = "^[0-9]+\\.[0-9]+\\.[0-9]+.*";
        };
        settings = {
          "server.enabled" = true;
          "note.literature-folder" = "Uni/Readings";
          "template.filename" = "<%= zt.citationKey %>";
        };
      };
      folder-notes.repo = "lostpaul/obsidian-folder-notes";
      make-md = {
        repo = "make-md/makemd";
        enable = false;
      };
      realclaudian = {
        repo = "yishentu/claudian";
        enable = false;
      };
    };

    homeManager =
      {
        config,
        pkgs,
        lib,
        sources,
        ...
      }:
      let
        cfg = config.obsidian;
        json = pkgs.formats.json { };
        vaults = lib.filter (v: v.enable) (lib.attrValues config.programs.obsidian.vaults);
        configDir =
          if pkgs.stdenv.hostPlatform.isDarwin then
            "${config.home.homeDirectory}/Library/Application Support/obsidian"
          else
            "${config.xdg.configHome}/obsidian";
        merge =
          path: name: p:
          lib.hm.generators.mkImpureConfigMerger {
            inherit pkgs path;
            format = "json";
            empty = "{}";
            jqOperation = "($static.defaults * $dynamic) * $static.settings";
            staticSettings = json.generate name { inherit (p) defaults settings; };
          };
      in
      {
        options.obsidian.plugins = mkOption {
          type = types.attrsOf pluginType;
          default = { };
        };

        config = {
          programs.obsidian = {
            enable = true;
            cli.enable = true;
            vaults.work.target = "Work/Work";
            defaultSettings.communityPlugins = lib.mapAttrsToList (id: p: {
              pkg = buildPlugin pkgs sources id p;
              inherit (p) enable;
            }) cfg.plugins;
          };

          home.activation.obsidianPluginSettings = lib.hm.dag.entryAfter [ "linkGeneration" "obsidian" ] (
            lib.concatMapStrings (
              vault:
              lib.concatStrings (
                lib.mapAttrsToList (
                  id: p:
                  merge "${config.home.homeDirectory}/${vault.target}/.obsidian/plugins/${id}/data.json"
                    "${id}-data.json"
                    p
                ) (lib.filterAttrs (_: p: p.settings != { } || p.defaults != { }) cfg.plugins)
              )
            ) vaults
          );

          # Catalyst beta asars come through the app's own updater, which HM's template switches off.
          home.activation.obsidianInsider = lib.hm.dag.entryAfter [ "obsidian" ] (
            merge "${configDir}/obsidian.json" "obsidian-insider.json" {
              defaults = { };
              settings = {
                insider = true;
                updateDisabled = false;
              };
            }
          );
        };
      };
  };
}
