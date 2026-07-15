{
  # Every addon here is an upstream GitHub release xpi. The `addonId` in the
  # overlay below must match `manifest.applications.zotero.id` inside the xpi —
  # fetchZoteroAddon asserts this at build time, so a silent upstream id change
  # fails the build instead of quietly not loading in Zotero.
  nvfetcher.sources = {
    zotero-better-bibtex = {
      src.github = "retorquere/zotero-better-bibtex";
      src.prefix = "v";
      fetch.url = "https://github.com/retorquere/zotero-better-bibtex/releases/download/v$ver/zotero-better-bibtex-$ver.xpi";
    };

    zotero-attanger = {
      src.github = "MuiseDestiny/zotero-attanger";
      fetch.url = "https://github.com/MuiseDestiny/zotero-attanger/releases/download/$ver/zotero-attanger.xpi";
    };

    zotero-actions-tags = {
      src.github = "windingwind/zotero-actions-tags";
      src.prefix = "v";
      fetch.url = "https://github.com/windingwind/zotero-actions-tags/releases/download/v$ver/actions-and-tags-for-zotero.xpi";
    };

    # Built from source (pnpm build + create-xpi), so this fetches the repo rather
    # than a release xpi, and tracks the tip of the default branch (main): $ver is
    # the commit, which doubles as the fetchFromGitHub rev.
    zotero-notero = {
      src.git = "https://github.com/dvanoni/notero.git";
      src.branch = "main";
      fetch.github = "dvanoni/notero";
    };

    zotero-ocr = {
      src.github = "UB-Mannheim/zotero-ocr";
      fetch.url = "https://github.com/UB-Mannheim/zotero-ocr/releases/download/$ver/zotero-ocr-$ver.xpi";
    };

    # Tags are inconsistently prefixed (v0.7.5, 0.8.0), so $ver is the tag itself.
    zotero-aria = {
      src.github = "lifan0127/ai-research-assistant";
      fetch.url = "https://github.com/lifan0127/ai-research-assistant/releases/download/$ver/aria.xpi";
    };

    # Tag and asset are both the full "papersgpt-vX.Y.Z" string.
    zotero-papersgpt = {
      src.github = "papersgpt/papersgpt-for-zotero";
      fetch.url = "https://github.com/papersgpt/papersgpt-for-zotero/releases/download/$ver/$ver.xpi";
    };

    # The repo also releases an Obsidian plugin; the Zotero addon lives on the
    # "zt-*" tags (the bare semver tags are Obsidian builds with no xpi).
    zotero-zotlit = {
      src.github_tag = "aidenlx/zotlit";
      src.include_regex = "zt-.*";
      src.prefix = "zt-";
      fetch.url = "https://github.com/aidenlx/zotlit/releases/download/zt-$ver/zotlit-zotero-$ver.xpi";
    };
  };

  den.aspects.zotero = {
    overlays = { sources, ... }: {
      zotero-addons = final: prev: {
        fetchZoteroAddon = prev.callPackage ./_fetch-zotero-addon.nix { };

        # Built from source; already installs into share/zotero/extensions, so it
        # sits alongside the fetchZoteroAddon outputs below.

        zoteroAddons = {
          notero = prev.callPackage ./_notero.nix { source = sources.zotero-notero; };
        }
        // prev.lib.mapAttrs (_: spec: final.fetchZoteroAddon spec) {
          better-bibtex = {
            addonId = "better-bibtex@iris-advies.com";
            inherit (sources.zotero-better-bibtex) pname version src;
          };
          attanger = {
            addonId = "zoteroattanger@polygon.org";
            inherit (sources.zotero-attanger) pname version src;
          };
          actions-tags = {
            addonId = "zoterotag@euclpts.com";
            inherit (sources.zotero-actions-tags) pname version src;
          };
          ocr = {
            addonId = "zotero-ocr@bib.uni-mannheim.de";
            inherit (sources.zotero-ocr) pname version src;
          };
          aria = {
            addonId = "aria@apex974.com";
            inherit (sources.zotero-aria) pname version src;
          };
          papersgpt = {
            addonId = "papersgpt@papersgpt.com";
            inherit (sources.zotero-papersgpt) pname version src;
          };
          zotlit = {
            addonId = "zotlit@aidenlx.site";
            inherit (sources.zotero-zotlit) pname version src;
          };
        };
      };
    };
  };
}
