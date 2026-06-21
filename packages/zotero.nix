{
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

  };
  den.aspects.zotero = {

    overlays = { sources, ... }: {
      zotero-extensions = self: super: {
        zotero-extensions = {
          zotero-better-bibtex = super.fetchFirefoxAddon {
            name = "zotero";
            inherit (sources.zotero-better-bibtex) src;
          };
          zotero-attanger = super.fetchFirefoxAddon {
            name = "zotero-attanger";
            inherit (sources.zotero-attanger) src;
          };
        };

      };
    };

  };
}
