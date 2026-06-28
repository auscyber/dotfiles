{ den, ... }:
{
  den.aspects.zotero = {
    homeManager =
      { pkgs, ... }:
      {
        programs.zotero = {
          enable = false;
          profiles."bla".extensions.packages = with pkgs.zotero-extensions; [
            zotero-attanger
            zotero-better-bibtex
          ];
        };
        home.packages = with pkgs; [
          tesseract
          poppler-utils
        ];
      };
  };
}
