{ den, ... }:
{
  den.aspects.zotero = {
    homeManager =
      { pkgs, ... }:
      {
        programs.zotero = {
          enable = true;
          # Must mirror the existing profiles.ini: mkFirefoxModule regenerates it
          # from these declarations, and Zotero's real profile is Profile1
          # "Default User" -> Profiles/x3xvsrif.default.
          profiles."Default User" = {
            id = 0;
            path = "x3xvsrif.default";
            isDefault = true;
            settings.extensions.update.autoUpdateDefault = false;
            extensions.packages = with pkgs.zoteroAddons; [
              better-bibtex
              attanger
              actions-tags
              notero
              ocr
              aria
              papersgpt
              zotlit
            ];
          };
        };
        home.packages = with pkgs; [
          tesseract
          poppler-utils
        ];
      };
  };
}
