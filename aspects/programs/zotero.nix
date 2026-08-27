{
  den,
  lib,
  ...
}:
let
  # `pkgs.tesseract` defaults to `enableLanguages = null`, which bundles the
  # full `tessdata` set (~1 GB). Zotero only OCRs English here.
  tesseractFor =
    pkgs:
    pkgs.tesseract.override {
      enableLanguages = [
        "eng"
        "osd"
      ];
    };
in
{
  den.aspects.zotero = {
    includes = [ den.aspects.homebrew ];
    brew.casks = [ "zotero" ];
    homeManager = { pkgs, ... }: {
      programs.zotero = {
        enable = true;
        # Must mirror the existing profiles.ini: mkFirefoxModule regenerates it
        # from these declarations, and Zotero's real profile is Profile1
        # "Default User" -> Profiles/x3xvsrif.default.
        profiles."Default User" = {
          id = 0;
          path = "x3xvsrif.default";
          isDefault = true;
          settings = lib.flattenAttrset {
            extensions.zotero.zoteroocr = {
              pdftoppmPath = lib.getExe' pkgs.poppler-utils "pdftoppm";

              ocrPath = lib.getExe (tesseractFor pkgs);
            };
            extensions.update.autoUpdateDefault = false;
            # Zotero 7 local API (localhost:23119) for zotero-mcp's local mode.
            extensions.zotero.httpServer.enabled = true;
            extensions.settings.extensions.zotero.httpServer.localAPI.enabled = true;
          };
          extensions.packages = with pkgs.zoteroAddons; [
            better-bibtex
            better-notes
            attanger
            actions-tags
            notero
            ocr
            zotlit
          ];
        };
      };
      home.packages = [
        (tesseractFor pkgs)
        pkgs.poppler-utils
      ];
    };
  };
  perSystem = { pkgs, ... }: {
    packages.percollate =
      (pkgs.percollate.override {
        chromium = pkgs.helium;
      }).overrideAttrs
        (attrs: {
          postInstall = ''
            wrapProgram $out/bin/percollate \
            	--set PUPPETEER_EXECUTABLE_PATH ${pkgs.helium}/bin/helium
          '';
        });
  };
}
