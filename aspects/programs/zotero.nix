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

  # ── WHERE IT SYNCS, RESOLVED FROM THIS HOST ──
  # The server that gives `webdav-provisioned` here, and this edge's
  # realisation: the account name, the DAV path, and the credential both ends
  # read.
  syncFor =
    host:
    let
      cands = den.lib.capability.resolveFrom host "webdav-provisioned" {
        name = null;
        carrier = null;
        within = null;
        sameHost = null;
        where = null;
      };
      cand = lib.head cands;
    in
    if cands == [ ] then
      null
    else
      {
        link = den.lib.capability.linkFor cand.via "webdav-provisioned" {
          inherit host;
          peer = den.aspects.zotero;
        };
        r = den.lib.capability.realiseOn "webdav-provisioned" {
          inherit cand;
          taker = den.aspects.zotero;
          from = host;
        };
      };
in
{
  # ── THE LIBRARY'S FILES, SHARED WITH WHOEVER INCLUDES IT ──
  # A groupfolder mounted as `zotero` because Zotero appends `zotero/` to its
  # URL; the service accounts below join its group, so they and ivy see one
  # folder.
  den.aspects.zotero-library.takes.share.displayName = "zotero";
  den.persons.ivy.includes = [ den.aspects.zotero-library ];

  den.aspects.zotero = {
    includes = [
      den.aspects.homebrew
      den.aspects.agenix-rekey
    ];
    brew.casks = [ "zotero" ];

    # Its own Nextcloud account per host, password minted here and provisioned
    # on the server (../services/nextcloud.nix's `nextcloud-service-accounts`).
    takes.webdav-provisioned = {
      account = "zotero";
      shares = [ "zotero-library" ];
    };
    # The credential's account on this side. home-manager's agenix has no
    # `owner`, so only the name matters.
    gives =
      { self, ... }:
      {
        user = _: {
          name = self.name;
          carrier = self.name;
        };
      };

    homeManager =
      {
        pkgs,
        scoped,
        host,
        ...
      }:
      let
        sync = syncFor host;
        domain = den.aspects.publish-ivymect-in.domain;
      in
      {
        age.scoped = lib.optionalAttrs (sync != null) {
          ${sync.r.scope}.secrets = lib.mapAttrs (_: e: removeAttrs e [ "owner" ]) sync.r.takerSecrets;
        };

        programs.zotero = {
          enable = true;
          # Must mirror the existing profiles.ini: mkFirefoxModule regenerates it
          # from these declarations, and Zotero's real profile is Profile1
          # "Default User" -> Profiles/x3xvsrif.default.
          profiles."Default User" = {
            id = 0;
            path = "x3xvsrif.default";
            isDefault = true;
            # File sync as this host's own service account. Zotero appends
            # `zotero/` to the URL, which is the shared library's mount. The
            # password never reaches a pref: the `webdav-password` addon reads
            # the decrypted file at startup and puts it in Zotero's keystore.
            settings = lib.flattenAttrset (
              lib.recursiveUpdate
                (lib.optionalAttrs (sync != null) {
                  extensions.zotero.sync.storage = {
                    protocol = "webdav";
                    scheme = "https";
                    url = "${sync.link.subdomain}.${domain}${sync.link.davPath}";
                    inherit (sync.link) username;
                  };
                  extensions.zotero.webdavPassword = {
                    enable = true;
                    file = sync.r.taker.pathIn scoped;
                  };
                })
                {
                  extensions.zotero.zoteroocr = {
                    pdftoppmPath = lib.getExe' pkgs.poppler-utils "pdftoppm";

                    ocrPath = lib.getExe (tesseractFor pkgs);
                  };
                  extensions.update.autoUpdateDefault = false;
                  # Zotero 7 local API (localhost:23119) for zotero-mcp's local mode.
                  extensions.zotero.httpServer.enabled = true;
                  extensions.settings.extensions.zotero.httpServer.localAPI.enabled = true;
                }
            );
            extensions.packages = with pkgs.zoteroAddons; [
              better-bibtex
              better-notes
              attanger
              actions-tags
              notero
              ocr
              webdav-password
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
  perSystem =
    {
      pkgs,
      config,
      ...
    }:
    {
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
