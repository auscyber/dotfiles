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
    includes = [
      den.aspects.homebrew
      den.aspects.agenix-rekey
    ];
    brew.casks = [ "zotero" ];

    # The Nextcloud app password Zotero syncs files with. Hand-managed rather
    # than generated: `occ` on secondpc is the only thing that can issue one
    # (see `packages.zotero-webdav-token` below), so it is not derived from
    # anything this repo holds. Nested under this aspect's own scope by
    # ../security/age-scope.nix, so it lands at `age.secrets."zotero/webdav"`.
    secrets.webdav.rekeyFile = ../../secrets/zotero/webdav.age;
    homeManager =
      {
        pkgs,
        scoped,
        ...
      }:
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
            settings = lib.flattenAttrset {
              extensions.zotero.zoteroocr = {
                pdftoppmPath = lib.getExe' pkgs.poppler-utils "pdftoppm";

                ocrPath = lib.getExe (tesseractFor pkgs);
              };
              extensions.update.autoUpdateDefault = false;
              # File sync against the Nextcloud on secondpc
              # (../services/nextcloud.nix). Zotero appends `zotero/` to this URL
              # and refuses to sync if that folder is missing, and the username is
              # part of the path -- which is why that instance's account is `ivy`.
              extensions.zotero.sync.storage = {
                protocol = "webdav";
                scheme = "https";
                url = "cloud.ivymect.in/remote.php/dav/files/ivy";
                username = "ivy";
              };
              # The password is a Nextcloud APP password, not the account
              # password. Zotero keeps it in the OS keystore and offers no way
              # to put it there but the sync pane, so the `webdav-password`
              # plugin below reads this file at startup and calls setPassword
              # itself.
              #
              # Only the DECRYPTED path is named here -- the token itself never
              # reaches the store or a pref file.
              extensions.zotero.webdavPassword = {
                enable = true;
                file = scoped.zotero.secrets.webdav.path;
              };
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
      # Mint the Nextcloud app password Zotero syncs with and store it as the
      # `zotero/webdav` secret the pref above points at. `occ` is the only
      # thing that can issue one and it lives on secondpc, so this is the one
      # credential here that is fetched rather than generated locally.
      #
      # It mints a NEW token every run rather than reading an existing one back
      # (Nextcloud stores only a hash), so run it deliberately. The old token
      # stays valid until revoked in Settings -> Security.
      packages.zotero-webdav-token = pkgs.writeShellApplication {
        name = "zotero-webdav-token";
        runtimeInputs = [
          pkgs.openssh
          pkgs.coreutils
          # `agenix edit`, wrapped by ../security/agenix-rekey.nix.
          config.packages.secret-edit
        ];
        text = ''
          host=''${ZOTERO_WEBDAV_SSH_HOST:-secondpc}
          account=''${ZOTERO_WEBDAV_ACCOUNT:-ivy}
          label=''${ZOTERO_WEBDAV_LABEL:-zotero}

          # `agenix edit` resolves the secret through the flake in the working
          # directory, so this only works from a checkout.
          if [ ! -e flake.nix ]; then
            echo "zotero-webdav-token: run this from the repo root" >&2
            exit 1
          fi

          # Two lines out: "app password:" then the token. No account password
          # is supplied -- an OIDC-provisioned user has none, and a token
          # minted without one still covers WebDAV (it only loses the
          # operations that need the login password, which is also why
          # server-side encryption must stay off).
          token=$(ssh "$host" \
            sudo nextcloud-occ user:auth-tokens:add "$account" --name "$label" \
            | tr -d '\r' | tail -n1)

          if [ -z "$token" ]; then
            echo "zotero-webdav-token: occ returned no token" >&2
            exit 1
          fi

          # `agenix edit` hands $EDITOR the decrypted temp file, so an $EDITOR
          # that copies stdin over it stores the value without the token ever
          # appearing in a command line or on the terminal.
          printf '%s' "$token" \
            | EDITOR='sh -c "cat > \"$1\"" --' secret-edit secrets/zotero/webdav.age

          echo "stored a new '$label' app password for $account" >&2
          echo "now run: nix run .#rekey" >&2
        '';
      };

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
