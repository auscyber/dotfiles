{
  inputs,
  lib,
  den,
  ...
}:
let
  correct-inputs = inputs ? my-nur;
  ifTest = b: v: if b then v else { };

  # Chromium-flavor extension IDs.
  # IDs preserved from the old auscybernix.browsers.helium config, plus
  # Chromium equivalents for the addons the zen aspect installs.
  extensions = [
    # from the old helium module
    { id = "ldipcbpaocekfooobnbcddclnhejkcpn"; } # Decentraleyes (legacy id retained)
    { id = "cdglnehniifkbagbbombnjghhcihifij"; } # Kagi Search
    { id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa"; } # 1Password
    { id = "ekhagklcjbdpajgpjgmbionohlpdbjgc"; } # Zotero Connector
    { id = "lkoeejijapdihgbegpljiehpnlkadljb"; } # libKey Nomad
    { id = "hghakoefmnkhamdhenpbogkeopjlkpoa"; } # Lean Library
    # zen-equivalents
    { id = "jjkmbcponlojhhfgkpndlcimcdmegcpb"; } # Auto Tab Discard
    { id = "ddkjiahejlhfcafbddmgiahcphecmpfh"; } # uBlock Origin Lite (Chromium Manifest V3)
  ];

  # Chromium ManagedPolicies that map cleanly to the zen profile.
  policies = {
    DefaultSearchProviderEnabled = true;
    DefaultSearchProviderName = "Kagi Search";
    DefaultSearchProviderKeyword = "kagi";
    DefaultSearchProviderSearchURL = "https://kagi.com/search?q={searchTerms}";
    BrowserSignin = 0;
    SyncDisabled = true;
    PasswordManagerEnabled = false;
    PromotionalTabsEnabled = false;
    MetricsReportingEnabled = false;
    SafeBrowsingProtectionLevel = 1;
    SearchSuggestEnabled = false;
    SpellCheckServiceEnabled = false;
    UrlKeyedAnonymizedDataCollectionEnabled = false;
    HomepageLocation = "https://kagi.com";
    BookmarkBarEnabled = true;
  };
in
{
  flake-file.inputs.my-nur = {
    url = "github:auscyber/nur-packages";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  den.aspects.browsers.helium = {
    includes = [
      den.aspects.stylix
      den.aspects.packages.helium
    ];

    provides.to-hosts = {
      nixos.environment.etc."1password/custom_allowed_browsers" = {
        text = lib.mkAfter ''
          .helium-wrapped
          helium
          helium-browser
        '';
        mode = "0755";
      };
    };

    homeManager =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      ifTest correct-inputs {
        options.helium.profileName = lib.mkOption {
          type = lib.types.str;
          default = "ivy";
          description = "Display name for the helium profile (matches the zen aspect convention).";
        };

        config = {
          home.sessionVariables.BROWSER = lib.mkDefault "helium";

          programs.helium = {
            enable = true;
            inherit extensions;
          };

          # Chromium-family policies live in /etc/<browser>/policies on Linux,
          # and ~/Library/Application Support/<browser>/managed on darwin.
          home.file =
            let
              policiesFile = {
                text = builtins.toJSON policies;
              };
              linuxPath = ".config/helium/policies/managed/dendritic.json";
              darwinPath = "Library/Application Support/Helium/managed/dendritic.json";
            in
            if pkgs.stdenv.hostPlatform.isDarwin then
              { "${darwinPath}" = policiesFile; }
            else
              { "${linuxPath}" = policiesFile; };
        };
      };
  };
}
