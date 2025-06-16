{config,pkgs,inputs,...}:
let
	inherit (pkgs) stdenv lib;
in
{
programs.zen-browser = {
    enable = true;
	package = pkgs.lib.mkForce (
      if pkgs.stdenv.hostPlatform.isDarwin then
	  null
      else
        pkgs.zen-browser
    );
    policies = {
      DisableAppUpdate = true;
      DisableTelemetry = true;
      # find more options here: https://mozilla.github.io/policy-templates/
    };
	profiles."ivy.Default (twilight)" = {
	isDefault = true;
	settings = {
	"extensions.autoDisableScopes" = 0;
	"zen.welcome-screen.seen" = true;
	};
	containers = {
Uni = {
    color = "green";
    icon = "fruit";
    id = 2;
  };
  shopping = {
    color = "blue";
    icon = "cart";
    id = 1;
  };
	};
	extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
	onepassword-password-manager zotero-connector ublock-origin
	];

	};
  };
home.activation = lib.optionalAttrs stdenv.hostPlatform.isDarwin {
    zen-browser =
      let
        profiles-ini =
          if stdenv.hostPlatform.isLinux then
            "${config.xdg.configHome}/zen/profiles.ini"
          else
            "${config.home.homeDirectory}/Library/\"Application Support\"/zen/profiles.ini";
      in
      inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        rm ${profiles-ini}.backup
        mv ${profiles-ini} ${profiles-ini}.generate
        cat ${profiles-ini}.generate > ${profiles-ini}
        echo ZenAvatarPath=chrome://browser/content/zen-avatars/avatar-01.svg >> ${profiles-ini}
      '';
  };
}
