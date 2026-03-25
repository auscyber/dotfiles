{config, pkgs, lib, ...}:
{

options.auscybernix.wms.niri = {
	enable = lib.mkEnableOption "Enable Niri Wayland compositor";
	};

	config = lib.mkIf config.auscybernix.wms.niri.enable {
		programs.niri = {
			enable = true;
			settings = {

			};
			#extraSessionCommands = ''
			#	nm-applet &
			#'';
		};
	};
}
