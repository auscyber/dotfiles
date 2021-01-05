{pkgs,lib, ...}:

{
   enable = true;
   experimentalBackends = true;
   fade = false;
   shadow = false;
   shadowOffsets = [(-30) (-30)];
   shadowOpacity = "0.25";
   shadowExclude = ["name = 'xmonad'"];
   vSync = true;
   refreshRate = 60;
   package = (pkgs.picom.overrideAttrs (attrs: {
	  src = builtins.fetchTarball {
        url = "https://github.com/jonaburg/picom/archive/next.tar.gz";
	};

   }));
   extraOptions = ''
# Corners
corner-radius = 10.0;
rounded-corners-exclude = [
    "class_g = 'Polybar'",
    "class_g = 'Minecraft'"
]
round-borders = 1;


    '';
}
