{pkgs,lib, ...}:

{
   enable = true;
   experimentalBackends = true;
   fade = false;
   shadow = true;
   shadowOffsets = [(-30) (-30)];
   shadowOpacity = "0.5";
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
    "class_g = 'Polybar'"
]
round-borders = 1;


    '';
}