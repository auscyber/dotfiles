self: super:

{
  picom = super.picom.overrideAttrs   (attrs: {
   src = builtins.fetchTarball {
        url = "https://github.com/jonaburg/picom/archive/next.tar.gz";
      };
       });

}
