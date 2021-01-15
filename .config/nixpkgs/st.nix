self: super: with super;{
  st = st.overrideAttrs (oldAttrs: rec {
    version = "0.8.4";
    buildInputs = oldAttrs.buildInputs ++ [harfbuzz xorg.libXrender ];
     patches = [
     (fetchpatch {
       url = "https://st.suckless.org/patches/ligatures/0.8.3/st-ligatures-20200430-0.8.3.diff";
       sha256 = "vKiYU0Va/iSLhhT9IoUHGd62xRD/XtDDjK+08rSm1KE=";
     })
    (fetchpatch {
      url = "http://st.suckless.org/patches/clipboard/st-clipboard-0.8.3.diff";
      sha256 = "y7N2dem0mGg2wZqtrMYWoAbfgcm/OU6eNXPhZPoYZ88=";
    })
     (fetchpatch {
       url = "http://st.suckless.org/patches/xresources/st-xresources-20200604-9ba7ecf.diff";
          sha256 = "8HV66XrTJu80H0Mwws5QL7BV6L9omUH6avFJqdDC7as=";
     })
      (./st/st-alpha-0.8.2.diff)
     (fetchpatch {
       url = "http://st.suckless.org/patches/alpha/st-alpha-0.8.2.diff";
          sha256 = "pOHiIBwoTG4N9chOM7ORD1daDHU/z92dVKzmt9ZIE5U=";      })

     ];
configFile = writeText "config.def.h" (builtins.readFile /home/auscyber/st/config.h);      

  });
}
