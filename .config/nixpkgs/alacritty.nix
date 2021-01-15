{pkgs, ...}:
let super = pkgs;
in
{

 
    enable = true;
    package = (pkgs.alacritty.overrideAttrs (old: rec {
    src = pkgs.fetchFromGitHub {
      owner = "zenixls2";
      repo = "alacritty";
      rev = "ligature";
      sha256 = "1wcqlnd2vss7p3b92xgvi22h31y29xv26hcnrv7b9z5p54aqmx8y";
    };
    installPhase =
      ''
        runHook preInstall
        install -D $releaseDir/alacritty $out/bin/alacritty
        install -D extra/linux/Alacritty.desktop -t $out/share/applications/
        install -D extra/logo/compat/alacritty-term.svg $out/share/icons/hicolor/scalable/apps/Alacritty.svg
        # patchelf generates an ELF that binutils' "strip" doesn't like:
        #    strip: not enough room for program headers, try linking with -N
        # As a workaround, strip manually before running patchelf.
        strip -S $out/bin/alacritty
        patchelf --set-rpath "${pkgs.lib.makeLibraryPath old.buildInputs}:${super.stdenv.cc.cc.lib}/lib${super.stdenv.lib.optionalString super.stdenv.is64bit "64"}" $out/bin/alacritty
        installShellCompletion --zsh extra/completions/_alacritty
        installShellCompletion --bash extra/completions/alacritty.bash
        installShellCompletion --fish extra/completions/alacritty.fish
        install -dm 755 "$out/share/man/man1"
        gzip -c extra/alacritty.man > "$out/share/man/man1/alacritty.1.gz"
        install -Dm 644 alacritty.yml $out/share/doc/alacritty.yml
        install -dm 755 "$terminfo/share/terminfo/a/"
        tic -xe alacritty,alacritty-direct -o "$terminfo/share/terminfo" extra/alacritty.info
        mkdir -p $out/nix-support
        echo "$terminfo" >> $out/nix-support/propagated-user-env-packages
        runHook postInstall
      '';
    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "IYrr2X2fUn2wxiM1sjnceaIGg+yKIfruz3BRMyI+yts=";
    }); 
    
    }));
    settings = {
      window.padding = {
	x = 10;
	y = 10;
      };
      background_opacity = 0.9;
      colors = {
	primary = {
	    background= "0x1d1f21";
	    foreground= "0xc5c8c6";

	};
	normal = {
	    black=   "0x282a2e";
	    red=     "0xa54242";
	    green=   "0x7c9440";
	    yellow=  "0xde935f" ;
	    blue =    "0x5f819d" ;
	    magenta =  "0x85678f" ;
	    cyan =    "0x5e8d87" ;
	    white =   "0x707880" ;


	};
	bright = {
	      black =   "0x373b41";
	      red =  "0xcc6666";
	      green=   "0xb5bd68";
	      yellow=  "0xf0c674";
	      blue=    "0x81a2be";
	      magenta= "0xb294bb";
	      cyan=    "0x8abeb7";
	      white=   "0xc5c8c6";

	};

      };
      font = {
	size = 10;
	ligatures = true;
	normal = {
	  family = "Hasklug Nerd Font";
	  style = "Regular";
	};
      };
    };
  }



