{pkgs, ...}:
let super = pkgs;
in
{
 programs.alacritty =  {
 
    enable = true;
    package = (pkgs.alacritty.overrideAttrs (old: rec {
#     src = old.src;
    src = pkgs.fetchFromGitHub {
      owner = "zenixls2";
      repo = "alacritty";
      rev = "ligature";
      sha256 = "2Th2aojTN36MgYSFXiACcBkTpTou/X1Ub5JR2sgZa34=";
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
      outputHash = "XMvLUAL25X1CL70EL0ziyhsK9fz6N1XGQ5aLyAeEWkk=";
    }); 
    
    }));
    settings = {
      window.padding = {
	x = 10;
	y = 10;
      };
      background_opacity = 0.9;
      colors = {
  # Default colors
  primary = {
    background = "0x161616";
    foreground = "0xffffff";
};
  # Normal colors
  normal = {
    black=   "0x222222";
    red=     "0xe84f4f";
    green =   "0xb7ce42";
    yellow=  "0xfea63c";
    blue=    "0x66aabb";
    magenta= "0xb7416e";
    cyan=    "0x6d878d";
    white=   "0xdddddd";
  };
  # Bright co"ors
  bright = {
    black =   "0x666666";
    red =     "0xd23d3d";
    green =    "0xbde077";
    yellow =  "0xffe863";
    blue =    "0xaaccbb";
    magenta = "0xe16a98";
    cyan =    "0x42717b";
    white =   "0xcccccc";	
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
  };
  }



