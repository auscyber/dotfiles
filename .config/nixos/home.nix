{ config, pkgs, ... }:
let haskellPacks = with pkgs.haskellPackages; [  haskell-language-server ];

in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [

  #Development
  jetbrains.idea-ultimate jdk
  xclip
  polybarFull  git nodejs  playerctl htop   
  #Wow
  fish rofi  feh starship maim discord spotify 
  (alacritty.overrideAttrs (old: rec {
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
      outputHash = "1nya7qi36lbhrzpgl8caxj1hd8krvhwv4d93qsq7sllzgpcyp2i1";
    }); 
    
    }))

 (picom.overrideAttrs (attrs: {
   src = builtins.fetchTarball {
        url = "https://github.com/jonaburg/picom/archive/next.tar.gz";
      };
       }))

  ] ++ haskellPacks;


  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "auscyber";
  home.homeDirectory = "/home/auscyber";
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
