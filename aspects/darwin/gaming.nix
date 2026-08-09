{ den, ... }: {
  den.aspects.darwin-gaming = {
    includes = [ den.aspects.homebrew ];
    brew = {
      taps.apple = "apple/homebrew-apple";
      brews = [ "apple/apple/game-porting-toolkit" ];
    };
    darwin =
      { pkgs, ... }:
      {
        environment.systemPackages = [
          (pkgs.writeShellApplication {
            name = "gpt-init-prefix";
            text = ''
              # Bootstraps a Wine prefix for Apple's Game Porting Toolkit:
              # copies the runtime libraries out of the mounted GPT .dmg
              # (Finder must already have it open, per Apple's install steps)
              # and opens winecfg so you can pick Windows 10 as the prefix OS
              # -- that dialog is interactive, there's no headless flag for it.
              prefix="''${GPT_PREFIX:-$HOME/my-game-prefix}"
              gpt_root="$(brew --prefix game-porting-toolkit)"

              volume="$(find /Volumes -maxdepth 1 -iname 'Game Porting Toolkit*' -print -quit)"
              if [[ -z "$volume" ]]; then
              	echo "Mount the Game Porting Toolkit .dmg (from developer.apple.com) first, then re-run." >&2
              	exit 1
              fi

              echo "Copying GPT runtime libraries from '$volume' into '$gpt_root/lib' ..."
              ditto "$volume/lib/" "$gpt_root/lib/"

              echo "Opening winecfg for '$prefix' -- select 'Windows 10' as the OS, then close it."
              WINEPREFIX="$prefix" arch -x86_64 "$gpt_root/bin/wine64" winecfg

              echo "Prefix ready. Install a Windows app into it with:"
              echo "  gameportingtoolkit '$prefix' /path/to/installer.exe"
            '';
          })
        ];
      };

  };
}
