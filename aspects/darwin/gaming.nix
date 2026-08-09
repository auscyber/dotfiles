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
              # Bootstraps a Wine prefix for Apple's Game Porting Toolkit.
              #
              # The brew formula (apple/apple/game-porting-toolkit, tapped
              # above) only provides the compiled wine64 binary. The actual
              # D3DMetal runtime libs and the `gameportingtoolkit*` launcher
              # scripts ship separately in Apple's login-gated "Evaluation
              # environment for Windows games" download -- a .dmg *nested
              # inside* the outer Game Porting Toolkit.dmg, which Finder does
              # not auto-mount; double-click it from the outer volume first.
              # (developer.apple.com/games/game-porting-toolkit)
              prefix="''${GPT_PREFIX:-$HOME/my-game-prefix}"
              gpt_root="$(brew --prefix game-porting-toolkit)"

              volume="$(find /Volumes -maxdepth 1 -iname 'Evaluation environment for Windows games*' -print -quit)"
              if [[ -z "$volume" ]]; then
                echo "Mount the nested 'Evaluation environment for Windows games' .dmg first (double-click it inside the Game Porting Toolkit volume), then re-run." >&2
                exit 1
              fi

              echo "Copying GPT runtime libraries from '$volume' into '$gpt_root/lib' ..."
              ditto "$volume/lib/" "$gpt_root/lib/"

              echo "Installing gameportingtoolkit* launcher scripts into '$gpt_root/bin' ..."
              cp "$volume"/gameportingtoolkit* "$gpt_root/bin/"
              chmod +x "$gpt_root"/bin/gameportingtoolkit*

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
