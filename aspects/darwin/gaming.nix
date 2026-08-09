{ den, ... }: {
  den.aspects.darwin-gaming = {
    includes = [ den.aspects.homebrew ];
    # apple/apple/game-porting-toolkit (Apple's own formula) currently fails
    # to resolve its dependency graph -- it pins openssl@1.1, which Homebrew
    # has since disabled/removed, so `brew bundle` hard-errors on it with
    # "Could not get runtime dependencies". gcenx/wine's game-porting-toolkit
    # is an actively-maintained rebuild that ships a prebuilt wine64 as a
    # cask (no compiling, no Apple-Developer-gated download).
    brew = {
      taps.gcenx = "gcenx/homebrew-wine";
      casks = [ "gcenx/wine/game-porting-toolkit" ];
    };
    darwin =
      { pkgs, config, ... }:
      {
        nix-homebrew = {
          enable = true;
          enableRosetta = true;
          user = config.system.primaryUser;
          trust.taps = builtins.attrNames config.nix-homebrew.taps;
          trust.casks = [ "gcenx/wine/game-porting-toolkit" ];
          mutableTaps = false;
          autoMigrate = true;
        };
        environment.systemPackages = [
          (pkgs.writeShellApplication {
            name = "gpt-init-prefix";
            text = ''
              # Bootstraps a Wine prefix for the gcenx/wine game-porting-toolkit
              # cask. Its wine64 needs Rosetta (nix-homebrew.enableRosetta,
              # aspects/darwin/homebrew.nix, already turns that on) and the
              # cask's own postflight handles de-quarantining/codesigning the
              # app, so this only needs to run winecfg.
              wine64="/Applications/Game Porting Toolkit.app/Contents/Resources/wine/bin/wine64"
              prefix="''${GPT_PREFIX:-$HOME/my-game-prefix}"

              if [[ ! -x "$wine64" ]]; then
              	echo "wine64 not found at '$wine64' -- is the game-porting-toolkit cask installed?" >&2
              	exit 1
              fi

              echo "Opening winecfg for '$prefix' -- select 'Windows 10' as the OS, then close it."
              WINEPREFIX="$prefix" arch -x86_64 "$wine64" winecfg

              echo "Prefix ready. Install/run a Windows app in it with:"
              echo "  WINEPREFIX='$prefix' arch -x86_64 '$wine64' /path/to/installer.exe"
            '';
          })
        ];
      };
  };
}
