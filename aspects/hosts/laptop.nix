{
  den,
  __findFile,
  ...
}:
{

  den.hosts.aarch64-darwin.Ivys-MacBook-Pro = {

    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICTsjq9lMzer6RPeDfXZ9eI1eiMf8b/fteSOb5XC5rBG";
    roles = [
      "study"
      "gui"
      "dev"
    ];
    users.ivypierlot = {
      flakeFolder = "/Users/ivypierlot/dendritic";
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H";
      roles = [
        "study"
        "gui"
        "dev"
      ];
    };
  };

  den.aspects.Ivys-MacBook-Pro = {
    includes = [
      den.aspects.vpn
      den.aspects.homebrew
      den.aspects.karabiner-driver
      den.aspects.sudoagents
      den.aspects.builders
    ];

    provides.to-users.homeManager.programs.git.enable = true;
    vpn = { };

    darwin =
      { pkgs, lib, ... }:
      {
        system.primaryUser = "ivypierlot";
        stylix = {
          enable = true;
          polarity = "dark";
        };
        stylix.targets.jankyborders.enable = false;
        programs._1password-gui.enable = true;
        services.karabiner-elements.enable = false;
        users.users.ivypierlot = {
          name = "ivypierlot";
          home = "/Users/ivypierlot";
        };

        environment.shells = [
          pkgs.bash
          pkgs.zsh
          pkgs.fish
        ];

        nix.gc.automatic = true;

        # Local linux builder VM (ephemeral) for cross-building x86_64/aarch64-linux.
        nix.distributedBuilds = true;

        # "assessment" specialisation: an exam/lockdown profile that disables the
        # window manager, keyboard remapper, status bar, window borders and VPN.
        # Boot it with `darwin-rebuild switch --specialisation assessment` (or the
        # equivalent activation), matching the old config's behaviour.
        specialisation.assessment.configuration = {
          services.karabiner-dk.enable = lib.mkForce false;
          networking.wg-quick.interfaces = lib.mkForce { };

          home-manager.users.ivypierlot = {
            services.rift.enable = lib.mkForce false;
            services.jankyborders.enable = lib.mkForce false;
            programs.kanata.enable = lib.mkForce false;
            programs.sketchybar.enable = lib.mkForce false;
          };
        };

        system.defaults.dock.persistent-apps = [
          #          "/Applications/Nix Apps/Zen.app"
          "/Applications/Microsoft Outlook.app"
          "/Applications/Fantastical.app"
          "/Applications/Notion.app"
          "/Applications/Microsoft Word.app"
          "/System/Applications/Messages.app"
          "/Applications/Beeper Desktop.app"
          "/Applications/1Password.app"
          "/System/Applications/System Settings.app"
          "/Applications/Nix Apps/Visual Studio Code.app"
          "/Applications/Zotero.app"
          "/Applications/Nix Apps/Ghostty.app"
          "/Applications/Todoist.app"
        ];
      };

    brew = {
      brews = [
        "mole"
        "speedtest"
      ];
      casks = [
        "jordanbaird-ice"
        "typewhisper"
        "mark-text"
        "craft"
        "raycast"
        "calibre"
        "sf-symbols"
        "font-sketchybar-app-font"
        "font-sf-pro"
        "todoist-app"
        "dockdoor"
        "beeper"
        "amethyst"
        "steam"
        "google-drive"
        "tailscale"
        "affinity-designer"
        "zotero"
        "affinity-publisher"
        "helium-browser"
        "plover"
        "postman"
        "tidal"
        "zoom"
        "microsoft-teams"
      ];
    };
  };

  den.aspects.ivypierlot = {
    includes = [
      den.aspects.agenix-rekey
      den.aspects.nixvim
      den.aspects.neovim
      den.aspects.fish
      den.aspects.nushell
      den.aspects.ghostty
      den.aspects.sketchybar
      den.aspects.zotero
      <browsers/helium>
      den.aspects.gui
      den.aspects.gpg
      #      den.aspects.rclone
      den.aspects.rift
      den.aspects.kanata
      den.aspects.dev
      den.aspects.file-local
      den.batteries.primary-user
      den.aspects.packages.cotabby
      #      <zen>
    ];

    provides.Ivys-MacBook-Pro.homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          cotabby
          claude-code
          nodejs
          opencode
          vscode
          pandoc
          texliveFull
          mupdf
          obsidian
          qemu
          input-leap
          pinentry_mac
        ];

        services.yubikey-agent.enable = true;
        programs.discord.enable = true;

        # 1Password SSH-key commit signing (mirrors the old home config).
        programs.git.signing = {
          format = "ssh";
          signByDefault = true;
          key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOuXMdca6Lz0Rxz+EmKy/cSXuBev6knlsdKzm7R5D4E1";
          signer = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
        };
        programs.jujutsu.settings = {
          signing = {
            behavior = "drop";
            backend = "ssh";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOuXMdca6Lz0Rxz+EmKy/cSXuBev6knlsdKzm7R5D4E1";
            backends.ssh.program = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
          };
          git.sign-on-push = true;
        };

        # Laptop SSH hosts + 1Password agent socket.
        programs.ssh.settings = {
          "*".identityAgent = "~/Library/Group\\ Containers/2BUA8C4S2C.com.1password/t/agent.sock";
          "auspc" = {
            forwardAgent = true;
            hostname = "192.168.0.24";
            user = "auscyber";
          };
          "secondpc".forwardAgent = true;
          "faggot.sh".forwardAgent = true;
        };

        # macOS per-user defaults carried over from the old home config.
        targets.darwin.defaults.NSGlobalDomain = {
          AppleIconAppearanceCustomTintColor = "0.593048 1.000000 0.728584 0.596341";
          AppleInterfaceStyle = "Dark";
          AppleShowAllFiles = true;
          ApplePressAndHoldEnabled = false;
          InitialKeyRepeat = 10;
          KeyRepeat = 3;
        };
      };
  };
}
