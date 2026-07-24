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
      wallpaper = ../../backgrounds/phoebebridgers-2.jpg;
      flakeFolder = "/Users/ivypierlot/dendritic";
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGtwIOzVblYtrx014SIyldn8EhdwxzSVRXGMz5LLtunc ivypierlot@Ivys-MacBook-Pro";
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
      den.aspects.ccache
      den.aspects.sccache
      den.aspects.lix
      #      den.aspects.builders
    ];

    provides.to-users.homeManager.programs.git.enable = true;
    vpn = { };

    darwin =
      {
        pkgs,
        lib,
        ...
      }:
      {
        stylix.targets.jankyborders.enable = false;
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

        # `gc.automatic` on its own runs `nix-collect-garbage` with no arguments,
        # which only drops paths nothing refers to. Every system generation is a
        # GC root, so without `--delete-older-than` the old ones pin their whole
        # closure forever and the weekly run reclaims almost nothing.
        nix.gc = {
          automatic = true;
          options = "--delete-older-than 14d";
          interval = {
            Hour = 3;
            Minute = 15;
            Weekday = 7;
          };
        };

        # Let the daemon collect mid-build instead of only once a week: when free
        # space drops under min-free it GCs until max-free is available again.
        # This machine runs close to full, so the weekly timer alone is too coarse.
        nix.settings = {
          min-free = 5 * 1024 * 1024 * 1024;
          max-free = 25 * 1024 * 1024 * 1024;
          # Keeping .drv files alive costs space and only buys offline rebuilds
          # of things already built; not worth it on a space-constrained laptop.
          keep-derivations = false;
        };

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
          "/Applications/Nix Apps/Zen.app"
          "/Applications/Microsoft Outlook.app"
          "/Applications/Fantastical.app"
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
  };

  den.aspects.laptop-brew = {
    includes = [ den.aspects.homebrew ];
    brew = {
      brews = [
        "mole"
        "speedtest"
      ];
      casks = [
        "discord"
        "typewhisper"
        "craft"
        "raycast"
        "calibre"
        "sf-symbols"
        "font-sf-pro"
        "todoist-app"
        "dockdoor"
        "beeper"
        "amethyst"
        "steam"
        #        "affinity-designer"
        #        "affinity-publisher"
        "plover"
        "postman"
        #        "tidal"
        #        "zoom"
      ];
    };
  };

  den.aspects.ivypierlot = {
    study.includes = [ den.aspects.zotero ];
    includes = [
      den.aspects.homebrew
      #      den.aspects.zed
      den.aspects.idris
      den.aspects.agenix-rekey
      den.aspects.onepassword
      den.aspects.nixvim
      den.aspects.neovim
      #      den.aspects.zed
      # Runs the lspmux server; nixvim and zed both spawn their servers through its
      # shims, so without this the shims have nothing to connect to.
      den.aspects.lspmux
      den.aspects.fish
      den.aspects.celler-push
      den.aspects.nushell
      den.aspects.ghostty
      den.aspects.sketchybar
      <browsers/zen>
      den.aspects.gui
      den.aspects.gpg
      #      den.aspects.rclone
      den.aspects.rift
      den.aspects.kanata
      den.aspects.dev
      den.aspects.opencode
      den.aspects.openclaw
      den.aspects.llama-cpp
      #      den.aspects.zeroclaw
      den.aspects.file-local
      den.batteries.primary-user
      den.aspects.cotabby
      #      <zen>
    ];
    provides.Ivys-MacBook-Pro.includes = [ den.aspects.laptop-brew ];
    provides.Ivys-MacBook-Pro.provides.to-users = {
      homeManager = { pkgs, ... }: {
        # Off the 8080 default so it doesn't collide with anything else that
        # assumes that port; cotabby (and any future local client) reads this
        # same option rather than a hardcoded port.
        programs.llama-cpp.port = 8090;

        home.packages = with pkgs; [
          nodejs
          opencode
          vscode
          pandoc
          # Medium over Full: Full costs ~1.4 GB of closure for packages this
          # machine never pulls in. If a document wants something Medium lacks,
          # prefer adding that package explicitly over going back to Full.
          texliveMedium
          mupdf
          # qemu dropped: nix.linux-builder already brings qemu-host-cpu-only,
          # which covers the aarch64-linux builder VM. Re-add the full package
          # only if you need to run VMs for other architectures by hand.
          input-leap
          pinentry_mac
        ];

        services.yubikey-agent.enable = false;
        #        programs.discord.enable = true;

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

        # Laptop SSH hosts + 1Password agent socket. auspc/secondpc come from
        # den.aspects.vpn-ssh-config (aspects/modules/network/vpn/ssh-config.nix),
        # pulled in automatically via den.aspects.vpn.includes.
        programs.ssh.settings = {
          "faggot.sh" = {
            hostname = "faggot.sh";
            user = "ivy";
            forwardAgent = true;
          };
          "imflo.pet" = {
            hostname = "imflo.pet";
            user = "ivy";
            forwardAgent = true;
          };
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
  };
}
