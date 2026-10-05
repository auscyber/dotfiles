{
  den,
  __findFile,
  ...
}:
# ── NO CELLER INSIDE A SPECIALISATION, AND IT IS NOT A PREFERENCE ──
#
# `celler-user`'s secrets resolve through `celler.use celler-caches celler-use`
# (../base/celler/_lib.nix), and `celler-caches` is a FLEET-WIDE collect:
# `den.policies.celler-caches` is a `pipe.collectAll`, so it reads every other
# host's record -- celler2's and secondpc's. A specialisation is evaluated over
# a PRIVATE ONE-ENTITY FLEET: ../../lib/specialisations.nix replaces the
# flake-system root's fleet-wide includes with this host's own two effects. So
# the collect comes back EMPTY inside one and `use` throws:
#
#   error: celler-use: no celler server `celler2` (servers: )
#
# -- measured when the take below still named celler2; it reads `secondpc` now
# and fails identically, because the EMPTY server list is the fault and not the
# name in it.
#
# Measured: of the four profiles below only `no-celler` evaluated; `assessment`,
# `study` and `life-project` all died on that throw, while the base
# configuration is fine. It is not a celler being down -- a celler cannot be
# RESOLVED from inside a specialisation at all until that one-entity fleet can
# see the rest of the house.
#
# It is also right on its own terms: an exam profile, a study profile and a dock
# have no business pushing NARs to a cache. And `nh-checked` is unaffected -- it
# picks the SMALLEST profile covering the aspects whose checks failed, so
# `no-celler` stays the cover for a dead celler.
let
  noCeller = [
    den.aspects.celler-host
    den.aspects.celler-user
  ];
in
{
  den.hosts.aarch64-darwin.Ivys-MacBook-Pro = {
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICTsjq9lMzer6RPeDfXZ9eI1eiMf8b/fteSOb5XC5rBG";
    roles = [
      "study"
      "gui"
      "dev"
      "gaming"
    ];
    # Whose machine this is -- a different fact from the household it serves.
    owner = den.persons.ivy;

    users.ivypierlot = {
      # The human behind this account (../../persons/ivy.nix). The link
      # points account -> person, never the reverse; ../../base/person.nix says why.
      person = den.persons.ivy;
      wallpaper = ../../backgrounds/phoebebridgers-2.jpg;
      flakeFolder = "/Users/ivypierlot/dendritic";
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGtwIOzVblYtrx014SIyldn8EhdwxzSVRXGMz5LLtunc ivypierlot@Ivys-MacBook-Pro";
      roles = [
        "study"
        "gui"
        "dev"
        "gaming"
      ];
    };
  };

  den.aspects.Ivys-MacBook-Pro = {
    # NO FLEET-DEFAULT CELLER WIRING. The one celler relationship this machine
    # has is stated explicitly, as `den.aspects.ivypierlot`'s `takes.celler`
    # below, so it is the take that decides the server and the carrier rather
    # than a default this host would inherit and then have to argue with.
    excludes = [ den.aspects.celler-default ];
    includes = [
      den.aspects.js
      den.aspects.vpn
      den.aspects.homebrew
      den.aspects.karabiner-driver
      den.aspects.sudoagents
      den.aspects.ccache
      den.aspects.sccache
      den.aspects.lix
      den.aspects.laptop-dock
      # Signs coolabah/paneru/kanata and execs them from a fixed path so their
      # TCC grants survive a rebuild, and (via `wrapperd`, which pulls in
      # `codesign`) replants that fixed path at boot and orders the agents that
      # exec it. Host level, not user level: it is an overlay, and overlays only
      # reach the host's `nixpkgs.overlays`.
      #
      # Two aspects on purpose: `codesign` (via `wrapperd`) cannot build until
      # the identity `codesign-identity` deploys is already on disk, and a switch
      # builds before it activates. To bootstrap (or to recover if the identity
      # is ever lost), comment out `den.aspects.wrapperd`, switch, then restore
      # it. `codesign-identity` also carries `sandbox = "relaxed"`, which is what
      # keeps every other derivation from reading the signing key.
      den.aspects.codesign-identity
      den.aspects.wrapperd
      # Host level, not user level: tailscaled is a root launchd daemon and its
      # auth key has to be readable by root, so the secret belongs to the host's
      # agenix scope. The user-level `t3code` aspect probes for this one with
      # `whenAspect` (ancestors count), and grows its `t3 serve
      # --tailscale-serve` agent only when it is present.
      den.aspects.tailscale
      # Backs this machine up to secondpc's Samba share over the tailnet, and
      # -- the part that matters most -- keeps /nix out of the backup. Pulls in
      # `tailscale` itself, since the destination name is resolved from
      # `tailscale status` at activation; see ../darwin/time-machine.nix for
      # the one-time keychain step it cannot do declaratively.
      den.aspects.time-machine
      # ── THE ROAMING RESOLVER ──
      # This machine is on the house lan for part of the week and in a cafe for
      # the rest, and the two want different ADDRESSES for the same published
      # names. `roams` below says which wire that is; the resolver decides at
      # run time, which is the only time the answer is knowable.
      #
      # NOT `den.aspects.roaming-resolver-carrier-names`, which would also pin
      # `secondpc.tailef3db9.ts.net` and so take the substituters, the ssh
      # config and the time-machine destination onto the lan at home. That is
      # the bigger win and the bigger risk; ../network/roaming-resolver.nix says
      # what has to be checked on the box first.
      den.aspects.roaming-resolver
      #      den.aspects.builders
    ];

    # ── THE WIRE THIS HOST IS SOMETIMES ON ──
    #
    # NOT `includes = [ den.aspects.lan-home ]`, and the difference is the whole
    # point. Including it would make ../base/capability.nix rank the lan at
    # preference 20 against the tailnet's 40 for EVERY eval-time answer -- the
    # celler substituter, the netrc, the push hook, ssh, the time-machine
    # destination -- and write 192.168.x addresses into a system that spends
    # half its life somewhere else. It would also make this host a lan sibling
    # from the other side, where its address is null (measured).
    #
    # `roams` is the weaker claim: the route exists when this box happens to be
    # there. ../network/roaming-resolver.nix is the only reader.
    roams = [ den.aspects.lan-home ];

    provides.to-users.homeManager.programs.git.enable = true;

    # "assessment": an exam/lockdown profile, expressed as this host minus the
    # aspects that have no business running during an assessment. Building it
    # re-resolves the whole aspect tree (users included) without them, so nothing
    # here has to know *which* options each aspect happens to set -- the previous
    # version of this was a hand-maintained list of `mkForce false` pokes that
    # went stale the moment an aspect grew another service.
    #
    # Activate with the generation's own script:
    #   /run/current-system/specialisation/assessment/activate
    specialisations.assessment.excludes = [
      # No Linux builder in a specialisation.
      #
      # `nix.linux-builder.package` carries an `apply` that re-`override`s the
      # guest NixOS system inside every darwin fixpoint, so each specialisation
      # evaluates a COMPLETE NixOS system of its own -- measured at ~1670
      # `evalModules` from `pam.nix` alone, per profile. Four configurations on
      # this host meant four guests.
      #
      # A profile switched into for an exam, a study session or a focused dock
      # has no reason to cross-build Linux; `darwin-rebuild switch` back to the
      # base configuration restores it. Possible because ../darwin/linux-builder.nix
      # is its own aspect rather than part of `den.aspects.nix`.
      den.aspects.linux-builder
      # Same reasoning: nixvim's lazyload runs ~458 `evalModules` per
      # configuration and nixpkgs' neovim plugin submodule another ~249, and an
      # exam profile has no use for the editor. `vim` remains via nixpkgs.
      den.aspects.neovim
      den.aspects.karabiner-driver
      den.aspects.vpn
      den.aspects.paneru
      den.aspects.jankyborders
      den.aspects.kanata
      den.aspects.coolabah
    ]
    ++ noCeller;

    # "study": everything that is not study, gone. The browser half of this
    # profile (the enterprise-policy site blocklist) is contributed by the zen
    # aspect itself -- aspects/programs/browsers/zen.nix -- so the blocklist
    # lives next to the browser and this host only says which distractions to
    # drop. Zotero, the class/timetable integration and the rest of the `study`
    # role content stay: they are what the profile is for.
    specialisations.study.excludes = [
      den.aspects.linux-builder # see `assessment` above
      # Also nixvim: ~458 `evalModules` + ~249 neovim plugin submodules for this
      # profile alone. The most reversible of these three exclusions -- if you
      # want an editor in the study profile, delete this line and pay ~10% of the
      # host's evaluation for it.
      den.aspects.neovim
      den.aspects.darwin-gaming
      den.aspects.laptop-brew # steam/discord/beeper casks
      #      den.aspects.llama-cpp
      den.aspects.cotabby
      den.aspects.opencode
      # Same reasoning as the two above -- and dropping it also takes the
      # `t3 serve --tailscale-serve` agent down with it, so the machine is not
      # quietly reachable from a phone during an exam profile.
      den.aspects.t3code
    ]
    ++ noCeller;

    # "life-project": the machine with a dock that is only the life-project
    # tools. `laptop-dock` owns the everyday dock, so the profile subtracts that
    # aspect and includes the trimmed one instead of `mkForce`-ing a list -- the
    # everyday dock can grow entries without this profile inheriting them.
    specialisations.life-project = {
      excludes = [
        den.aspects.linux-builder # see `assessment` above
        den.aspects.neovim # ditto -- this profile is a dock, not a dev shell
        den.aspects.laptop-dock
        den.aspects.darwin-gaming
      ]
      ++ noCeller;
      includes = [ den.aspects.laptop-dock-focused ];
    };

    # "no-celler": no celler caches at all, for when they are down -- nothing
    # substituted from them, nothing pushed to them.
    specialisations.no-celler = {
      excludes = [
        den.aspects.celler-host
        den.aspects.celler-user
      ];

      # WHEN to pick it is not written here: `den.aspects.celler-host` declares
      # what it `requires` (../base/celler/push.nix), and
      # ../tooling/nh-checked.nix selects whichever profile excludes the aspects
      # whose requires failed. The check belongs to the thing that needs it, so
      # this list and that check cannot drift apart.
    };

    vpn = { };
    nix.settings = {
      min-free = 1024 * 1024 * 1024; # 1 GiB
    };

    darwin = { pkgs, ... }: {
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
        max-free = 25 * 1024 * 1024 * 1024;
        # Keeping .drv files alive costs space and only buys offline rebuilds
        # of things already built; not worth it on a space-constrained laptop.
        keep-derivations = false;
      };

      # Local linux builder VM (ephemeral) for cross-building x86_64/aarch64-linux.
      nix.distributedBuilds = true;

      # The specialisations live on the aspect above, as sets of excluded and
      # included aspects rather than lists of overrides.
    };
  };

  # The everyday dock, as its own aspect so a specialisation can drop it whole
  # (see `specialisations.life-project`) instead of `mkForce`-ing the list.
  den.aspects.laptop-dock.darwin.system.defaults.dock.persistent-apps = [
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

  # The life-project dock: writing, planning, reading, a terminal. No chat, no
  # media, no IDE.
  den.aspects.laptop-dock-focused.darwin.system.defaults.dock = {
    persistent-apps = [
      "/Applications/Nix Apps/Zen.app"
      "/Applications/Fantastical.app"
      "/Applications/Microsoft Word.app"
      "/Applications/Zotero.app"
      "/Applications/Todoist.app"
      "/Applications/Nix Apps/Ghostty.app"
    ];
    # Nothing to fish out of the dock that is not pinned above.
    show-recents = false;
    static-only = true;
  };

  den.aspects.laptop-brew = {
    includes = [ den.aspects.homebrew ];
    brew = {
      brews = [
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
    # NO `via`. The carrier is DERIVED at the point of use, not chosen here: a
    # connection to a cache needs a transport, and which one it gets falls out
    # of the flow's own constraints (../base/celler/push.nix resolves it through
    # ../base/capability.nix).
    #
    # For this push the derivation lands on the tailnet, and for the reason the
    # override used to encode by hand: `celler push` sends NARs far past
    # Cloudflare's 100MB body cap, so `takes.http.maxBodyBytes = b: b == null`
    # excludes the public carrier by arithmetic -- and this laptop has no
    # `lanAddress`, so the lan is not a candidate either.
    #
    # That is the whole argument for deriving it. The override was right, and it
    # was right only because somebody worked out the same thing once and wrote
    # the answer down where nothing could check it.
    # ── THE DECLARATION IS THE CHAIN, AND `celler-use` IS DERIVED FROM IT ──
    # Was a flat `celler-use.<server>.push = [ "main" ]`, which named the server,
    # the cache and the flow in one key and so could not say which was which.
    # As a take it is the path: that celler, its cache `main`, and that cache's
    # push -- and `push` is not furnished by anybody, it is derived from the
    # cache above it (../base/capability-types.nix).
    #
    # `name = "celler2"` filters the celler level. A celler server is named
    # after its host, so the name comes off the candidate's host rather than
    # from anything the giver furnishes -- a giver's `gives` runs in the ASKER's
    # scope and could only name this laptop.
    #
    # This could not be said until celler2 declared itself a celler: it runs a
    # container-ised one on 8080 and so cannot include `den.aspects.celler`,
    # whose port is 8069. The capability half is parametric now, and
    # ../hosts/celler2.nix states its own.
    #
    # ── IT SHOULD SAY `secondpc`, AND CHANGING IT NEEDS A SECRET RUN ──
    #
    # celler2 is a declared host and an OCI image (`cache2.ivymect.in` through a
    # cloudflared tunnel) but not a machine that is up, so this push goes
    # nowhere; secondpc really does serve one, and `celler-host`'s own check on
    # this host already points at it
    # (`http://secondpc.tailef3db9.ts.net:8069/nix-cache-info`).
    #
    # What stops the one-word edit is that the TOKEN'S SCOPE is this take.
    # agenix-rekey names a generated secret `<n>-<hash of its scope>.age`, so
    # flipping the name here renames two files that do not exist yet -- measured:
    #
    #   error: host Ivys-MacBook-Pro:
    #   age.secrets."celler-host/celler_token_celler2".rekeyFile
    #   (secrets/generated/Ivys-MacBook-Pro/celler-host/celler_token_celler2-5ba000c4.age)
    #   doesn't exist. Did you run `agenix generate`?
    #
    # against the `-888b7d5c` on disk. Minting them is `nix run .#gen-secrets`,
    # which ../../CLAUDE.md forbids while the master key is mid-recovery. So the
    # change is one word plus one secret run, in that order, and not before.
    takes.celler = {
      name = "celler2";
      gives.cache.main.gives.push = { };
    };
    study.includes = [ den.aspects.zotero ];
    includes = [
      den.aspects.zig
      den.aspects.homebrew
      #      den.aspects.zed
      den.aspects.idris
      den.aspects.swift
      den.aspects.agenix-rekey
      den.aspects.onepassword
      den.aspects.nixvim
      den.aspects.neovim
      #      den.aspects.zed
      # Runs the lspmux server. nixvim connects to its unix socket directly (RPC);
      # zed and the CLIs go through the `lspmux client` shims. Without this there
      # is nothing on the other end of either.
      den.aspects.lspmux
      den.aspects.fish
      den.aspects.nushell
      den.aspects.ghostty
      den.aspects.coolabah
      <browsers/zen>
      den.aspects.gui
      den.aspects.gpg
      #      den.aspects.rclone
      den.aspects.paneru
      den.aspects.kanata
      den.aspects.dev
      den.aspects.opencode
      # Pulls in the claude / opencode / jujutsu provider wiring by itself, via
      # whenAspect -- it reacts to what this user already has rather than
      # forcing any of them on.
      den.aspects.t3code
      #      den.aspects.openclaw
      #      den.aspects.llama-cpp
      # zeroclaw-daemon depends on a rust-src component that fails to build
      # (rust-src-stable-2026-08-20.drv), taking the whole system switch down
      # with it. Dropped until that's fixed upstream.
      # den.aspects.zeroclaw
      den.aspects.file-local
      den.batteries.primary-user
      den.aspects.cotabby
      #      <zen>
    ];
    provides.Ivys-MacBook-Pro.includes = [
      den.aspects.laptop-brew
      den.aspects.darwin-gaming
      # esphome + esptool, and the CH34x serial driver cask. Laptop only: the
      # dashboard on secondpc (../services/homeassistant/esphome.nix) is the
      # other half, and the two share nothing but the device YAML.
      den.aspects.esp-dev
    ];
    provides.Ivys-MacBook-Pro.provides.to-users = {
      homeManager = { pkgs, ... }: {
        home.packages = with pkgs; [
          nodejs
          opencode
          vscode
          pandoc
          # scheme-small + French over scheme-medium: medium pulls in every TeX
          # Live language collection (dozens of hyphenation packages), which is
          # most of what made evaluating this closure slow. English ships with
          # scheme-small already; add collection-lang<x> here for any other
          # language a document needs, rather than going back to Medium/Full.
          (texlive.combine {
            inherit (texlive) scheme-small collection-langfrench;
          })
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
