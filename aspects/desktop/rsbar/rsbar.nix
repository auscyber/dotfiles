{
  inputs,
  lib,
  den,
  config,
  ...
}:
let
  # Alias for the flake-parts top-level `config`, captured here before
  # `homeManager` below binds its own (home-manager) `config` of the same name --
  # which would otherwise shadow this one and put `codesign.mkSignedWrapper`
  # out of reach inside it.
  flakeParts = config;
in
{
  den.aspects.rsbar = {
    # Declared on the aspect, not the file: the partition generator reads which
    # aspect owns an input, and which platforms pull that aspect in. rsbar is a
    # window-server daemon and builds for darwin only, so this lives in the
    # `darwin` bucket (../../../partition-map.nix).
    flake-file = _: {
      inputs.rsbar.url = "github:auscyber/rsbar";
      inputs.rsbar.inputs.nixpkgs.follows = "nixpkgs";
      inputs.rsbar.inputs.crane.follows = "crane";
      inputs.rsbar.inputs.flake-parts.follows = "flake-parts";
      inputs.rsbar.inputs.rust-overlay.follows = "rust-overlay";
    };

    overlays = {
      # Built from rsbar's `nix/package.nix` directly rather than taken from its
      # flake's `packages.<system>.rsbar`.
      #
      # That output cannot be evaluated: rsbar's flake.nix builds its toolchain
      # with `(import inputs.rust-overlay { inherit pkgs; }).rust-bin`, and
      # rust-overlay's default.nix is a bare `final: prev:` overlay -- applying
      # it to one attrset leaves a *function*, so selecting `.rust-bin` off it
      # fails with "expected a set but found a function". Nothing here can fix
      # that from the outside, and it only bites once something forces the
      # derivation (a `.name` read does not), which is why it surfaces at
      # activation rather than at eval.
      #
      # `nix/package.nix` itself is fine -- it takes `craneLib` and `src` as
      # arguments. So supply them from this flake's own `crane` and
      # `rust-overlay`, which are root inputs the partition already sees, and
      # skip the broken flake output entirely. rsbar's home-manager module
      # defaults `package` to that same output, so ./. sets
      # `programs.rsbar.package = pkgs.rsbar` and the default is never forced.
      rsbar = lib.optional (inputs ? rsbar) (
        final: _prev: {
          rsbar = final.callPackage "${inputs.rsbar}/nix/package.nix" {
            craneLib = (inputs.crane.mkLib final).overrideToolchain (
              p: (p.extend (import inputs.rust-overlay)).rust-bin.stable.latest.default
            );
            src = inputs.rsbar;
          };
        }
      );
    };

    darwin = { pkgs, ... }: {
      fonts.packages =
        with pkgs;
        [ sketchybar-app-font ]
        ++ (with nerd-fonts; [
          hack
          roboto-mono
        ]);
    };

    # `homeManager`, not `hmDarwin`: a den class body routed through `hmDarwin`
    # is merged as a bare config fragment, so it cannot carry the `imports` that
    # brings rsbar's own home-manager module in. Same shape den.aspects.paneru
    # uses, and safe for the same reason -- rsbar builds for darwin only and is
    # only ever included on a darwin host.
    homeManager =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      {
        imports = [ inputs.rsbar.homeManagerModules.rsbar ];

        config =
          let
            colors = config.stylix.base16Scheme;

            # `programs.rsbar.finalPackage` (rsbar's own home-manager module,
            # `nix/hm-module.nix`) wraps `programs.rsbar.package` a second
            # time -- for `extraPackages` on PATH -- through a plain
            # `symlinkJoin` + `wrapProgram` that has never heard of
            # `mkSignedWrapper`. Same shape, and for the same reason, as
            # sketchybar and paneru: `pkgs.rsbar` is deliberately absent from
            # the `signed` list in ../../darwin/codesign.nix so that wrap is
            # the ONLY one `finalPackage` carries, and it is signed here
            # instead. rsbar drives SkyLight and the Accessibility API, so its
            # TCC grant is worth exactly as much as the stability of the path
            # launchd execs.
            rsbarSigned = flakeParts.flake.lib.codesign.mkSignedWrapper pkgs {
              package = config.programs.rsbar.finalPackage;
              entitlements = flakeParts.flake.lib.codesign.entitlementsFor.rsbar or { };
            };
          in
          {
            programs.rsbar = {
              enable = true;
              package = pkgs.rsbar;
              configType = "lua";
              service.enable = true;
              extraPackages = with pkgs; [
                jq
                yq
                nowplaying-cli
              ];

              # rsbar runs `rsbarrc` as a subprocess with the config's own
              # directory appended to `package.path`/`package.cpath` (see
              # `rsbar_lua::host::add_search_paths`), so every `require` below
              # resolves to a file some *other* aspect dropped next to this one
              # -- there is no `extraLuaPackages` to push a nixpkgs Lua package
              # onto, the way sketchybar's module has.
              #
              # `wm` is the WM-agnostic provider contract the sketchybar config
              # already uses: den.aspects.paneru ships
              # aspects/wms/paneru/sketchybar/wm.lua as `wm.lua` here when
              # paneru is the enabled WM. `pcall` because a host may enable the
              # bar with no window manager at all, and an absent provider
              # should leave a working bar rather than an empty one.
              config =
                # lua
                ''
                  -- Generated from aspects/desktop/rsbar/rsbar.nix -- do not edit by hand.
                  local sbar = require("rsbar")

                  sbar.begin_config()

                  sbar.bar({
                  	height = 32,
                  	color = 0xff${colors.base00},
                  })

                  sbar.default({
                  	label = { color = 0xff${colors.base05} },
                  	icon = { color = 0xff${colors.base05} },
                  })

                  -- `add(kind, name, opts)` with `opts.position`, which is what
                  -- rsbar documents; SbarLua's positional third argument works
                  -- too, but `wm`/`paneru_bar` already speak the documented form.
                  local clock = sbar.add("item", "clock", {
                  	position = "right",
                  	update_freq = 1,
                  	label = { color = 0xff${colors.base0B} },
                  })

                  clock:subscribe("routine", function()
                  	clock:set({ label = os.date("%a %d %b %H:%M") })
                  end)

                  local ok, err = pcall(require, "wm")
                  if not ok then
                  	print("rsbar: no window-manager provider: " .. tostring(err))
                  end

                  sbar.end_config()
                  sbar.event_loop()
                '';
            };

            # rsbar's own home-manager module points this launchd job's
            # `Program` at `programs.rsbar.finalPackage` directly, which is the
            # UNSIGNED wrapProgram wrap (see `rsbarSigned` above). Forced to
            # the signed equivalent instead, so the process launchd actually
            # execs bottoms out at `${trustedDir}/rsbard` rather than a raw,
            # rebuild-varying store path -- TCC's Accessibility/Screen-Recording
            # grants are worthless to a client whose path never survives a
            # rebuild. Only `Program` is overridden; `KeepAlive` and the rest
            # keep whatever that module set.
            launchd.agents.rsbar.config.Program = lib.mkForce (lib.getExe rsbarSigned);
          };
      };

    includes = [ den.aspects.packages.sketchybar_app_font ];
  };
}
