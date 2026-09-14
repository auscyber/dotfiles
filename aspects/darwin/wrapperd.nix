# Plants the codesign-signed wrappers into `trustedDir` and orders the launchd
# agents that exec them, over a Mach-ports service.
#
# ## The bug this fixes
#
# `aspects/darwin/codesign.nix` signs paneru/sketchybar/coolabah/kanata and copies
# each signed binary to a fixed path (`trustedDir` = `/run/wrappers/bin`) so its
# TCC grant survives a rebuild. But `/run` on darwin is a synthetic firmlink to
# `/private/var/run`, which macOS WIPES on boot, and nix-darwin activation only
# runs at `darwin-rebuild` time -- `org.nixos.activate-system`'s RunAtLoad job
# (`activate-system-start`) only relinks `/run/current-system`, sets gcroots,
# checks the nixbld users and rebuilds `/etc`; it never runs `postActivation`.
# So after a reboot `trustedDir` is empty until the next manual switch, and every
# launchd agent that execs `${trustedDir}/<name>` crash-loops under `KeepAlive`.
#
# ## The shape of the fix
#
#   * `packages/wrapperd` -- a Rust binary. `wrapperd serve` plants the wrappers
#     then binds a Mach service (`in.ivymect.wrapperd`) and answers readiness;
#     `wrapperd plant` just plants. `wrapperd-wait --label L --then P` is the
#     per-agent shim: it blocks on the service until L is released, then execs P.
#   * a system LaunchDaemon runs `wrapperd serve` at boot (`RunAtLoad`,
#     `wait4path /nix/store`), so the wrappers are replanted before anything that
#     waits on them. Both `wrapperd` and `wrapperd-wait` are store binaries that
#     never live under `trustedDir`, so they run even while it is still empty.
#   * each managed agent's `Program` is routed through `wrapperd-wait` (in the
#     agent's own aspect), gating it on the daemon rather than exec'ing the
#     trusted path directly.
#
# The code-signing model in `codesign.nix` is untouched: `wrapperd-wait` execs
# the REAL signed copy at `${trustedDir}/<name>`, so TCC still evaluates the
# stable trusted path and the signature/entitlements baked into that image. (A
# setuid-style execve stub could not carry a signature -- code identity is
# re-derived from the final image on every exec -- which is why the signed binary
# is still COPIED to the stable path rather than symlinked or re-execed.)
#
# ## Planters are separate
#
# `extraModules/darwin/wrappers` keeps its own planter for the setuid/setgid C
# stubs (`nix-ccache`) and its own `rm -rf ${wrapperDir}` on activation. This
# aspect only plants the codesign copies, and -- like `codesign.nix` did before
# it -- runs in `postActivation` with `lib.mkAfter` so it lands after that
# module's `rm -rf`, not before it.
{
  den,
  lib,
  config,
  ...
}:
let
  # Flake-parts `config`, captured before the darwin module below binds its own
  # `config` of the same name -- so `flake.lib.codesign` stays reachable inside.
  flakeParts = config;
  inherit (flakeParts.flake.lib.codesign) mkSignedWrapper entitlementsFor trustedDir;

  service = "in.ivymect.wrapperd";
in
{
  # Consumed by each managed agent's own aspect to route its launchd job through
  # `wrapperd-wait`: the job blocks on the wrapperd service until `label` is
  # released, then execs the signed copy at `${trustedDir}/${name}` -- the same
  # stable path `codesign.nix` keys the TCC grant on. The whole command lives in
  # `ProgramArguments` and `Program` is forced back to null: home-manager's
  # launchd module builds its command as `Program` ++ `ProgramArguments`, so
  # setting both plants the wait binary's own path as a positional argument.
  flake.lib.wrapperd = {
    inherit service;
    waitConfig =
      {
        pkgs,
        label,
        name,
      }:
      let
        wait = lib.getExe' pkgs.wrapperd "wrapperd-wait";
      in
      {
        Program = lib.mkForce null;
        ProgramArguments = lib.mkForce [
          wait
          "--label"
          label
          "--service"
          service
          "--then"
          "${trustedDir}/${name}"
        ];
      };
  };

  den.aspects.wrapperd = {
    includes = [ den.aspects.codesign ];

    # `pkgs.wrapperd`. Sorted after codesign's `zzz-codesign` is irrelevant --
    # this depends on none of the signed attributes, only on the crate.
    overlays.wrapperd = [
      (final: _prev: {
        wrapperd = final.callPackage ../../packages/wrapperd/package.nix { };
      })
    ];

    darwin =
      {
        config,
        pkgs,
        ...
      }:
      let
        # A wrapped program plants BOTH its verbatim env-setting script and the
        # hidden `.<name>-wrapped` signed Mach-O it execs; a bare binary is just
        # itself. Which is which is fixed per program (see `codesign.nix`): the
        # `finalPackage`-wrapped bars are wrapped, kanata is bare. `name` is the
        # stable filename the copy lands under (what `codesign.nix` keys it on:
        # `--binary-identifier "$name"`, `$name` = basename of `bin/*`) -- a plain
        # literal, NOT `baseNameOf (lib.getExe pkg)`, which would carry the store
        # path as string context and is rejected as an attribute key.
        entriesFor =
          { name, wrapped, pkg }:
          {
            "${name}" = "${pkg}/trusted/${name}";
          }
          // lib.optionalAttrs wrapped {
            ".${name}-wrapped" = "${pkg}/trusted/.${name}-wrapped";
          };

        # Same collection and same `enable`-gating as `codesign.nix`'s old
        # activation script -- read from `enable` rather than `finalPackage or
        # null`, because the option exists (with no value) even where the module
        # is off, and the `assessment` specialisation excludes these.
        fromUser = user: [
          {
            on = user.services.paneru.enable or false;
            get = _: {
              name = "paneru";
              wrapped = true;
              pkg = mkSignedWrapper pkgs {
                package = user.services.paneru.finalPackage;
                entitlements = entitlementsFor.paneru or { };
              };
            };
          }
          {
            on = user.programs.sketchybar.enable or false;
            get = _: {
              name = "sketchybar";
              wrapped = true;
              pkg = mkSignedWrapper pkgs { package = user.programs.sketchybar.finalPackage; };
            };
          }
          {
            on = user.programs.coolabah.enable or false;
            get = _: {
              name = "coolabah";
              wrapped = true;
              pkg = mkSignedWrapper pkgs { package = user.programs.coolabah.finalPackage; };
            };
          }
          {
            on = user.programs.kanata.enable or false;
            # Already signed via codesign's overlay (package defaults to
            # `kanata-with-cmd`, which is in the `signed` list); bare.
            get = _: {
              name = "kanata";
              wrapped = false;
              pkg = user.programs.kanata.package;
            };
          }
        ];

        items =
          map (e: e.get null) (
            lib.filter (e: e.on) (lib.concatMap fromUser (lib.attrValues (config.home-manager.users or { })))
          )
          ++ lib.optional (pkgs ? kanata-vk-agent) {
            name = "kanata-vk-agent";
            wrapped = false;
            pkg = pkgs.kanata-vk-agent;
          };

        wrappers = lib.foldl' (acc: it: acc // entriesFor it) { } items;

        manifest = (pkgs.formats.toml { }).generate "wrapperd-manifest.toml" {
          dir = trustedDir;
          inherit wrappers;
        };

        wrapperd = lib.getExe pkgs.wrapperd;
      in
      lib.mkIf (wrappers != { }) {
        # The boot planter + orchestrator. `wait4path /nix/store` because a
        # LaunchDaemon can fire before the Nix store volume is mounted;
        # `RunAtLoad` + `KeepAlive` so it comes up at boot and stays up to answer
        # late waits. `MachServices` is what registers `service` in the bootstrap
        # namespace -- the daemon claims that launchd-held receive right on
        # `Receiver::bind`.
        launchd.daemons.wrapperd.serviceConfig = {
          Label = service;
          ProgramArguments = [
            "/bin/sh"
            "-c"
            "/bin/wait4path /nix/store && exec ${wrapperd} serve --manifest ${manifest} --service ${service}"
          ];
          RunAtLoad = true;
          KeepAlive = true;
          MachServices.${service} = true;
          StandardErrorPath = "/var/log/wrapperd.log";
          StandardOutPath = "/var/log/wrapperd.log";
        };

        # Still plant synchronously on switch, so `darwin-rebuild switch` leaves
        # the wrappers present immediately rather than waiting for the daemon to
        # be (re)loaded. `mkAfter` puts this after `extraModules/darwin/wrappers`'
        # `rm -rf ${wrapperDir}` within `postActivation`; `extraActivation` would
        # run before that wipe.
        system.activationScripts.postActivation.text = lib.mkAfter ''
          ${wrapperd} plant --manifest ${manifest}
        '';
      };
  };
}
