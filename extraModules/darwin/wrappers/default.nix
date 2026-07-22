# A `security.wrappers` equivalent for nix-darwin.
#
# nix-darwin has no setuid/setgid wrapper mechanism (NixOS's lives in
# nixos/modules/security/wrappers, and its wrapper.c is deeply Linux-specific --
# linux/capability.h, /proc, prctl, xattrs -- so none of it ports). This provides
# the same option surface for the subset that makes sense on macOS: install a
# small compiled wrapper, owned root:<group>, with the setgid (and/or setuid) bit
# set, into a directory on PATH.
#
# ## Why a compiled wrapper and not just a chmod'd script
#
# macOS -- like every other unix -- ignores the setuid/setgid bits on interpreter
# scripts. Only a compiled binary can carry them. So for each wrapper we compile
# a tiny C stub that execs the real program; the stub is what gets the set-id bit.
#
# ## Where it lives, and the reboot caveat
#
# `/run` on darwin is a synthetic firmlink to `/private/var/run`, which macOS
# clears on boot -- and nix-darwin activation only runs at `darwin-rebuild` time,
# not at boot. So, exactly like the tmpfiles shim in this repo, the wrappers are
# (re)created on activation and will be MISSING after a reboot until the next
# `darwin-rebuild activate`. `/run/wrappers/bin` is kept as the default to match
# NixOS; point `security.wrapperDir` at a persistent path if the reboot gap bites.
#
# The store cannot hold set-id files (it is read-only and Nix would refuse them),
# so the compiled stub is built unprivileged in the store and then copied into
# `wrapperDir` and chmod'd during the (root) activation script.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.security;

  wrapperType = lib.types.submodule (
    {
      name,
      config,
      ...
    }:
    {
      options = {
        source = lib.mkOption {
          type = lib.types.path;
          description = "The program to run with elevated privileges.";
        };
        program = lib.mkOption {
          type = lib.types.str;
          default = name;
          description = "The name under which the wrapper is installed on PATH.";
        };
        owner = lib.mkOption {
          type = lib.types.str;
          default = "root";
          description = "Owner of the wrapper.";
        };
        group = lib.mkOption {
          type = lib.types.str;
          default = "root";
          description = "Group of the wrapper.";
        };
        setuid = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether to add the setuid bit (runs as `owner`).";
        };
        setgid = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether to add the setgid bit (runs as `group`).";
        };
      };
    }
  );

  # The stub. It is compiled once per wrapper with SOURCE_PROG baked in. macOS's
  # dyld already strips DYLD_* for set-id images, but the program we exec is not
  # itself set-id, so we clear those injection vectors before handing off the
  # elevated gid/uid to it.
  wrapperCSrc = pkgs.writeText "darwin-security-wrapper.c" ''
    #include <unistd.h>
    #include <stdlib.h>
    #include <stdio.h>

    #ifndef SOURCE_PROG
    #error SOURCE_PROG must be defined via -include
    #endif

    int main(int argc, char **argv) {
        unsetenv("DYLD_INSERT_LIBRARIES");
        unsetenv("DYLD_LIBRARY_PATH");
        unsetenv("DYLD_FRAMEWORK_PATH");
        unsetenv("DYLD_FALLBACK_LIBRARY_PATH");
        unsetenv("DYLD_FALLBACK_FRAMEWORK_PATH");
        execv(SOURCE_PROG, argv);
        perror("darwin security wrapper: execv");
        return 127;
    }
  '';

  mkWrapperBin =
    w:
    pkgs.runCommandCC "security-wrapper-${w.program}" { } ''
      mkdir -p "$out/bin"
      # Bake the target path in via a generated header so no C-string escaping
      # has to happen in Nix. `w.source` is a store path -> printf-safe.
      printf '#define SOURCE_PROG "%s"\n' '${w.source}' > prog.h
      $CC -O2 -Wall -include prog.h ${wrapperCSrc} -o "$out/bin/${w.program}"
    '';

  # 0755 + setuid(+4000) + setgid(+2000), rendered as the octal chmod wants.
  wrapperMode = w: toString (755 + (if w.setuid then 4000 else 0) + (if w.setgid then 2000 else 0));

  wrappers = lib.attrValues cfg.wrappers;

  installOne =
    w:
    let
      dst = lib.escapeShellArg "${cfg.wrapperDir}/${w.program}";
    in
    ''
      cp ${mkWrapperBin w}/bin/${lib.escapeShellArg w.program} ${dst}
      chown ${lib.escapeShellArg "${w.owner}:${w.group}"} ${dst}
      chmod ${wrapperMode w} ${dst}
    '';
in
{
  options.security = {
    wrappers = lib.mkOption {
      type = lib.types.attrsOf wrapperType;
      default = { };
      description = ''
        Set-id wrapper programs, mirroring NixOS `security.wrappers` for the
        subset nix-darwin can support. Installed into {option}`security.wrapperDir`.
      '';
    };
    wrapperDir = lib.mkOption {
      type = lib.types.str;
      default = "/run/wrappers/bin";
      description = ''
        Directory the wrappers are installed into and which is added to PATH.
        Recreated on every activation; see the module header re: the reboot gap.
      '';
    };
  };

  config = lib.mkIf (wrappers != [ ]) {
    environment.systemPath = [ cfg.wrapperDir ];

    # Runs last (after the `groups`/`users` activation steps), so chowning to a
    # managed group is safe, and after `createRun` has set up `/run`.
    system.activationScripts.postActivation.text = ''
      # security.wrappers
      rm -rf ${lib.escapeShellArg cfg.wrapperDir}
      mkdir -p ${lib.escapeShellArg cfg.wrapperDir}
      ${lib.concatStringsSep "\n" (map installOne wrappers)}
    '';
  };
}
