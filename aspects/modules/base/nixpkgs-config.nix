{ den, lib, ... }:
let
  # Explicit allowlist of unfree packages we actually want to install.
  # Add a name here when something legitimately needs unfree status; avoid
  # turning on blanket allowUnfree so accidental unfree pulls stay visible.
  allowed = [
    "1password"
    "1password-cli"
    "1password-gui"
    "1password-gui-beta"
    "claude-code"
    "discord"
    "google-chrome"
    "helium"
    "helium-bin"
    "libkey-nomad"
    "memorymate"
    "minecraft-launcher"
    "minecraft-server"
    "nvidia-settings"
    "nvidia-x11"
    "obsidian"
    "opencode"
    "slack"
    "spotify"
    "steam"
    "steam-original"
    "steam-run"
    "steam-runtime"
    "steam-unwrapped"
    "tidal-hifi"
    "vscode"
    "zoom"
    "cmp-nvim-lsp-document-symbol"
  ];

  predicate = a: pkg: builtins.elem (lib.getName pkg) a;
in
{

  den.quirks.unfreeAllowed = { };
  den.policies.pipe-unfree =
    { host, ... }:
    let
      inherit (den.lib.policy) pipe;
    in
    pipe.from "unfreeAllowed " [
      (pipe.fold (acc: n: acc ++ n) [ ])
      pipe.expose
    ];
  den.aspects.nixpkgs-config = {
    includes = [ den.policies.pipe-unfree ];
    os = { unfreeAllowed, ... }: {
      nixpkgs.config = {
        allowUnfreePredicate = predicate (allowed ++ unfreeAllowed);
        # pam_rssh's meta.platforms is linux-only; we still want the .dylib build
        # on darwin since it works in practice. Same story for a handful of
        # cross-arch dev tools. Flip off if a real unsupported package sneaks in.
        allowUnsupportedSystem = true;
      };
    };
    homeManager = { unfreeAllowed, ... }: {
      nixpkgs.config = {

        allowUnfreePredicate = predicate (allowed ++ unfreeAllowed);
        allowUnsupportedSystem = true;
      };
    };
  };

  den.schema.host.includes = [ den.aspects.nixpkgs-config ];
  den.schema.user.includes = [ den.aspects.nixpkgs-config ];
}
