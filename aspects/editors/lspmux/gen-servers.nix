{ lib, ... }:
# `nix run .#gen-lsp-servers` -- the generator half of checks.lspmux-server-names.
#
# The check (./lspmux-check.nix) diffs the hand-written `lsp-servers` table
# against nvim-lspconfig's `lsp/<name>.lua`; this writes down what those files
# actually say, so a new server's `exe`/`args`/`dynamicCmd` can be read off
# upstream instead of guessed at and then argued with by the check.
#
#   nix run .#gen-lsp-servers -- --format nix --only pyright,gopls
#   nix run .#gen-lsp-servers -- --format nix --fields exe,args,dynamicCmd -o servers.nix
#
# The scripts themselves (../../../scripts/gen-lsp-servers.{sh,lua}) also run
# standalone; the app exists so neither the nvim nor the nvim-lspconfig they read
# is whatever happens to be on the caller's $PATH. Both are pinned here -- and
# `vimPlugins.nvim-lspconfig` is the same derivation the check greps, so the two
# halves cannot end up reading different versions of upstream.
{
  perSystem =
    { pkgs, ... }:
    let
      # The pair has to land in the store together: the wrapper resolves the lua
      # extractor as its own sibling, so a bare `${./file.sh}` (which stores the
      # script alone, under /nix/store) would leave it with nothing next to it.
      generator = pkgs.runCommandLocal "gen-lsp-servers-src" { } ''
        mkdir -p "$out"
        cp ${../../../scripts/gen-lsp-servers.sh} "$out/gen-lsp-servers.sh"
        cp ${../../../scripts/gen-lsp-servers.lua} "$out/gen-lsp-servers.lua"
        chmod +x "$out/gen-lsp-servers.sh"
      '';
    in
    {
      apps.gen-lsp-servers = {
        type = "app";
        program = lib.getExe (
          pkgs.writeShellApplication {
            name = "gen-lsp-servers";
            # neovim-unwrapped rather than `neovim`: a wrapped nvim injects the
            # user's plugins with `--cmd "set packpath^=..."` ahead of the
            # script's own args, and those plugins then race the extraction (a
            # copilot autostart lands in the stubbed rpc entry points). The
            # script defends against that anyway; this removes the need.
            runtimeInputs = [
              pkgs.neovim-unwrapped
              pkgs.coreutils
              pkgs.gnused
            ];
            # `--src` goes first so a caller can still point the run at their own
            # nvim-lspconfig checkout: the wrapper takes the last `--src` it is
            # given. Without one it would fall back to building the plugin
            # itself, which is exactly the flake evaluation this app already did.
            text = ''
              exec ${lib.getExe' pkgs.bash "bash"} ${generator}/gen-lsp-servers.sh \
                --src ${pkgs.vimPlugins.nvim-lspconfig} "$@"
            '';
          }
        );
      };
    };
}
