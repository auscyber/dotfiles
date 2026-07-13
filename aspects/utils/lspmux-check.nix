{ lib, ... }:
# nvim-lspconfig is the source of truth for how a language server is spawned. Our
# lspmux shims have to agree with it on two things: the binary name (`exe`, which is
# what the shim looks up on $PATH and, for dynamicCmd servers, what it shadows) and
# the args (`args`, which editors pass when they spawn the shim by path). If upstream
# renames a binary or changes a flag, the shim silently stops matching and the server
# quietly runs unmultiplexed -- or not at all. This check diffs our table against
# lspconfig's own `lsp/<name>.lua` so that failure is loud instead.
#
# Not gated to a single system: it is a grep over a vim plugin, it can run anywhere.
{
  perSystem =
    { pkgs, ... }:
    {
      checks.lspmux-server-names =
        let
          servers = import ../modules/editors/_lspmux-servers.nix pkgs;

          # <lspconfig-name> <dynamic|static> <cmd...>  -- the cmd we believe lspconfig
          # spawns for this server
          spec = lib.concatStringsSep "\n" (
            map (
              server:
              lib.concatStringsSep " " (
                [
                  server.lspconfig
                  (if server.dynamicCmd or false then "dynamic" else "static")
                  server.exe
                ]
                ++ (server.args or [ ])
              )
            ) servers
          );
        in
        pkgs.runCommand "check-lspmux-server-names"
          {
            lspconfig = pkgs.vimPlugins.nvim-lspconfig;
            spec = pkgs.writeText "lspmux-servers" spec;
          }
          ''
            fail=0

            while read -r name kind exe args; do
              lua="$lspconfig/lsp/$name.lua"
              want="$exe''${args:+ $args}"

              if [ ! -f "$lua" ]; then
                echo "FAIL $name: nvim-lspconfig has no lsp/$name.lua (renamed or dropped upstream)" >&2
                fail=1
                continue
              fi

              if grep -qE "^[[:space:]]*cmd = function" "$lua"; then
                if [ "$kind" != dynamic ]; then
                  echo "FAIL $name: lspconfig now computes cmd at runtime; mark it dynamicCmd" >&2
                  fail=1
                elif grep -qF "'$exe'" "$lua"; then
                  echo "ok   $name -> $exe (cmd computed at runtime, shim shadows the name)"
                else
                  echo "FAIL $name: runtime cmd never mentions '$exe'" >&2
                  fail=1
                fi
                continue
              fi

              if [ "$kind" = dynamic ]; then
                echo "FAIL $name: marked dynamicCmd but lspconfig now uses a static cmd" >&2
                fail=1
                continue
              fi

              # `  cmd = { 'pyright-langserver', '--stdio' },` -> `pyright-langserver --stdio`
              got=$(
                sed -n "s/^[[:space:]]*cmd = {\(.*\)}.*/\1/p" "$lua" \
                  | head -1 \
                  | grep -o "'[^']*'" \
                  | tr -d "'" \
                  | tr '\n' ' ' \
                  | sed 's/ *$//'
              )

              if [ -z "$got" ]; then
                echo "FAIL $name: could not read cmd out of lsp/$name.lua" >&2
                fail=1
              elif [ "$got" != "$want" ]; then
                echo "FAIL $name: lspconfig spawns [$got] but we wrap [$want]" >&2
                fail=1
              else
                echo "ok   $name -> $want"
              fi
            done < "$spec"

            [ $fail -eq 0 ] || exit 1
            echo ok > $out
          '';
    };
}
