{ inputs, ... }: {
  ff.claude-code.url = "github:sadjow/claude-code-nix";
  ff.claude-code.inputs.nixpkgs.follows = "nixpkgs";
  den.aspects.claude = {
    secrets.claude_token.rekeyFile = ./token.age;
    secrets.dad_token.rekeyFile = ./other_token.age;
    overlays.claude-code = inputs.claude-code.overlays.default;
    homeManager =
      { pkgs, scoped, ... }:
      let
        token_name = "dad_token";
      in

      {
        programs.claude-code = {
          enable = true;
          enableMcpIntegration = true;

          # A directory, not a file: the skill ships its script alongside
          # SKILL.md, and the module symlinks the whole tree to
          # `.claude/skills/flamegraph`. The script re-enters the target
          # project's own dev shell (devenv or flake), since a project's
          # toolchain is generally not on the ambient PATH here.
          context = ./context.md;
          skills.flamegraph = ./skills/flamegraph;
          package =
            pkgs.runCommand "claude-wrapped"
              {
                version = pkgs.claude-code.version;
                nativeBuildInputs = [ pkgs.makeWrapper ];
              }
              ''
                # Create a symlink tree of the original package
                mkdir -p $out/bin
                ln -s ${pkgs.claude-code}/bin/claude $out/bin/claude

                # Wrap the symlink — it becomes a shell script that sets
                # env vars and then calls the original binary.
                wrapProgram $out/bin/claude \
                --run 'export CLAUDE_CODE_OAUTH_TOKEN=$(cat ${scoped.claude.secrets.${token_name}.path})'
              '';
        };
      };
  };
}
