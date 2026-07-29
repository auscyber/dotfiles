{ inputs, ... }: {
  ff.claude-code.url = "github:sadjow/claude-code-nix";
  ff.claude-code.inputs.nixpkgs.follows = "nixpkgs";
  den.aspects.claude = {
    secrets.claude_token.rekeyFile = ./token.age;
    overlays.claude-code = inputs.claude-code.overlays.default;
    homeManager = { pkgs, scoped, ... }: {
      programs.claude-code = {
        enable = true;
        enableMcpIntegration = true;
        package =
          pkgs.runCommand "claude-wrapped"
            {
              nativeBuildInputs = [ pkgs.makeWrapper ];
            }
            ''
              # Create a symlink tree of the original package
              mkdir -p $out/bin
              ln -s${pkgs.claude-code}/bin/claude $out/bin/claude

              # Wrap the symlink — it becomes a shell script that sets
              # env vars and then calls the original binary.
              wrapProgram $out/bin/claude \
              --run "export CLAUDE_CODE_AUTH_TOKEN=$(cat ${scoped.claude.secrets.cclaude_token.path})"
            '';
      };
    };
  };
}
