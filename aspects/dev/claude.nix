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
        token_name = "claude_token";
      in

      {
        programs.claude-code = {
          enable = true;
          enableMcpIntegration = true;
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
