{ inputs, ... }:
{
  ff.claude-code.url = "github:sadjow/claude-code-nix";
  ff.claude-code.inputs.nixpkgs.follows = "nixpkgs";
  den.aspects.claude = {
    secrets.claude_token.rekeyFile = ./claude_token.age;
    overlays.claude-code = inputs.claude-code.overlays.default;
    homeManager = { pkgs, ... }: {
      programs.claude-code = {
        enable = true;
        enableMcpIntegration = true;
        package = pkgs.claude-code;
      };
    };
  };
}
