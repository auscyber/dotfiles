{ den, rootPath, ... }:
{
  den.aspects.dev-nix = {
    secrets.nix_github_token = {
      rekeyFile = rootPath + "/secrets/github_token.age";
    };
    homeManager =
      { pkgs, config, ... }:
      {
        age.templates."extra-nix-conf" = {
          dependencies = {
            inherit (config.age.secrets) nix_github_token;
          };
          content =
            {
              pkgs,
              placeholders,
              ...
            }:
            ''
              access-tokens = github.com=${placeholders.nix_github_token}
            '';
        };

        nix.extraOptions = ''
          !include ${config.age.templates."extra-nix-conf".path}
        '';
        home.packages = with pkgs; [
          nil
          nixfmt-rfc-style
          cachix
          devenv
        ];
      };
  };
}
