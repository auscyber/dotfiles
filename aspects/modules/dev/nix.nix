{ den, rootPath, ... }:
{
  den.aspects.dev-nix = { user, ... }: {
    templates =
      {
        config,
        user,
        secrets,
        ...
      }:
      {
        "extra-nix-conf" = {
          dependencies = {
            inherit (secrets) nix_github_token;
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
      };
    secrets = { config, user, ... }: {
      nix_github_token = {
        rekeyFile = rootPath + "/secrets/github_token.age";
      };
    };
    homeManager =
      { pkgs, config, ... }:
      {

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
