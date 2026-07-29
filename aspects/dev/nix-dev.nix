{
  den,
  rootPath,
  ...
}:
{
  # NOT a bare parametric aspect (`den.aspects.dev-nix = { user, ... }: { ... }`).
  # den coerces that shape to `{ includes = [ fn ]; }`, so the class content ends
  # up on an anonymous child aspect -- where `den.schema.aspect` never sees it
  # under the name `dev-nix`, and its secrets would not be scoped. Each class
  # body requests the context args it needs instead.
  den.aspects.dev-nix = {
    templates =
      {
        config,
        user,
        scoped,
        ...
      }:
      {
        "extra-nix-conf" = {
          dependencies = {
            inherit (scoped.secrets) nix_github_token;
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
    secrets =
      {
        config,
        user,
        ...
      }:
      {
        nix_github_token = {
          rekeyFile = rootPath + "/secrets/github_token.age";
        };
      };
    homeManager =
      {
        pkgs,
        scoped,
        ...
      }:
      {
        nix.extraOptions = ''
          !include ${scoped.dev-nix.access."extra-nix-conf".path}
        '';
        home.packages = with pkgs; [
          nil
          nixfmt
          cachix
          devenv
        ];
      };
  };
}
