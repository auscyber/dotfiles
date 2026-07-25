{ den, ... }:
{
  den.aspects.wakatime = {
    includes = [ den.aspects.agenix-rekey ];
    nvim.plugins.wakatime.enable = true;
    secrets.wakatime.rekeyFile = ../../secrets/wakatime_api.age;
    homeManager = { config, ... }: {
      age.templates = {
        wakatime_config = {
          dependencies = {
            wakatime = config.age.secrets.wakatime;
          };
          content =
            {
              pkgs,
              placeholders,
              ...
            }:
            ''
              [settings]
              api_key = ${placeholders.wakatime}

            '';
          path = "${config.home.homeDirectory}/.wakatime.cfg";
        };
      };
    };
  };
}
