{
  config,
  inputs,
  self,
  ...
}:
{

  perSystem =
    { config, pkgs, ... }:
    {

      apps.docs =
        let
          systems = self.lib.file.parseSystemConfigurations ../systems;
          data = {

            systems = pkgs.lib.mapAttrsToList (
              name:
              { system, hostname, ... }:
              {
                inherit name system;

              }
            ) systems;
          };
          input = pkgs.writeText "input.yaml" (pkgs.lib.generators.toYAML { } data);

        in
        {
          type = "app";
          program = "${pkgs.writeShellScript "create-docs" ''
            		  cat ${input} | ${pkgs.mustache-go}/bin/mustache ${./README.md.mustache} > README.md
            		''}";
        };

    };
}
