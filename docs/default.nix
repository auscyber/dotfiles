{
  config,
  inputs,
  self,
  lib,
  ...
}:
let
  walkConfig = self.lib.extra.walkConfig;
in
{

  perSystem =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {

      apps.docs =
        let
          systems = self.lib.file.parseSystemConfigurations ../systems;
          homes = self.lib.file.parseHomeConfigurations ../homes;

          data = {

            homes = pkgs.lib.mapAttrsToList (
              name:
              { system, hostname, ... }:
              {
                inherit name system;
                id = "home-${name}-${system}";
                config = self.lib.extra.displayLine "config" (walkConfig
                  self.homeConfigurations.${name}.config.auscybernix
                ) 0;

                #          builtins.readFile (pkgs.runCommand "script"  {} ''
                #				echo '${builtins.toJSON (walkConfig self.homeConfigurations.${name}.config.auscybernix)}'
                #				echo '${builtins.toJSON (walkConfig self.homeConfigurations.${name}.config.auscybernix)}' | ${lib.getExe pkgs.jq} > $out
                #				'');

              }
            ) homes;
            systems = pkgs.lib.mapAttrsToList (
              name:
              { system, hostname, ... }:
              {
                inherit name system hostname;
                id = "system-${name}-${system}";
                config = self.lib.extra.displayLine "config" (walkConfig (
                  if lib.strings.hasSuffix "linux" system then
                    self.nixosConfigurations.${name}.config.auscybernix
                  else if lib.strings.hasSuffix "darwin" system then
                    self.darwinConfigurations.${name}.config.auscybernix
                  else
                    { }
                )) 0;

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
