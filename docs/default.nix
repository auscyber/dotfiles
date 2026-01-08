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
		  displayLine = name: value: self.lib.extra.displayLine name (walkConfig value) 0;
		  getSecrets = conf: displayLine "secrets" (walkConfig (lib.mapAttrs (k: v: v.name ) conf.age.secrets));



          data = {

            homes = pkgs.lib.mapAttrsToList (
              name:
              { system, hostname, ... }:
              {
                inherit name system;
                id = "home-${name}-${system}";
                config = displayLine "config"
                  self.homeConfigurations.${name}.config.auscybernix
                  + "\n" + getSecrets
				  self.homeConfigurations.${name}.config;

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
                config = let config =
if lib.strings.hasSuffix "linux" system then
                    self.nixosConfigurations.${name}.config
                  else if lib.strings.hasSuffix "darwin" system then
                    self.darwinConfigurations.${name}.config
                  else
                    self.nixosConfigurations."${name}".config;
					in

				displayLine "config" config.auscybernix
                  + "\n" + getSecrets config;

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
