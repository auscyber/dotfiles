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
      system,
      ...
    }:
    let
      # Build an extended lib that includes lib.extra (toBase64, etc.) and lib.hm,
      # matching how mkExtendedLib works in lib/systems/common.nix.
      extendedLib = inputs.nixpkgs.lib.extend self.lib.overlay;

      # Stub specialArgs that various modules require at evaluation time.
      stubArgs = {
        inherit inputs;
        inherit (inputs) self;
        flakeConfig = {
          flake.auscybernix = {
            builders = {
              buildMachines = { };
              sshKeys = [ ];
              extraBuildMachines = { };
            };
            vpn.configMap = { };
          };
        };
        systemIdentifier = "options-doc";
        hostname = "options-doc";
        inherit system;
        isInside = false;
      };

      # Evaluate all custom modules together to collect every auscybernix.* option
      # declaration.  A freeformType is used so that config assignments to
      # NixOS/Darwin/Home-Manager namespaces (age.*, programs.*, system.*, …) are
      # accepted without needing all upstream modules to be present.
      modulesEval = extendedLib.evalModules {
        specialArgs = {
          inherit pkgs;
          lib = extendedLib;
        };
        modules =
          # Directories whose default.nix files are discovered automatically.
          (self.lib.file.importModulesRecursive ../modules/nixos)
          ++ (self.lib.file.importModulesRecursive ../modules/darwin)
          ++ (self.lib.file.importModulesRecursive ../modules/common)
          ++ (self.lib.file.importModulesRecursive ../modules/home)
          # Standalone .nix files in modules/common/ that importModulesRecursive
          # does not pick up (it only discovers directories containing default.nix).
          ++ [
            ../modules/common/allConfigs.nix
            ../modules/common/vpn.nix
            ../modules/common/secrets.nix
            ../modules/common/ssh-keys.nix
            ../modules/common/pia.nix
            ../modules/common/nix/builders.nix
          ]
          ++ [
            # Accept config assignments to any undeclared option path.
            { _module.freeformType = extendedLib.types.attrsOf extendedLib.types.anything; }
            # Provide stub values for config.* references used in option defaults
            # (e.g. auscybernix.nix.builders.builderConfig.publicHostKey uses
            # config.age.rekey.hostPubkey, and the ipAddress option uses
            # config.auscybernix.vpn.ipAddress).
            {
              # Dummy value satisfying config.age.rekey.hostPubkey references in
              # option defaults (e.g. builders publicHostKey).  Not used for real
              # secrets; only present so the options evaluation succeeds.
              age.rekey.hostPubkey = "age1qyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqs3290gq";
              auscybernix.vpn.ipAddress = extendedLib.mkDefault null;
            }
            # Forward the required specialArgs as _module.args.
            { _module.args = stubArgs; }
          ];
      };

      optionsDoc = pkgs.nixosOptionsDoc {
        options = builtins.removeAttrs modulesEval.options [ "_module" ];
      };
    in
    {

      # Combined options documentation package.  The derivation exposes both a
      # human-readable Markdown file (options.md) and a machine-readable JSON
      # file (options.json) containing all option declarations collected across
      # the NixOS, Darwin, Home-Manager and common modules.
      packages.options-doc = pkgs.runCommand "options-doc" { } ''
        mkdir -p $out
        cp ${optionsDoc.optionsCommonMark} $out/options.md
        cp ${optionsDoc.optionsJSON}/share/doc/nixos/options.json $out/options.json
      '';

      apps.docs =
        let
          systems = self.lib.file.parseSystemConfigurations ../systems;
          homes = self.lib.file.parseHomeConfigurations ../homes;
          displayLine = name: value: self.lib.extra.displayLine name (walkConfig value) 0;
          getSecrets =
            conf: displayLine "secrets" (walkConfig (lib.mapAttrs (k: v: v.name) conf.age.secrets));

          data = {

            homes = pkgs.lib.mapAttrsToList (
              name:
              { system, hostname, ... }:
              {
                inherit name system;
                id = "home-${name}-${system}";
                description = self.homeConfigurations.${name}.config.auscybernix.meta.description or "";
                config =
                  displayLine "config" self.homeConfigurations.${name}.config.auscybernix
                  + "\n"
                  + getSecrets self.homeConfigurations.${name}.config;

                #          builtins.readFile (pkgs.runCommand "script"  {} ''
                #				echo '${builtins.toJSON (walkConfig self.homeConfigurations.${name}.config.auscybernix)}'
                #				echo '${builtins.toJSON (walkConfig self.homeConfigurations.${name}.config.auscybernix)}' | ${lib.getExe pkgs.jq} > $out
                #				'');

              }
            ) homes;
            systems = pkgs.lib.mapAttrsToList (
              name:
              { system, hostname, ... }:
              let
                config =
                  if lib.strings.hasSuffix "linux" system then
                    self.nixosConfigurations.${name}.config
                  else if lib.strings.hasSuffix "darwin" system then
                    self.darwinConfigurations.${name}.config
                  else
                    self.nixosConfigurations."${name}".config;
              in
              {
                inherit name system hostname;
                id = "system-${name}-${system}";
                description = config.auscybernix.meta.description or "";
                config = displayLine "config" config.auscybernix + "\n" + getSecrets config;

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
