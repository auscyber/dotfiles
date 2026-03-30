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

      # Module that prevents config references which would create circular dependencies
      # (inspired by home-manager's approach). This catches any attempts to reference
      # config values in option defaults/declarations, which would cause infinite recursion.
      poisonModule =
        let
          poisonAttr = n: {
            name = n;
            value = abort ''
              error: the option documentation generation has a dependency on the configuration.

              Accessed config path: config.${n}

              This usually happens when an option default or declaration references the config,
              like: default = ''${config.some.value};

              To fix this, use `defaultText` instead:
                defaultText = lib.literalExpression "\\''${config.some.value}";
            '';
          };
        in
        { options, ... }:
        {
          config = lib.listToAttrs (map poisonAttr (lib.filter (n: n != "_module") (lib.attrNames options)));
        };

      # Reusable function to build options docs for a given set of modules
      # This follows the home-manager pattern of using a poisonModule to catch bad references
      buildOptionsDocs =
        { name, modules }:
        let
          modulesEval = extendedLib.evalModules {
            specialArgs = {
              inherit pkgs inputs;
              lib = extendedLib;
            };
            modules = modules ++ [ { _module.check = false; } poisonModule ];
          };
          optionsDoc = pkgs.nixosOptionsDoc {
            options = builtins.removeAttrs modulesEval.options [ "_module" ];
            warningsAreErrors = false;
          };
        in
        pkgs.runCommand name { } ''
          mkdir -p $out
          cp ${optionsDoc.optionsCommonMark} $out/options.md
          cp ${optionsDoc.optionsJSON}/share/doc/nixos/options.json $out/options.json
        '';

      # NixOS options documentation
      # Uses safe modules that don't have config references in their option defaults
      # Matching the evaluation strategy used for actual NixOS configurations
      nixosOptionsModules =
        [
          ../modules/common/allConfigs.nix
          ../modules/common/ssh-keys.nix
          ../modules/common/builders
        ]
        ++ (self.lib.file.importModulesRecursive ../modules/nixos)
        ++ [
          # Stub config values to satisfy option defaults that reference them
          {
            age.rekey.hostPubkey = "age1qyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqs3290gq";
            auscybernix.vpn.ipAddress = extendedLib.mkDefault null;
          }
          # Forward the required specialArgs as _module.args.
          { _module.args = stubArgs; }
        ];

      # Darwin options documentation
      # Uses safe modules that don't have config references in their option defaults
      # Matching the evaluation strategy used for actual Darwin configurations
      darwinOptionsModules =
        [
          ../modules/common/allConfigs.nix
          ../modules/common/ssh-keys.nix
          ../modules/common/builders
        ]
        ++ (self.lib.file.importModulesRecursive ../modules/darwin)
        ++ [
          {
            age.rekey.hostPubkey = "age1qyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqs3290gq";
            auscybernix.vpn.ipAddress = extendedLib.mkDefault null;
          }
          { _module.args = stubArgs; }
        ];
    in
    {

      # NixOS-specific options documentation
      packages.options-doc-nixos = buildOptionsDocs {
        name = "options-doc-nixos";
        modules = nixosOptionsModules;
      };

      # Darwin-specific options documentation
      packages.options-doc-darwin = buildOptionsDocs {
        name = "options-doc-darwin";
        modules = darwinOptionsModules;
      };

      # Combined options documentation package (for backward compatibility)
      # Contains both NixOS and Darwin documentation
      packages.options-doc = pkgs.runCommand "options-doc" { } ''
        mkdir -p $out/nixos $out/darwin
        cp -r ${config.packages.options-doc-nixos}/* $out/nixos/
        cp -r ${config.packages.options-doc-darwin}/* $out/darwin/
        # Provide copies at root level for easy access
        cp ${config.packages.options-doc-nixos}/options.md $out/options-nixos.md
        cp ${config.packages.options-doc-darwin}/options.md $out/options-darwin.md
        cp ${config.packages.options-doc-nixos}/options.json $out/options-nixos.json
        cp ${config.packages.options-doc-darwin}/options.json $out/options-darwin.json
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
                #echo '${builtins.toJSON (walkConfig self.homeConfigurations.${name}.config.auscybernix)}'
                #echo '${builtins.toJSON (walkConfig self.homeConfigurations.${name}.config.auscybernix)}' | ${lib.getExe pkgs.jq} > $out
                #'');

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
