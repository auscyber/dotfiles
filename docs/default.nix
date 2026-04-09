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
      extendedLib = inputs.nixpkgs.lib.extend self.lib.overlay;

      # Poison module: prevents accidental config dependencies during doc generation
      # Accessing any option value returns a dummy stub to allow docs generation
      # This encourages using defaultText instead of config-dependent defaults
      poisonModule =
        { options, ... }:
        {
          config = extendedLib.listToAttrs (
            extendedLib.mapAttrsToList (
              name: _:
              if extendedLib.hasPrefix "_" name then
                extendedLib.nameValuePair name (extendedLib.mkDefault { })
              else
                extendedLib.nameValuePair name (extendedLib.mkDefault "")
            ) options
          );
        };

      nixosBaseModuleList = self.lib.file.filterModuleListForOptionsDoc {
        moduleList = import "${inputs.nixpkgs}/nixos/modules/module-list.nix";
        excludedPaths = [ "${inputs.nixpkgs}/nixos/modules/services/networking/atticd.nix" ];
      };

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
        homeDirectory = "/tmp/options-doc";
        username = "options-doc";
        inherit system;
        isInside = false;
      };

      buildOptionsDocs =
        {
          name,
          modules,
          class ? null,
          includePoison ? false,
        }:
        let
          hasNixosOptionDocs = builtins.hasAttr "nixos-option-docs" pkgs;
          nixosOptionDocsBin =
            if hasNixosOptionDocs then "${pkgs."nixos-option-docs"}/bin/nixos-option-docs" else "";
          modulesEval = extendedLib.evalModules (
            {
              specialArgs = {
                inherit pkgs inputs;
                lib = extendedLib;
              };
              modules = modules ++ [ { _module.check = false; } ] ++ lib.optional includePoison poisonModule;
            }
            // lib.optionalAttrs (class != null) { inherit class; }
          );
          optionsDoc = pkgs.nixosOptionsDoc {
            options = builtins.removeAttrs modulesEval.options [ "_module" ];
            warningsAreErrors = false;
          };
        in
        pkgs.runCommand name { } ''
          set -euo pipefail
          mkdir -p $out
          cp ${optionsDoc.optionsJSON}/share/doc/nixos/options.json $out/options.json

          # Prefer nixos-option-docs if present, otherwise synthesize markdown from JSON.
          if [ -n "${nixosOptionDocsBin}" ] && [ -x "${nixosOptionDocsBin}" ]; then
            if "${nixosOptionDocsBin}" --help >/dev/null 2>&1; then
              if "${nixosOptionDocsBin}" "$out/options.json" > "$out/options.md" 2>/dev/null; then
                exit 0
              fi
            fi
          fi

          {
            echo "# Options"
            echo
            echo "Generated from options.json (CommonMark renderer bypassed)."
            echo
            ${pkgs.jq}/bin/jq -r '
              def asText:
                if . == null then
                  "n/a"
                elif (type == "object") then
                  (.text // .literalExpression // tostring)
                else
                  tostring
                end;

              to_entries
              | sort_by(.key)
              | .[]
              | . as $entry
              | [
                  "## `\($entry.key)`",
                  "",
                  "- **Type:** `\(($entry.value.type // "") | asText)`",
                  "- **Default:** `\(($entry.value.default // null) | asText)`",
                  "- **Example:** `\(($entry.value.example // null) | asText)`",
                  "- **Read-only:** `\(($entry.value.readOnly // false) | tostring)`",
                  "",
                  "### Description",
                  "",
                  (($entry.value.description // "n/a") | tostring),
                  "",
                  "### Declarations",
                  "",
                  (if ($entry.value.declarations // []) | length == 0 then
                     "- n/a"
                   else
                     (($entry.value.declarations // []) | map("- `" + tostring + "`") | join("\n"))
                   end),
                  "",
                  "---",
                  ""
                ]
              | join("\n")
            ' "$out/options.json"
          } > "$out/options.md"
        '';

      homeOptionsModules = [
        { _module.args.lib = extendedLib; }
      ]
      ++ (import "${inputs.home-manager}/modules/modules.nix" {
        inherit pkgs;
        lib = extendedLib;
        check = false;
      })
      ++ self.auscybernix.importedHomeModules
      ++ self.auscybernix.standaloneHomeModules
      ++ [
        ../modules/common/secrets.nix
        ../modules/common/nix
        ../modules/common/allConfigs.nix
      ]
      ++ (extendedLib.importModulesRecursive ../modules/home)
      ++ [
        ../modules/home/default.nix
        {
          home.username = "options-doc";
          home.homeDirectory = "/tmp/options-doc";
          home.stateVersion = "24.11";
          age.rekey.storageMode = extendedLib.mkForce "local";
          age.rekey.localStorageDir = ../secrets/rekeyed + "/options-doc";
          age.rekey.hostPubkey = "age1qyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqs3290gq";
          auscybernix.standalone.enable = true;
          auscybernix.secrets.enable = true;
        }
      ];

      nixosOptionsModules = [
        { _module.args.lib = extendedLib; }
      ]
      ++ nixosBaseModuleList
      ++ [
        ../modules/common/allConfigs.nix
        ../modules/common/hm
        ../modules/common/ssh-keys.nix
      ]
      ++ (extendedLib.importModulesRecursive ../modules/nixos)
      ++ [
        {
          age.rekey.hostPubkey = "age1qyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqs3290gq";
          age.rekey.storageMode = "local";
          age.rekey.localStorageDir = ../secrets/rekeyed;
          auscybernix.vpn.ipAddress = extendedLib.mkDefault null;
        }
        { _module.args = stubArgs; }
      ];

      darwinOptionsModules = [
        { _module.args.lib = extendedLib; }
      ]
      ++ (import "${inputs.darwin}/modules/module-list.nix")
      ++ [
        ../modules/common/nix
        ../modules/common/secrets.nix
        ../modules/common/hm
        ../modules/common/common
        ../modules/common/allConfigs.nix
        ../modules/common/kmonad
        ../modules/common/ssh-keys.nix
      ]
      ++ (extendedLib.importModulesRecursive ../modules/darwin)
      ++ [
        {
          age.rekey.hostPubkey = "age1qyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqs3290gq";
          age.rekey.storageMode = "local";
          age.rekey.localStorageDir = ../secrets/rekeyed;
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
      # Contains NixOS, Darwin, and Home documentation
      packages.options-doc-home = buildOptionsDocs {
        name = "options-doc-home";
        modules = homeOptionsModules;
        class = "homeManager";
        includePoison = false;
      };

      packages.options-doc = pkgs.runCommand "options-doc" { } ''
        mkdir -p $out/nixos $out/darwin $out/home
        cp -r ${config.packages.options-doc-nixos}/* $out/nixos/
        cp -r ${config.packages.options-doc-darwin}/* $out/darwin/
        cp -r ${config.packages.options-doc-home}/* $out/home/
        # Provide copies at root level for easy access
        cp ${config.packages.options-doc-nixos}/options.md $out/options-nixos.md
        cp ${config.packages.options-doc-darwin}/options.md $out/options-darwin.md
        cp ${config.packages.options-doc-home}/options.md $out/options-home.md
        cp ${config.packages.options-doc-nixos}/options.json $out/options-nixos.json
        cp ${config.packages.options-doc-darwin}/options.json $out/options-darwin.json
        cp ${config.packages.options-doc-home}/options.json $out/options-home.json
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
