{config,lib, pkgs,...}: 
with lib;
let 
  configedModule = mod: types.submoduleWith {
    modules = toList (import mod);
    specialArgs = { inherit config lib pkgs; };
  };
  application = types.submodule {
	options = {
		name = mkOption {
			type = types.str;
			default = "";
			description = "The name of the application or context for which this keybind is applicable.";
		};
		keybinds = mkOption {
			type = types.listOf (configedModule ./types/keybind.nix);
			default = [];
			description = "A list of keybind definitions for this application.";
		};
		description = mkOption {
			type = types.str;
			default = "";
			description = "A description of the application or context for documentation purposes.";
		};
		transformer = mkOption {
			type = configedModule ./types/transformers.nix;
			default = [];
			description = "A transformer that can convert keybinds for this application.";
		};
	};
};
in 
{

  options.
    services.keybindControl = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the keybind control service.";
      };
      applications = mkOption {
        type = types.attrsOf application;
        default = { };
        description = "A set of applications with their keybinds and transformers. Each application can have its own keybinds and transformers.";
      };
      groups = mkOption {
        type = types.attrsOf (types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              default = "";
              description = "The name of the group.";
            };
            description = mkOption {
              type = types.str;
              default = "";
              description = "A description of the group for documentation purposes.";
            };
          };
        });
        default = { };
        description = "A set of groups for organizing keybinds. Each group can have a name and description.";
      };


  };
  config = 
    let 
      cfg = config.services.keybindControl;
    evaluated = cfg.applications.skhd.transformer.textFunction cfg.applications.skhd.keybinds;
    allBinds = builtins.zipAttrsWith ((name: values: values)) (builtins.attrValues cfg.applications);
#      modules = mapAttrs (_: value: value.transformer.function value.keybinds) cfg.applications;
    
    in
    mkIf cfg.enable (mkMerge ([
      {
      home.file."test".text = "${builtins.trace allBinds.keybinds evaluated}";
      }]
       
     
   # ++ modules
    
    ));
}