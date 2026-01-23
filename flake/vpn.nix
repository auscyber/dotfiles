inp@{
  config,
  self,
  inputs,
  ...
}:
let
  lib = inputs.nixpkgs.lib;
in
{

  flake.auscybernix.vpn.configMap =
    let
	filteredConfigs = self.lib.extra.filterDummy (lib.filterAttrs (name: value: value.config.auscybernix.vpn.enable) config.flake.auscybernix.systems);
      folded =
        builtins.foldl'
          (out: config: {
            num = out.num + 1;
            configs = out.configs // {
              "${config.name}" = {
                pubkey = builtins.readFile config.pubkey;
				description = config.description;
                ipAddress = "10.100.0.${builtins.toString out.num}/24";
              };

            };
          })
          {
            num = 2;
            configs = { };
          }
          (
            lib.mapAttrsToList (name: value: {
			  name = "${name}-${value.system.system}";
			  description = "${name}";
                pubkey = value.config.auscybernix.vpn.pubkey;
            }) filteredConfigs
          );
    in
    folded.configs;

}
