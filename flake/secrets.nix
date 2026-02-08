{
  inputs,
  lib,
  self,
  ...
}:
{
  imports = [
  ];
  flake = {

  };

  perSystem =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      # Add `config.agenix-rekey.package` to your devshell to
      # easily access the `agenix` command wrapper.

      # You can define agenix-rekey.nixosConfigurations if you want to change which
      # hosts are considered for rekeying.
      # Refer to the flake.parts section on agenix-rekey to see all available options.
      agenix-rekey.nixosConfigurations =
        inputs.self.nixosConfigurations // inputs.self.darwinConfigurations; # (not technically needed, as it is already the default)
      agenix-rekey.homeConfigurations = inputs.self.homeConfigurations;
      agenix-rekey.collectHomeManagerConfigurations = true;
    };

}
