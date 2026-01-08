{
  self,
  inputs,
  lib,
  ...
}:
{
  imports = [

    inputs.agenix-rekey.flakeModule
    ./systems.nix
    ../lib
    ../docs
    ../overlays
    ./homes.nix

    ./secrets.nix
    ./shells.nix
    ../ci
    ./packages.nix
    ./input-branches.nix
    inputs.flake-parts.flakeModules.partitions
    inputs.flake-parts.flakeModules.flakeModules
    inputs.input-branches.flakeModules.default
    ./formatter.nix
    ./templates.nix
  ];
  flake = {

  };
  perSystem =
    { config, pkgs, ... }:
    {
      apps.initHome = {
        type = "app";
        program = pkgs.writeShellScriptBin "init-home" ''
          	instance_folder="./homes/${pkgs.stdenv.system}/$USER@$(hostname -s)"
          	if ! [ -d "$instance_folder" ]; then
          	  echo "Creating home instance folder at $instance_folder"
          	else
          	  echo "Home instance folder $instance_folder already exists"
          	  exit 1
          	fi
          	echo "Creating default.nix in $instance_folder"
          	mkdir -p "$instance_folder"
          	cat  << EOF > "$instance_folder/default.nix"
          	{config, pkgs,...}:
          	{
          	auscybernix.nix.flake = "$CWD";
          	home.stateVersion = "24.05";
          	}
          	EOF
          	'';
      };

    };

}
