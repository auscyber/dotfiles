{
  inputs,
  ...
}:

{
  perSystem =
    { system, pkgs, ... }:
    {
      apps.fetch = {
        type = "app";
        program = inputs.nvfetcher.packages."${system}".default;
      };

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

