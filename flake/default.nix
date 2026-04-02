{ inputs, ... }:
{
  # External flake modules and out-of-tree directories that import-tree
  # cannot auto-discover.  All *.nix files inside ./flake/ (except those
  # under _-prefixed paths) are picked up automatically by import-tree.
  imports = [
    inputs.agenix-rekey.flakeModule
    inputs.nix-topology.flakeModule
    inputs.flake-parts.flakeModules.partitions
    inputs.flake-parts.flakeModules.flakeModules
    inputs.input-branches.flakeModules.default
    ../lib
    ../docs
    ../overlays
    ../ci
  ];

  perSystem =
    { pkgs, ... }:
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
          cat << EOF > "$instance_folder/default.nix"
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
