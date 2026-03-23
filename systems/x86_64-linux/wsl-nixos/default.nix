{

  ...
}:
{
  imports = [ ./configuration.nix ];
  #./../../modules/system/grub.nix
  #./boot.nix
  #    ./mailserver.nix
  #    ./minecraft.nix
  #  home-manager = {
  #    useGlobalPkgs = true;
  #    useUserPackages = true;
  #    users.nixos = {
  #      imports = [
  #
  #        (
  #          {
  #            config,
  #            pkgs,
  #            system,
  #            lib,
  #            modulesPath,
  #            ...
  #          }:
  #          {
  #            programs.git.extraConfig = {
  #
  #              core.sshCommand = "ssh.exe";
  #            };
  #
  #          }
  #        )
  #      ];
  #      home.username = "nixos";
  #      home.sessionVariables = {
  #        FLAKENAME = "nixos";
  #        NIXFLAKE = "$HOME/dotfiles/nixos-config";
  #      };
  #
  #    };
  #  };

}
