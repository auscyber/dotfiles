{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.programs._1password-cli;

in
{

  options.auscybernix.programs._1password-cli = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable 1Password shell plugins for bash, zsh, and fish shell.";
    };

  };
  config = lib.mkIf cfg.enable {
    programs._1password-shell-plugins = {
      # enable 1Password shell plugins for bash, zsh, and fish shell
      enable = true;
      # the specified packages as well as 1Password CLI will be
      # automatically installed and configured to use shell plugins
      plugins = with pkgs; [
        gh
        (nodePackages.vercel.overrideAttrs (res: {
          meta = res.meta // {
            mainProgram = "vercel";
          };
        }))
        pkgs.awscli2
      ];
    };
  };

}
