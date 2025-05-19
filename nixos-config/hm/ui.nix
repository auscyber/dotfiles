{ config, pkgs, ... }:
let
  # onePassPath = "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock";
  onePassPath = "~/.1password/agent.sock";
in
{
  #  targets.genericLinux.enable = true;
  home.packages = with pkgs; [
    #input-leap
    discord
    #    wezterm
    spotify
    #    rnix-lsp
    #    beeper
    #    google-chrome
    #    thunderbird
  ];
  stylix = {
    enable = true;
    image = ../../backgrounds/boygenius-performs-gq.jpg;
  };
  #  programs._1password.enable = true;
  programs.ssh = {
    enable = false;
    extraConfig = ''
      Host *
          IdentityAgent ${onePassPath}
    '';
  };
}
