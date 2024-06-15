{ config, pkgs, ... }:
let
  # onePassPath = "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock";
  onePassPath = "~/.1password/agent.sock";
in
{
  nixpkgs.config.allowUnfree = true;
  targets.genericLinux.enable = true;
  home.packages = with pkgs; [
    discord
    wezterm
    spotify
    #    rnix-lsp
    (polybar.override {
      pulseSupport = true;
      iwSupport = true;
      githubSupport = true;
    })
    google-chrome
  
  ];
programs.ssh = {
    enable = true;
    extraConfig = ''
      Host *
          IdentityAgent ${onePassPath}
    '';
  };
}
