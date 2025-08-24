{
  config,
  lib,
  pkgs,
  ...
}:

{

  fonts.packages =
    with pkgs;
    (map (x: nerd-fonts.${x}) [
      "fira-code"
      "inconsolata"
      "hasklug"
      "roboto-mono"
    ]);

  environment.systemPackages = with pkgs; [
    git
    vim
    neovim
    wget
    htop
    curl
  ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.zsh.enable = true;
  nix = {
    # Binary Cache for Haskell.nix
    settings = {
      substituters = [
        "https://nix-community.cachix.org"
        "https://iohk.cachix.org"
        "https://cache.nixos.org"
        "https://devenv.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      ];
    };

    #    package = pkgs.nixVersions.latest;

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };

  nixpkgs.config.allowUnfree = true;

}
