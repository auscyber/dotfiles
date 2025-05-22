{ config
, lib
, pkgs
, ...
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

  programs.zsh.enable = true;
  nix = {
    # Binary Cache for Haskell.nix

    package = pkgs.nixVersions.latest;

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
