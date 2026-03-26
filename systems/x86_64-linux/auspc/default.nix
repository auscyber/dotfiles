{
  nixpkgs,

  ...
}:
{
  auscybernix.meta.description = "Main Gaming desktop";
  auscybernix.secrets.enable = true;

  auscybernix.modules.enable = {
    # ── generic (NixOS + nix-darwin) ─────────────────────────────────────
    allConfigs        = true;
    common            = true;
    hm                = true;
    nix               = true;
    ssh-keys          = true;
    vpn               = true;
    general           = true;
    builds-options    = true;
    builds-platform   = true;
    secrets           = true;
    secrets-platform  = true;
    "1password"       = true;
    # ── nixos ─────────────────────────────────────────────────────────────
    games             = true;
    bootlogo          = true;
    ssh               = true;
    ext-stylix        = true;
    ext-home-manager  = true;
    ext-agenix        = true;
    ext-agenix-rekey  = true;
    ext-lanzaboote    = true;
    ext-sops          = true;
  };

  imports = [
    ./configuration.nix
    ./hardware-configuration.nix
    ./graphics.nix
  ];

}
