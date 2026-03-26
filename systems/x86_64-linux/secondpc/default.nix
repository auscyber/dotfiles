{
  inputs,
  ...
}:
{
  auscybernix.meta.description = "Home server";
  auscybernix.secrets.enable = true;

  auscybernix.modules.enable = {
    # ── generic ───────────────────────────────────────────────────────────
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
    # ── nixos ─────────────────────────────────────────────────────────────
    ssh               = true;
    ext-home-manager  = true;
    ext-agenix        = true;
    ext-agenix-rekey  = true;
    ext-sops          = true;
    ext-arion         = true;
  };

  imports = [
    ./configuration.nix
    ./hardware-configuration.nix
    ./services.nix
    ./grafana.nix
    ./cache.nix
    ./vpn.nix
    ./navidrome.nix
  ];

}
