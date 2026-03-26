{
  inputs,
  ...
}:
{
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
    secrets           = true;
    secrets-platform  = true;
    "1password"       = true;
    # ── nixos ─────────────────────────────────────────────────────────────
    ssh               = true;
    ext-stylix        = true;
    ext-home-manager  = true;
    ext-agenix        = true;
    ext-agenix-rekey  = true;
    ext-sops          = true;
  };

  imports = [
    inputs.nixos-hardware.nixosModules.microsoft-surface-common
    inputs.nixos-hardware.nixosModules.microsoft-surface-laptop-amd
    ./configuration.nix
  ];

}
