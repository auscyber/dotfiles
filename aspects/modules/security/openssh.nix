{ den, ... }:
{
  den.aspects.openssh = {
    nixos = {
      services.openssh = {
        enable = true;
        settings.StreamLocalBindUnlink = "yes";
        settings.AllowAgentForwarding = true;
        forwardX11 = true;
      };
      security.sudo.enable = true;
    };
  };
}
