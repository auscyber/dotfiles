{ den, ... }:
{
  # Standalone aspect - for users on non-NixOS/Darwin hosts
  # nh environment variables are now set via den.policies.nh-env in base/nh.nix
  # based on the user's flakeFolder
  den.aspects.standalone = {
    standaloneHome = {
      # Add standalone-specific config here if needed
    };
  };
}
