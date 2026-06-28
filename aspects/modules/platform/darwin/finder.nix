{ den, ... }:
{
  den.aspects.darwin-finder = {
    darwin = {
      system.defaults.NSGlobalDomain.AppleShowAllExtensions = true;
      system.defaults.NSGlobalDomain.AppleTemperatureUnit = "Celsius";
      system.defaults.finder = {
        ShowPathbar = true;
        AppleShowAllExtensions = true;
        FXPreferredViewStyle = "clmv";
        ShowStatusBar = true;
      };
    };
  };
}
