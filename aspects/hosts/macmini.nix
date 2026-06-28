{ den, ... }:
{
  den.hosts.aarch64-darwin.macmini = {
    roles = [ "gui" ];
    users.ivypierlot = {
      roles = [ "gui" ];
    };
  };

  den.aspects.macmini = {
    includes = [
      den.aspects.homebrew
    ];

    darwin = {
      system.primaryUser = "ivypierlot";
      users.users.ivypierlot = {
        name = "ivypierlot";
        home = "/Users/ivypierlot";
      };
    };

    brew.casks = [
      "visual-studio-code"
      "postman"
      "bartender"
    ];
  };

  den.aspects.ivypierlot.provides.macmini = { };
}
