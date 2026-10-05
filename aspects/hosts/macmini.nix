{ den, ... }: {
  den.hosts.aarch64-darwin.macmini = {
    roles = [ "gui" ];
    # Whose machine this is -- a different fact from the household it serves.
    owner = den.persons.ivy;

    users.ivypierlot = {
      # The human behind this account (../../persons/ivy.nix). The link
      # points account -> person, never the reverse; ../../base/person.nix says why.
      person = den.persons.ivy;
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
