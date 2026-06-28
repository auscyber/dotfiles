{ den, ... }:
{
  den.aspects.ssh-config = {
    homeManager =
      { ... }:
      {
        programs.ssh = {
          enable = true;
          enableDefaultConfig = false;
          settings = {
            "*".forwardAgent = true;
            "faggot.sh" = {
              hostname = "faggot.sh";
              user = "ivy";
              forwardAgent = true;
            };
            "secondpc" = {
              hostname = "121.200.22.213";
              forwardAgent = true;
              user = "auscyber";
            };
            "imflo.pet" = {
              hostname = "imflo.pet";
              forwardAgent = true;
              user = "ivy";
            };
            "auspc" = {
              hostname = "192.168.0.24";
              forwardAgent = true;
              user = "auscyber";
            };
          };
        };
      };
  };
}
