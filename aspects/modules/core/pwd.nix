{ den, ... }:
{
  den.aspects.user-pwd = {
    includes = [ den.aspects.agenix-rekey ];
    secrets =
      { secrets, ... }:
      {
        ivy-password = {
          rekeyFile = ./ivy-password.age;
          intermediary = true;
        };
        ivy-pwd-hash.generator = {
          dependencies = [ secrets.ivy-password ];
          script =
            {
              pkgs,
              lib,
              decrypt,
              deps,
              ...
            }:
            ''
              ${decrypt} ${lib.escapeShellArg (lib.head deps).file} | \
                  ${pkgs.openssl}/bin/openssl passwd -6 -stdin
            '';
        };
      };
  };

  den.aspects.auscyber.provides.to-hosts =
    {
      host,
      user,
      ...
    }:
    {
      includes = [ den.aspects.user-pwd ];
      os =
        { config, ... }:
        {
          users.users.${user.name}.hashedPasswordFile = config.age.secrets.ivy-pwd-hash.file;
        };

    };

}
