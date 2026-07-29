{ den, ... }: {
  den.aspects.user-pwd = {
    includes = [ den.aspects.agenix-rekey ];
    secrets = { scoped, ... }: {
      ivy-password = {
        rekeyFile = ./ivy-password.age;
        intermediary = true;
      };
      ivy-pwd-hash.generator = {
        dependencies = [ scoped.secrets.ivy-password ];
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
      os = { scoped, ... }: {
        users.users.${user.name}.hashedPasswordFile = scoped.user-pwd.secrets.ivy-pwd-hash.file;
      };
    };
}
