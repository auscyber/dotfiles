{ den, ... }: {
  den.aspects.secondpc.nixos = {
    services.vaultwarden = {
      enable = true;
      backupDir = "/var/local/vaultwarden/backup";
      config = {
        DOMAIN = "https://bitwarden.ivymect.in";
        SIGNUPS_ALLOWED = false;
        ROCKET_ADDRESS = "127.0.0.1";
        ROCKET_PORT = 8222;
        ROCKET_LOG = "critical";
        SMTP_HOST = "127.0.0.1";
        SMTP_PORT = 25;
        SMTP_SSL = false;
        SMTP_FROM = "admin@bitwarden.ivymect.in";
        SMTP_FROM_NAME = "ivymect.in Bitwarden server";
      };
    };
  };
}
