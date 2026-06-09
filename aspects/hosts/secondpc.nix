{ den, ... }:
{
  den.hosts.x86_64-linux.secondpc = {
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICj7wlOxTp0NQJoUhRtj7k8gtDC0lCr5MJqLV5LxG9Yf root@kexec-minimal";
    users.auscyber = { };
  };

  den.aspects.secondpc = {
    includes = [
      den.aspects.nginx
      den.aspects.nix
      den.aspects.vpn-server
    ];
  };
  den.aspects.auscyber = {
    includes = [
      den.aspects.agenix-rekey
    ];
    secrets."bob" = {
      generator.script = "alnum";
    };
  };
}
