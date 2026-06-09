{ lib, den, ... }:
let
  nginxVhosts =
    { class, aspect-chain }:
    den.batteries.forward {
      each = lib.singleton true; # single forward; item ignored
      fromClass = _: "vhosts"; # the custom class you write into
      intoClass = _: "nixos"; # services.nginx.virtualHosts is a NixOS option
      intoPath = _: [
        "services"
        "nginx"
        "virtualHosts"
      ];
      fromAspect = _: lib.head aspect-chain;
      adaptArgs = lib.id;
    };
in
{
  den.aspects.nginx = {
    includes = [ nginxVhosts ];
    nixos = {
      security.acme = {
        acceptTerms = true;
        defaults.email = "ivyp@outlook.com.au";
        defaults.dnsProvider = "cloudflare";
      };

      services.nginx = {
        enable = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;
        recommendedOptimisation = true;
      };
    };
  };
}
