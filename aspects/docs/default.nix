{
  config,
  inputs,
  self,
  lib,
  den,
  ...
}:
{
  perSystem =
    {
      config,
      pkgs,
      lib,
      system,
      ...
    }:
    {
      apps.docs =
        let
          # All hosts as a flat attrset: { name -> host }
          allHosts = lib.mergeAttrsList (builtins.attrValues den.hosts);
          allAspectNames = builtins.attrNames den.aspects;

          # Aspects that declare provides.${hostName}
          providingAspects =
            hostName:
            builtins.filter (
              aspectName:
              let
                asp = den.aspects.${aspectName} or { };
              in
              builtins.isAttrs (asp.provides or null)
              && builtins.hasAttr hostName asp.provides
            ) allAspectNames;

          data = {
            systems = lib.mapAttrsToList (
              hostName: host:
              {
                name = hostName;
                system = host.system or hostName;
                roles = lib.concatStringsSep ", " (host.roles or [ ]);
                aspects = map (n: { name = n; }) (providingAspects hostName);
              }
            ) allHosts;

            edges = builtins.concatLists (
              lib.mapAttrsToList (
                hostName: _:
                map (aspectName: {
                  from = aspectName;
                  to = hostName;
                }) (providingAspects hostName)
              ) allHosts
            );
          };

          input = pkgs.writeText "input.yaml" (pkgs.lib.generators.toYAML { } data);
        in
        {
          type = "app";
          program = "${pkgs.writeShellScript "create-docs" ''
            cat ${input} | ${pkgs.mustache-go}/bin/mustache ${./README.md.mustache} >README.md
          ''}";
        };
    };
}
