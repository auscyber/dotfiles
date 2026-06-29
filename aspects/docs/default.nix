{
  inputs,
  lib,
  den,
  ...
}:
let
  diagram = inputs.den-diagram.lib;
in
{
  perSystem =
    { pkgs, ... }:
    let
      # All hosts flattened: { hostName -> host }
      allHosts = lib.mergeAttrsList (builtins.attrValues den.hosts);

      # Generate diagram for a single host
      hostDiagram = hostName: host:
        let
          classes =
            if lib.strings.hasSuffix "darwin" (host.system or hostName) then
              [ "darwin" "homeManager" ]
            else
              [ "nixos" "homeManager" ];

          captured = den.lib.capture.captureWithPathsWith {
            inherit classes;
            root = den.lib.resolveEntity "host" { host = host; };
            ctx = { inherit host; };
          };

          g = diagram.context {
            entries = captured.entries;
            ctxTrace = captured.ctxTrace;
            name = hostName;
          };

          rc = diagram.renderContext {
            inherit pkgs;
            theme = diagram.themeFromBase16 {
              inherit pkgs;
              scheme = "catppuccin-mocha";
            };
          };
        in
        {
          mermaid = diagram.toMermaid g;
          dot = diagram.toDot g;
          svg = rc.mmdSourceToSvg hostName (diagram.toMermaid g);
        };

      hostDiagrams = lib.mapAttrs hostDiagram allHosts;

      data = {
        systems = lib.mapAttrsToList (
          hostName: host:
          {
            name = hostName;
            system = host.system or hostName;
            roles = lib.concatStringsSep ", " (host.roles or [ ]);
            mermaid = hostDiagrams.${hostName}.mermaid;
          }
        ) allHosts;
      };

      input = pkgs.writeText "input.yaml" (pkgs.lib.generators.toYAML { } data);
    in
    {
      apps.docs = {
        type = "app";
        program = "${pkgs.writeShellScript "create-docs" ''
          cat ${input} | ${pkgs.mustache-go}/bin/mustache ${./README.md.mustache} >README.md
        ''}";
      };

      # Individual SVG outputs per host
      packages = lib.mapAttrs' (
        hostName: diag:
        lib.nameValuePair "diagram-${hostName}" diag.svg
      ) hostDiagrams;
    };
}
