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

      classFor = host:
        if lib.strings.hasSuffix "darwin" (host.system or "") then "darwin"
        else if lib.strings.hasSuffix "linux" (host.system or "") then "nixos"
        else "unknown";

      classesFor = host:
        let c = classFor host;
        in if c == "darwin" then [ "darwin" "homeManager" ]
           else if c == "nixos" then [ "nixos" "homeManager" ]
           else [ "homeManager" ];

      # Get direct includes of an aspect (extract .name from each)
      getIncludes = aspectName:
        let
          asp = den.aspects.${aspectName} or { };
          includes = asp.includes or [ ];
        in
        builtins.filter (n: n != null) (
          map (inc:
            if builtins.isAttrs inc && inc ? name then inc.name
            else null
          ) includes
        );

      # Build text tree recursively
      buildTree = depth: seen: prefix: isLast: name:
        if depth <= 0 || builtins.elem name seen then
          "${prefix}${if isLast then "└── " else "├── "}${name}${if builtins.elem name seen then " (circular)" else " ..."}\n"
        else
          let
            children = getIncludes name;
            newSeen = seen ++ [ name ];
            line = "${prefix}${if isLast then "└── " else "├── "}${name}\n";
            childPrefix = prefix + (if isLast then "    " else "│   ");
            childLines = lib.imap0 (i: child:
              buildTree (depth - 1) newSeen childPrefix (i == (builtins.length children - 1)) child
            ) children;
          in
          line + lib.concatStrings childLines;

      # Build tree for a list of root aspects
      buildAspectTree = rootAspects:
        lib.concatStrings (
          lib.imap0 (i: name:
            buildTree 5 [ ] "" (i == (builtins.length rootAspects - 1)) name
          ) rootAspects
        );

      # Generate diagram for a single host
      hostDiagram = hostName: host:
        let
          captured = den.lib.capture.captureWithPathsWith {
            classes = classesFor host;
            root = den.lib.resolveEntity "host" { host = host; };
            ctx = { inherit host; };
          };

          # Filter entries to exclude provides and policies
          filteredEntries = builtins.filter (
            entry:
            let
              path = entry.path or [ ];
              pathStr = lib.concatStringsSep "." path;
            in
            !(lib.hasInfix "provides" pathStr)
            && !(lib.hasInfix "policies" pathStr)
            && !(lib.hasInfix "policy" pathStr)
          ) captured.entries;

          g = diagram.context {
            entries = filteredEntries;
            ctxTrace = captured.ctxTrace;
            name = hostName;
          };
        in
        {
          mermaid = diagram.toMermaid g;
        };

      # Aspects that have this host in provides or match by name
      hostAspects = hostName:
        builtins.filter (
          aspectName:
          let asp = den.aspects.${aspectName} or { };
          in aspectName == hostName
             || (builtins.isAttrs (asp.provides or null) && builtins.hasAttr hostName asp.provides)
        ) (builtins.attrNames den.aspects);

      data = {
        systems = lib.mapAttrsToList (
          hostName: host:
          let
            diag = hostDiagram hostName host;
            aspects = hostAspects hostName;
          in {
            name = hostName;
            system = host.system or hostName;
            class = classFor host;
            roles = lib.concatStringsSep ", " (host.roles or [ ]);
            tree = buildAspectTree aspects;
            mermaid = diag.mermaid;
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
    };
}
