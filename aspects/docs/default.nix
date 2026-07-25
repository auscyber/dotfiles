{
  inputs,
  lib,
  den,
  ...
}:
let
  diagram = inputs.den-diagram.lib;

  # All hosts flattened: { hostName -> hostAttr }
  allHosts = lib.mergeAttrsList (builtins.attrValues den.hosts);
in
{
  perSystem =
    {
      pkgs,
      config,
      ...
    }:
    let
      # Generate diagram for a single host
      hostDiagram =
        hostName: host:
        let
          classes =
            if lib.hasSuffix "darwin" (host.system or "") then
              [
                "darwin"
                "homeManager"
              ]
            else
              [
                "nixos"
                "homeManager"
              ];

          captured = den.lib.capture.captureWithPathsWith {
            inherit classes;
            root = den.lib.resolveEntity "host" { inherit host; };
            ctx = { inherit host; };
          };

          g = diagram.context {
            entries = captured.entries;
            ctxTrace = captured.ctxTrace;
            name = hostName;
          };

          # filterUserAspects alone only drops <anon>/resolve(...) bookkeeping
          # and wrapper/context nodes -- it leaves every `<policy:aspect-role>[N]`
          # dispatch node in place, and those dominate node count (one per
          # aspect x role combination). `simplified` additionally folds
          # provider sub-aspects into their parents and flattens the
          # per-host entity-kind grouping. On top of that, drop the
          # remaining policy-dispatch routing nodes outright -- they're
          # pipeline plumbing for policy resolution, not aspects a user
          # declared, and pruning them (with edges touching them) is what
          # actually collapses the diagram to a readable size.
          gSimplified = diagram.graph.simplified g;
          keptIds = lib.listToAttrs (
            map (n: {
              name = n.id;
              value = true;
            }) (builtins.filter (n: !(n.isPolicyDispatch or false)) gSimplified.nodes)
          );
          gFiltered = gSimplified // {
            nodes = builtins.filter (n: keptIds ? ${n.id}) gSimplified.nodes;
            edges = builtins.filter (
              e: keptIds ? ${e.from} && keptIds ? ${e.to}
            ) gSimplified.edges;
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
          mermaid = diagram.toMermaid gFiltered;
          svg = rc.mmdSourceToSvg hostName (diagram.toMermaid gFiltered);
          dot = diagram.toDot gFiltered;
        };

      # Build diagrams for all hosts
      hostDiagrams = lib.mapAttrs hostDiagram allHosts;

      # Package info from legacyPackages.my
      myPackages = config.legacyPackages.my or { };
      packageList = lib.mapAttrsToList (name: pkg: {
        inherit name;
        version = pkg.version or "unknown";
        description = pkg.meta.description or "";
        homepage = pkg.meta.homepage or "";
      }) myPackages;

      # Data for mustache template
      data = {
        systems = lib.mapAttrsToList (hostName: host: {
          name = hostName;
          system = host.system or "unknown";
          roles = lib.concatStringsSep ", " (host.roles or [ ]);
          mermaid = hostDiagrams.${hostName}.mermaid;
        }) allHosts;

        packages = packageList;
      };

      input = pkgs.writeText "input.yaml" (lib.generators.toYAML { } data);

      # Write SVGs to files
      svgFiles = lib.mapAttrs (
        hostName: diag: pkgs.writeText "${hostName}.svg" (builtins.readFile diag.svg)
      ) hostDiagrams;
    in
    {
      apps.docs = {
        type = "app";
        program = "${pkgs.writeShellScript "create-docs" ''
          cat ${input} | ${pkgs.mustache-go}/bin/mustache ${./README.md.mustache} > README.md
          mkdir -p screenshots/diagrams
          ${lib.concatStringsSep "\n" (
            lib.mapAttrsToList (name: svg: "cp ${svg} screenshots/diagrams/${name}.svg") svgFiles
          )}
          echo "Generated README.md and diagrams in screenshots/diagrams/"
        ''}";
      };

      packages.docs-diagrams = pkgs.runCommand "docs-diagrams" { } ''
        mkdir -p $out
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: svg: "cp ${svg} $out/${name}.svg") svgFiles)}
      '';
    };
}
