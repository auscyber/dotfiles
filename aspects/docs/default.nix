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

          # filterUserAspects/simplified don't touch these: the `isPolicyDispatch`
          # node field turned out not to be set on them (verified against actual
          # generated output, not just the library source). What's actually
          # reliable is the rendered label -- every one of these nodes has a
          # label starting with `<policy:`, e.g. `<policy:ccache-role-dev>[6]`
          # or the nested `<policy:<policy:onepassword-role-gui>[132]/to-hosts>[0]`.
          # There's one per aspect x role combination, and they dominate node
          # count on any host with roles. Match on the label directly instead
          # of trusting the isPolicyDispatch flag.
          gSimplified = diagram.graph.simplified g;
          isPolicyNode = n: (builtins.match "<policy:.*" n.label) != null;

          # `den` traces the WHOLE aspect tree for every host regardless of
          # whether an aspect actually applies to it -- that's why
          # darwin-base/darwin-finder/etc show up even on plain NixOS hosts,
          # identically across all 10 hosts. `hasClass` (per this host's
          # traced classes) is the field that says "this aspect actually
          # contributes content". A flat hasClass filter is wrong though --
          # it drops organizational/entity nodes (e.g. the user entity
          # "ivypierlot") that don't carry hasClass themselves but anchor
          # real hasClass=true aspects underneath them, leaving those
          # aspects dangling/disconnected. Need the ancestor closure instead:
          # keep a node if IT is relevant OR IT'S AN ANCESTOR of a relevant
          # node -- same strategy den-diagram's own `classSlice` filter uses
          # (generalized across this host's two traced classes, since
          # classSlice only takes one).
          hasAnyClass = n: builtins.any (c: n.perClass.${c}.hasClass or false) classes;

          # parent adjacency: child id -> [parent ids], built from edges
          # (edge.from is the parent, edge.to is the child).
          parentsOf =
            let
              adj = lib.foldl' (
                acc: e: acc // { ${e.to} = (acc.${e.to} or [ ]) ++ [ e.from ]; }
              ) { } gSimplified.edges;
            in
            id: adj.${id} or [ ];

          seedIds = map (n: n.id) (builtins.filter hasAnyClass gSimplified.nodes);

          relevantIds =
            let
              expand =
                id: visited:
                if visited ? ${id} then
                  visited
                else
                  lib.foldl' (acc: p: expand p acc) (visited // { ${id} = true; }) (parentsOf id);
            in
            lib.foldl' (acc: id: expand id acc) { } seedIds;

          isIrrelevant = n: !(relevantIds ? ${n.id}) && n.id != gSimplified.rootId;

          keepNode = n: !(isPolicyNode n) && !(isIrrelevant n);
          keptIds = lib.listToAttrs (
            map (n: {
              name = n.id;
              value = true;
            }) (builtins.filter keepNode gSimplified.nodes)
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
