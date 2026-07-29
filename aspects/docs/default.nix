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
  # Moved from the now-deleted aspects/docs/diagram.nix, which was this
  # module's only other declarer of the `den-diagram` input.
  ff.den-diagram.url = "github:denful/den-diagram";
  # Without this, den-diagram pulls its own full nixpkgs (a separate
  # releases.nixos.org nixexprs.tar.xz) rather than reusing ours.
  ff.den-diagram.inputs.nixpkgs.follows = "nixpkgs";

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

          # `simplified` was pulling in `aspectsOnly`, which unconditionally
          # drops every edge tagged style = "provide" (provider-provenance
          # edges). For several nodes -- including, on some no-role hosts,
          # literally every edge out of the host root -- that "provide" edge
          # was their ONLY connection, so `simplified` left them as
          # disconnected floating nodes (confirmed: 7-11 orphaned nodes per
          # host after regenerating, with the host root itself orphaned on
          # 5 of them). Use `filterUserAspects` instead: it drops the same
          # <anon>/resolve(...)/wrapper noise but keeps every edge,
          # including "provide" ones, so real structural connections aren't
          # severed as a side effect of "simplifying".
          isPolicyNode = n: (builtins.match "<policy:.*" n.label) != null;
          gBase = diagram.graph.filterUserAspects g;
          policyKeptIds = lib.listToAttrs (
            map (n: {
              name = n.id;
              value = true;
            }) (builtins.filter (n: !(isPolicyNode n)) gBase.nodes)
          );
          gNoPolicy = gBase // {
            nodes = builtins.filter (n: policyKeptIds ? ${n.id}) gBase.nodes;
            edges = builtins.filter (e: policyKeptIds ? ${e.from} && policyKeptIds ? ${e.to}) gBase.edges;
          };

          # `den` traces the WHOLE aspect tree for every host regardless of
          # whether an aspect actually applies to it -- that's why
          # darwin-base/darwin-finder/etc showed up even on plain NixOS
          # hosts, identically across all 10 hosts. `diagram.graph.classSlice`
          # is den-diagram's own filter for exactly this: it keeps a node if
          # IT (or anything under it) actually contributes to a given class,
          # via ancestor closure -- so organizational/entity nodes (e.g. the
          # user entity "ivypierlot") that don't carry hasClass themselves
          # but anchor real content underneath them are correctly kept
          # instead of left dangling. It only takes one class at a time, so
          # union the slices across this host's two traced classes
          # (nixos/darwin + homeManager).
          perClassSlices = map (c: diagram.graph.classSlice c gNoPolicy) classes;
          keptIds = lib.foldl' (
            acc: slice: lib.foldl' (acc': n: acc' // { ${n.id} = true; }) acc slice.nodes
          ) { } perClassSlices;

          gFiltered = gNoPolicy // {
            nodes = builtins.filter (n: keptIds ? ${n.id}) gNoPolicy.nodes;
            edges = builtins.filter (e: keptIds ? ${e.from} && keptIds ? ${e.to}) gNoPolicy.edges;
          };

          rc = diagram.renderContext {
            inherit pkgs;
            theme = diagram.themeFromBase16 {
              inherit pkgs;
              scheme = "catppuccin-mocha";
            };
          };

          # Same node set the diagram renders, as a plain sorted list of
          # display labels -- host/user scoped duplicates (e.g. "shell"
          # applied at both host and user level) collapse to one entry
          # since they share a label, and the root itself is excluded.
          aspectNames = lib.unique (
            map (n: n.label) (builtins.filter (n: n.id != gFiltered.rootId) gFiltered.nodes)
          );
        in
        {
          mermaid = diagram.toMermaid gFiltered;
          svg = rc.mmdSourceToSvg hostName (diagram.toMermaid gFiltered);
          dot = diagram.toDot gFiltered;
          aspects = lib.sort (a: b: a < b) aspectNames;
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
          aspects = hostDiagrams.${hostName}.aspects;
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
