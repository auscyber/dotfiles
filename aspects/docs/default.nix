{
  inputs,
  lib,
  den,
  capabilityGraph,
  routeGraph,
  ...
}:
let
  diagram = inputs.den-diagram.lib;
  hostAspects = import ../../lib/host-aspects.nix { inherit lib den diagram; };

  # All hosts flattened: { hostName -> hostAttr }
  allHosts = lib.mergeAttrsList (builtins.attrValues den.hosts);
in
{
  # Moved from the now-deleted aspects/docs/diagram.nix, which was this
  # module's only other declarer of the `den-diagram` input.
  # ── ALWAYS INCLUDED, WHICH IS HOW THE INPUT GETS TRACED ──
  #
  # Nothing referred to this aspect, and that is the whole reason
  # `inputs.den-diagram` reached no layer flake: ../framework/layers.nix gathers
  # `inputs`-class declarations from `den.lib.capture.captureFleet`, so an aspect
  # no entity includes is never traced and its input is never placed. Measured
  # against the generator rather than guessed -- `flake.layerPlan` put
  # den-diagram in NO layer, root included -- and the symptom was that the whole
  # of this directory could not evaluate, `apps.docs` and `checks.docs-diagrams`
  # with it.
  #
  # An include is a REFERENCE, which is the point: the state stays on the aspect
  # and nothing has to look it up again. Free here because this aspect writes
  # only `inputs` plus top-level `perSystem` -- no nixos, darwin or homeManager
  # content -- so including it everywhere adds nothing to any host.
  den.default.includes = [ den.aspects.docs ];

  den.aspects.docs = {
    layers = [ "dev" ];
    inputs.den-diagram.url = "github:denful/den-diagram";
    # Without this, den-diagram pulls its own full nixpkgs (a separate
    # releases.nixos.org nixexprs.tar.xz) rather than reusing ours.
    inputs.den-diagram.inputs.nixpkgs.follows = "nixpkgs";
  };

  perSystem =
    {
      pkgs,
      config,
      ...
    }:
    let
      # Generate diagram for a single host. The graph filtering lives in
      # ../../lib/host-aspects.nix, shared with aspects/framework/partition-map.nix
      # so the "which aspects does this host really use" answer cannot drift
      # between the diagrams and the partition analysis.
      hostDiagram =
        hostName: host:
        let
          filtered = hostAspects.forHost hostName host (hostAspects.classesFor host);

          # ── THE CAPABILITY LAYER GOES IN THE SAME GRAPH ──
          # ./capability-graph.nix adds the resolution of every take on this
          # host -- the giver that won, the carrier it won over, and the
          # candidates that lost with the clause they lost on -- onto the
          # aspect nodes that are already here. `forHost` itself is untouched,
          # because its `.names` is the "which aspects does this host use"
          # answer and a capability node is not an aspect; see that file's
          # header.
          graph = capabilityGraph.overlay {
            inherit host;
            inherit (filtered) graph;
          };

          # The same IR, cut down to the resolution layer and the aspects
          # attached to it. A 135-node host graph is the right artefact for
          # "what is on this box" and the wrong one for "why did that take
          # resolve there".
          slice = capabilityGraph.sliceOf { inherit graph; };

          rc = diagram.renderContext {
            inherit pkgs;
            theme = diagram.themeFromBase16 {
              inherit pkgs;
              scheme = "catppuccin-mocha";
            };
          };
        in
        {
          mermaid = diagram.toMermaid graph;
          svg = rc.mmdSourceToSvg hostName (diagram.toMermaid graph);
          dot = diagram.toDot graph;
          aspects = lib.sort (a: b: a < b) filtered.names;

          # ── MERMAID FOR THE SLICE, AND ONLY MERMAID ──
          # den-diagram's DOT renderer emits an edge label for exactly one
          # style ("replaced"), so the carrier and its preference -- which are
          # the whole point of the slice -- are dropped there. Mermaid's
          # `provide` style carries an arbitrary label, so the resolution view
          # is rendered as mermaid and the DOT output above stays the plain
          # aspect-plus-capability topology.
          capabilityMermaid = diagram.toMermaid slice;
          capabilitySvg = rc.mmdSourceToSvg "${hostName}-capabilities" (diagram.toMermaid slice);
          resolutions = capabilityGraph.resolutionsFor { inherit host; };
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

      # The resolution slice, as both a committable mermaid source and an SVG.
      # ../../docs/capability-graphs/ is where `apps.capability-graphs` writes
      # the sources, because mermaid renders in a browser and in most markdown
      # viewers with no toolchain at all -- which matters more for something
      # read while debugging than DOT's better layout does.
      capabilityFiles = lib.mapAttrs (
        hostName: diag: pkgs.writeText "${hostName}-capabilities.mmd" diag.capabilityMermaid
      ) hostDiagrams;

      # ── THE FLEET'S ROUTES AND SEGMENTS (./route-graph.nix) ──
      routesMermaid = pkgs.writeText "routes.mmd" (diagram.toMermaid routeGraph.graph);
      routesSvg = pkgs.writeText "routes.svg" (
        builtins.readFile (
          (diagram.renderContext {
            inherit pkgs;
            theme = diagram.themeFromBase16 {
              inherit pkgs;
              scheme = "catppuccin-mocha";
            };
          }).mmdSourceToSvg
            "routes"
            (diagram.toMermaid routeGraph.graph)
        )
      );

      capabilitySvgFiles = lib.mapAttrs (
        hostName: diag: pkgs.writeText "${hostName}-capabilities.svg" (builtins.readFile diag.capabilitySvg)
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

      # The resolution view on its own, written where it can be opened: a
      # `.mmd` per host under docs/capability-graphs/, plus the rendered SVG.
      apps.capability-graphs = {
        type = "app";
        program = "${pkgs.writeShellScript "capability-graphs" ''
          mkdir -p docs/capability-graphs
          ${lib.concatStringsSep "\n" (
            lib.mapAttrsToList (name: f: "cp ${f} docs/capability-graphs/${name}.mmd") capabilityFiles
          )}
          ${lib.concatStringsSep "\n" (
            lib.mapAttrsToList (name: f: "cp ${f} docs/capability-graphs/${name}.svg") capabilitySvgFiles
          )}
          cp ${routesMermaid} docs/capability-graphs/routes.mmd
          cp ${routesSvg} docs/capability-graphs/routes.svg
          echo "Wrote docs/capability-graphs/"
        ''}";
      };

      # Also a check: this aspect lives in the `dev` partition (see
      # ../../partition-map.nix) and `packages` is taken from the base
      # evaluation, so `packages.docs-diagrams` is not reachable from the CLI.
      # `checks` is partitioned to `all`, so this alias is.
      checks.docs-diagrams = config.packages.docs-diagrams;
      # The source alone, buildable where mermaid-cli's browser is not.
      checks.route-graph-mermaid = routesMermaid;
      checks.route-graph = pkgs.runCommand "route-graph" { } ''
        mkdir -p $out
        cp ${routesMermaid} $out/routes.mmd
        cp ${routesSvg} $out/routes.svg
      '';

      packages.docs-diagrams = pkgs.runCommand "docs-diagrams" { } ''
        mkdir -p $out
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: svg: "cp ${svg} $out/${name}.svg") svgFiles)}
      '';
    };
}
