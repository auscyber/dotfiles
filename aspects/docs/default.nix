{
  inputs,
  lib,
  den,
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
      # Generate diagram for a single host. The graph filtering lives in
      # ../../lib/host-aspects.nix, shared with aspects/framework/partition-map.nix
      # so the "which aspects does this host really use" answer cannot drift
      # between the diagrams and the partition analysis.
      hostDiagram =
        hostName: host:
        let
          filtered = hostAspects.forHost hostName host (hostAspects.classesFor host);
          inherit (filtered) graph;

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

      # Also a check: this aspect lives in the `dev` partition (see
      # ../../partition-map.nix) and `packages` is taken from the base
      # evaluation, so `packages.docs-diagrams` is not reachable from the CLI.
      # `checks` is partitioned to `all`, so this alias is.
      checks.docs-diagrams = config.packages.docs-diagrams;

      packages.docs-diagrams = pkgs.runCommand "docs-diagrams" { } ''
        mkdir -p $out
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: svg: "cp ${svg} $out/${name}.svg") svgFiles)}
      '';
    };
}
