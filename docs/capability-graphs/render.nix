# Render the capability-resolution view of one host to mermaid, or to a text
# report of the same data.
#
# ── WHY THIS IS A STANDALONE EXPRESSION AND NOT AN APP ──
#
# `apps.capability-graphs` in ../../aspects/docs/default.nix is the real
# producer and reads `inputs.den-diagram`. That input is DECLARED (on
# `den.aspects.docs`) and placed in no lock: `aspects/framework/layers.nix`
# gathers `inputs`-class declarations from a FLEET CAPTURE, and nothing in the
# tree includes `den.aspects.docs`, so the aspect is never traced and its input
# never reaches `partitions/dev/flake.nix`. Confirmed against the generator's
# own output -- `flake.layerFlakeText.dev` does not mention it, and
# `flake.layerPlan` has it in no layer, root included.
#
# So the whole `aspects/docs/` tree is currently un-evaluable, and this file is
# how the graphs get produced in the meantime: it pulls den-diagram as a flake
# of its own and reads everything else out of `.#debug.config`, which is where
# `_module.args.capabilityGraph` lives. Retire it once the input is locked.
#
# PINNED, not floating: an unpinned `github:denful/den-diagram` re-resolves on
# every run, so the committed graphs would change for reasons that have nothing
# to do with this flake.
{
  root,
  host,
  view ? "slice",
  # ── ONE TAKE, OR ALL OF THEM ──
  # secondpc declares about fifty takes and the whole-host slice is the right
  # artefact for "what does this box resolve". It is the wrong one for "why did
  # THAT take land there", so a focus of `"vban-turntable.udp"` (or just
  # `"vban-turntable"`) keeps only the matching resolutions and the aspects they
  # touch.
  focus ? null,
  reachOverride ? { },
  denDiagram ? "github:denful/den-diagram/268bdbd9678ca3afab4ccbf40c76cf90b6205e2b",
}:
let
  flake = builtins.getFlake "git+file://${root}";
  cfg = flake.debug.config;

  lib = flake.inputs.nixpkgs.lib.extend (import "${root}/lib");
  diagram = import "${builtins.getFlake denDiagram}/nix" { inherit lib; };

  inherit (cfg) den;
  cg = cfg._module.args.capabilityGraph;

  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;
  h = hostsOf.${host};

  hostAspects = import "${root}/lib/host-aspects.nix" { inherit lib den diagram; };
  filtered = hostAspects.forHost host h (hostAspects.classesFor h);

  all = cg.resolutionsFor {
    host = h;
    inherit reachOverride;
  };

  resolutions =
    if focus == null then
      all
    else
      lib.filter (r: r.taker == focus || "${r.taker}.${r.cap}" == focus) all;

  full = cg.overlay {
    host = h;
    inherit (filtered) graph;
    inherit resolutions;
  };

  slice = cg.sliceOf { graph = full; };

  # ── THE TEXT FORM OF THE SAME ANSWER ──
  # `explain` is ../../aspects/base/capability.nix's own `explainTake`, the
  # table it prints into a throw. Rendered beside the graph so a reader can
  # check the picture against the string the resolver itself produces.
  report =
    let
      one =
        r:
        let
          w = r.who.winner;
          c = r.how.winner;
        in
        lib.concatStringsSep "\n" (
          [
            "### ${r.taker} takes `${r.cap}`"
            ""
            "- clauses: ${if r.clauses == "" then "(none)" else "`${r.clauses}`"}"
            "- routed: ${if r.routed then "yes" else "no"}"
            (
              if w == null then
                "- **unresolved**"
              else
                "- giver: `${w.giver}` on `${w.on}` (reach = ${w.reach}, ${
                  if w.sameHost then "same host" else "cross-host"
                })"
            )
            (
              if c == null then
                "- carrier: none (${if r.routed then "no carrier qualified" else "not a routed capability"})"
              else
                "- carrier: `${c.carrier}` at preference ${toString c.preference}${
                  lib.optionalString (c.address != null) ", address ${c.address}"
                }"
            )
          ]
          ++ lib.optionals (r.explain.who != "") [
            ""
            "WHO (which giver):"
            ""
            "```"
            r.explain.who
            "```"
          ]
          ++ lib.optionals (r.explain.how != "") [
            ""
            "HOW (which carrier):"
            ""
            "```"
            r.explain.how
            "```"
          ]
          ++ lib.optionals (r.who.unreachable != [ ]) [
            ""
            "OUT OF REACH: ${
              lib.concatMapStringsSep ", " (x: "${x.giver} on ${x.on} (reach = ${x.reach})") r.who.unreachable
            }"
          ]
          ++ lib.optionals (r.who.pretended != [ ]) [
            ""
            "OUT OF REACH (what-if): ${
              lib.concatMapStringsSep ", " (x: "${x.giver} (reach = ${x.reach})") r.who.pretended
            }"
          ]
          ++ [ "" ]
        );
    in
    lib.concatStringsSep "\n" (
      [
        "# Capability resolution on `${host}`${lib.optionalString (focus != null) " -- `${focus}`"}"
        ""
        "${toString (lib.length resolutions)} take${
          lib.optionalString (lib.length resolutions != 1) "s"
        }, from the aspects this host has."
        ""
      ]
      ++ map one resolutions
    );
in
if view == "slice" then
  diagram.toMermaid slice
else if view == "full" then
  diagram.toMermaid full
else if view == "dot" then
  diagram.toDot slice
else if view == "report" then
  report
else if view == "json" then
  builtins.toJSON resolutions
else
  throw "render.nix: unknown view `${view}` (slice | full | dot | report | json)"
