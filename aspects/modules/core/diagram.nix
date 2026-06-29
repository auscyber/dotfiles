{ inputs, den, ... }:
let
  diagram = inputs.den-diagram.lib;

in
{
  ff.den-diagram.url = "github:denful/den-diagram";
  flake.diagram = diagram;
  perSystem =
    { inputs', pkgs, ... }:

    let
      host = den.hosts.aarch64-darwin.Ivys-MacBook-Pro;
      captured = den.lib.capture.captureWithPathsWith {
        classes = [
          "darwin"
          "homeManager"
        ];
        root = den.lib.resolveEntity "host" { inherit host; };
        ctx = { inherit host; };
      };

      # 2. Graph — builds format-agnostic IR from trace entries
      g = diagram.context {
        entries = captured.entries;
        ctxTrace = captured.ctxTrace;
        name = host.name;
      };
      rc = diagram.renderContext {
        inherit pkgs;
        theme = diagram.themeFromBase16 {
          inherit pkgs;
          scheme = "catppuccin-mocha";
        };
      };
      svg = rc.mmdSourceToSvg "my-diagram" (diagram.toMermaid g);
    in
    {
      packages.den-diagram = pkgs.writeText "mermaid" (diagram.toDot g);
    };
}
