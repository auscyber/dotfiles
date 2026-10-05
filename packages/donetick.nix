{
  # Not in nixpkgs. Tracked here so `nix run .#update-sources` moves the tag and
  # the hash together, like every other source in this repo.
  nvfetcher.sources.donetick = {
    src.github = "donetick/donetick";
    fetch.github = "donetick/donetick";
  };

  den.aspects.packages.donetick = {
    overlays = { sources, ... }: {
      donetick = _self: super: {
        donetick = super.buildGoModule {
          inherit (sources.donetick) pname version src;

          vendorHash = "sha256-abI5330bLKF+eBqgPcIadhOp5xDfqL+LmCb4oqn0qgw=";

          # Upstream's Dockerfile builds exactly this way: pure Go, no cgo, and
          # NO frontend build step -- the web assets under ./assets are checked
          # in and embedded, so there is no npm stage to reproduce here.
          env.CGO_ENABLED = 0;

          # `go build .` at the root, matching the Dockerfile. Without this
          # buildGoModule walks every package, and ./frontend has no Go in it.
          subPackages = [ "." ];

          ldflags = [
            "-s"
            "-w"
            "-X donetick.com/core/config.Version=${sources.donetick.version}"
          ];

          # The repo has no vcs info in the tarball, and the Dockerfile passes
          # the same flag for the same reason.
          env.GOFLAGS = "-buildvcs=false";

          # Upstream reads its config from a file named by $DT_ENV inside a
          # `config/` directory relative to the working directory. The unit in
          # ../aspects/services/donetick.nix sets WorkingDirectory to its state
          # dir and writes the config there, so ship the stock set as a
          # reference rather than as the thing that runs.
          postInstall = ''
            mkdir -p "$out/share/donetick"
            cp -r config "$out/share/donetick/config-defaults"
          '';

          meta = {
            description = "Self-hosted task and chore manager";
            homepage = "https://donetick.com";
            mainProgram = "core";
          };
        };
      };
    };
  };
}
