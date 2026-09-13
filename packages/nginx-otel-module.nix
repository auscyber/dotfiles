{
  den.aspects.packages.nginx-otel-module = {
    overlays = { ... }: {
      nginx-otel-module = final: prev: {
        nginxModules = prev.nginxModules.extend (
          finalNginx: _: {
            otel = final.callPackage ./_nginx-otel-module.nix { };
          }
        );
      };
    };
  };
}
