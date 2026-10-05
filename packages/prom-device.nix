{
  # The `prom_device` Home Assistant custom component, written in-tree rather
  # than fetched: there is no upstream repository for it, so there is nothing
  # for nvfetcher to track and no revision to pin. The source lives beside this
  # file, under ./prom-device/custom_components/prom_device/ -- the layout
  # `buildHomeAssistantComponent` looks for (it copies ./custom_components/
  # wholesale when the top level has no manifest.json).
  #
  # `version` is the component's own, and has to stay in step with the
  # `"version"` in its manifest.json: Home Assistant reads the manifest, not
  # this.
  den.aspects.packages.prom-device = {
    overlays = { ... }: {
      prom-device = _final: prev: {
        prom-device = prev.buildHomeAssistantComponent {
          owner = "dendritic";
          domain = "prom_device";
          version = "0.2.0";

          # `builtins.path` with an explicit name, so the store path is
          # ...-prom-device-src rather than named after the checkout directory
          # and rebuilt whenever that is renamed.
          src = builtins.path {
            name = "prom-device-src";
            path = ./prom-device;
          };

          # No `dependencies`: the component imports voluptuous, aiohttp and
          # home-assistant's own helpers only, all of which are already in the
          # interpreter home-assistant runs under. `requirements` in the
          # manifest is correspondingly empty, which is what the
          # manifest-requirements check hook verifies against this.

          meta = {
            description = "Prometheus instant queries as one Home Assistant device per host";
          };
        };
      };
    };
  };
}
