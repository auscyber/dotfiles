{ den, ... }: {
  den.aspects.cotabby = {
    # packages.cotabby only builds/overlays the app; llama-cpp runs the shared
    # llama-server this aspect points Cotabby's "Local Endpoint" engine at
    # (and owns the `programs.llama-cpp.port` option read below).
    includes = [
      den.aspects.packages.cotabby
      den.aspects.llama-cpp
    ];

    # Cotabby has no config file -- every Settings pane control is an
    # NSUserDefaults key under bundle id com.jacobfu.tabby (confirmed via
    # `defaults read com.jacobfu.tabby` against an already-configured
    # install). `targets.darwin.defaults` writes them the same way the GUI
    # would, from within home-manager (so it can read `programs.llama-cpp.port`
    # directly, rather than reaching across into nix-darwin's config tree).
    #
    # cotabbySelectedEngine = "openAICompatible" points Cotabby's own
    # completion pipeline at the llama-cpp aspect's llama-server instead of
    # Cotabby's bundled "Open Source" engine, which otherwise downloads and
    # keeps resident a second, separate GGUF (it already had one under
    # ~/Library/Application Support/Cotabby/LlamaRuntime). One server, one
    # resident model, shared between Cotabby and anything else (e.g. openclaw)
    # configured to hit the same OpenAI-compatible endpoint.
    #
    # Cotabby caches these in memory at launch, so a running instance needs a
    # quit/relaunch (menu bar icon -> Quit) after `home-manager switch` to
    # pick up a changed port or engine.
    homeManager =
      {
        config,
        pkgs,
        ...
      }:
      {
        home.packages = [ pkgs.cotabby ];

        targets.darwin.defaults."com.jacobfu.tabby" = {
          cotabbySelectedEngine = "openAICompatible";
          cotabbyOpenAICompatibleAPIMode = "chatCompletions";
          cotabbyOpenAICompatibleBaseURL = "http://127.0.0.1:${toString config.programs.llama-cpp.port}/v1";
          cotabbyOpenAICompatibleModelName = "qwen3-8b";
        };
      };
  };
}
