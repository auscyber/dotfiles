{
  den,
  lib,
  ...
}:
{
  den.aspects.nixvim = {
    # copilot.lua's default `server.type = "nodejs"` runs the ~220MB node server
    # bundled in the plugin, once per nvim. Its `binary` type instead execs a
    # copilot-language-server off disk -- and when that path is the lspmux shim,
    # every nvim on the machine shares one server process (~2MB of `lspmux client`
    # each) instead of carrying its own.
    #
    # `custom_server_filepath` is not optional here: without it the `binary` type
    # curl/wget-downloads and unzips a release build into the plugin directory at
    # runtime.
    includes = [ den.aspects.lspmux ];

    nvim = { pkgs, ... }: {
      plugins.copilot-lua.settings.server = {
        type = "binary";
        custom_server_filepath = lib.getExe pkgs.lspmuxed.copilot-language-server;
      };
    };
  };
}
