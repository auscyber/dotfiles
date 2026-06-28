{ den, ... }:
{
  den.aspects.llama-cpp = {
    homeManager =
      { config, pkgs, lib, ... }:
      let
        host = "127.0.0.1";
        port = 8080;
        cacheDir = "${config.xdg.cacheHome}/llama-cpp";
        args = [
          "--host"
          host
          "--port"
          (toString port)
        ];
      in
      {
        home.file."models/.keep".text = "";
        home.file.".cache/llama-cpp/.keep".text = "";

        launchd.agents.llama_cpp = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
          enable = true;
          config = {
            Program = "${pkgs.llama-cpp}/bin/llama-server";
            ProcessType = "interactive";
            ProgramArguments = args;
            EnvironmentVariables = {
              LLAMA_CACHE = cacheDir;
              XDG_CACHE_HOME = config.xdg.cacheHome;
            };
            KeepAlive = true;
          };
        };

        systemd.user.services.llama-cpp = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
          Unit = {
            Description = "llama.cpp llama-server (OpenAI-compatible, user)";
            After = [ "network-online.target" ];
          };
          Service = {
            Type = "simple";
            ExecStart = "${pkgs.llama-cpp}/bin/llama-server ${lib.escapeShellArgs args}";
            Environment = [
              "LLAMA_CACHE=${cacheDir}"
              "XDG_CACHE_HOME=${config.xdg.cacheHome}"
            ];
            Restart = "on-failure";
            RestartSec = 10;
          };
        };
      };
  };
}
