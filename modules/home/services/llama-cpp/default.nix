# home-modules/llama-cpp.nix
#
# Home Manager module: runs llama.cpp as a *user* systemd service.
# Why: start/stop without sudo, models in ~/models, dev-friendly.
#
# Future refactor note:
# - Extract shared option/arg building into lib/llama-cpp-common.nix
# - Then have a NixOS wrapper for always-on/server use-cases.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.llama-cpp;

  # Build argument list, then escape for systemd ExecStart.
  baseArgs = [
    "--host"
    cfg.host
    "--port"
    (toString cfg.port)
  ];

  modelArgs = lib.optionals (cfg.model != null) [
    "-m"
    cfg.model
  ];

  # Router mode: llama-server supports --models-dir and --models-preset
  presetFile =
    if cfg.modelsPreset != null then
      pkgs.writeText "llama-models.ini" (lib.generators.toINI { } cfg.modelsPreset)
    else
      null;

  routerArgs =
    (lib.optionals (cfg.modelsDir != null) [
      "--models-dir"
      cfg.modelsDir
    ])
    ++ (lib.optionals (cfg.modelsPreset != null) [
      "--models-preset"
      presetFile
    ]);

  args = baseArgs ++ modelArgs ++ routerArgs ++ cfg.extraFlags;

  execStart = "${cfg.package}/bin/llama-server ${lib.escapeShellArgs args}";
in
{
  options.services.llama-cpp = {
    enable = lib.mkEnableOption "llama.cpp llama-server (user service)";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llama-cpp;
      description = "llama.cpp package to use (override with CUDA-enabled build).";
    };

    # Either set model OR use router mode (modelsDir/modelsPreset)
    model = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Path to GGUF model. If null, runs in router mode.";
      example = "${config.home.homeDirectory}/models/qwen2.5-3b-instruct-q4_k_m.gguf";
    };

    modelsDir = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Router mode: directory containing models.";
      example = "${config.home.homeDirectory}/models";
    };

    modelsPreset = lib.mkOption {
      type = lib.types.nullOr (lib.types.attrsOf (lib.types.attrsOf lib.types.anything));
      default = null;
      description = "Router mode: attrset converted to INI passed as --models-preset.";
      example = lib.literalExpression ''
        {
          "qwen3b" = {
            model = "${config.home.homeDirectory}/models/qwen2.5-3b-instruct-q4_k_m.gguf";
            jinja = "on";
            c = "4096";
            n-gpu-layers = "auto";
          };
        }
      '';
    };

    extraFlags = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Extra flags passed to llama-server.";
      example = [
        "--jinja"
        "-ngl"
        "auto"
        "-c"
        "4096"
      ];
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Listen address.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = "Listen port.";
    };

    autoStart = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Autostart on user login (default false).";
    };

    restartPolicy = lib.mkOption {
      type = lib.types.enum [
        "on-failure"
        "no"
      ];
      default = "on-failure";
      description = "systemd restart policy.";
    };

    cacheDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.xdg.cacheHome}/llama-cpp";
      description = "Cache directory for llama.cpp.";
    };
  };

  config = lib.mkIf cfg.enable {
    # Ensure dirs exist (no /var involved)
    home.file."models/.keep".text = ""; # creates ~/models
    home.file.".cache/llama-cpp/.keep".text = ""; # creates cache dir

    launchd.agents.llama_cpp = {
      enable = true;
      config = {
        Program = "${cfg.package}/bin/llama-server";
        ProcessType = "interactive";
        ProgramArguments = args;
        Environment = {
          LLAMA_CACHE = cfg.cacheDir;
          XDG_CACHE_HOME = config.xdg.cacheHome;
        };

        KeepAlive = true;
      };
    };

    systemd.user.services.llama-cpp = {
      Unit = {
        Description = "llama.cpp llama-server (OpenAI-compatible, user)";
        After = [ "network-online.target" ];
      };

      Service = {
        Type = "simple";
        ExecStart = execStart;

        Environment = [
          "LLAMA_CACHE=${cfg.cacheDir}"
          "XDG_CACHE_HOME=${config.xdg.cacheHome}"
        ];

        Restart = cfg.restartPolicy;
        RestartSec = 10;
      };

      Install = {
        # Manual by default; enable later by setting autoStart=true.
        WantedBy = lib.optionals cfg.autoStart [ "default.target" ];
      };
    };
  };
}
