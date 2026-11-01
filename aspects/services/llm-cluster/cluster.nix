{
  den,
  lib,
  ...
}:
# An `llm-cluster` is an entity kind, like a household. A host joins one by
# naming it:
#
#   den.hosts.x86_64-linux.auspc.llm-cluster = den.llm-clusters.home;
#
# Every member gets `llm-node`; the member the cluster names as `head` gets
# `llm-head` instead. Both are `reach = "llm-cluster"`, so a worker only ever
# answers the head of its own cluster.
#
# `head` points back UP at a host, against the direction rule in
# ../../base/household.nix. Only `head.name` is ever read through it.
let
  inherit (lib) mkOption types;

  modelType = types.submodule {
    options = {
      hf = mkOption {
        type = types.str;
        example = "Qwen/Qwen2.5-1.5B-Instruct-GGUF:Q4_K_M";
        description = "HuggingFace GGUF reference (`hf-repo`).";
      };
      contextSize = mkOption {
        type = types.ints.positive;
        default = 16384;
      };
      loadOnStartup = mkOption {
        type = types.bool;
        default = false;
      };
      extraArgs = mkOption {
        type = types.attrsOf (
          types.oneOf [
            types.str
            types.int
            types.bool
          ]
        );
        default = { };
        description = "Further llama-server preset keys for this model, flag names without dashes.";
      };
    };
  };

  clusterType = den.lib.schema.mkInstanceType {
    strict = true;
  } den.schema.llm-cluster;

  isHead = host: c: c != null && c.head.name == (host.name or null);
in
{
  options.den.llm-clusters = mkOption {
    type = types.attrsOf clusterType;
    default = { };
    description = "llama.cpp RPC clusters, each a head and the hosts that name it.";
  };

  config = {
    den.schema.llm-cluster = {
      inherits = [ den.schema.conf ];

      options = {
        head = mkOption {
          type = types.raw;
          example = lib.literalExpression "den.hosts.x86_64-linux.secondpc";
          description = "The member that runs llama-server over every other member's rpc-server.";
        };
        models = mkOption {
          type = types.attrsOf modelType;
          description = "Served models, keyed by the id advertised at /v1/models.";
        };
        maxLoaded = mkOption {
          type = types.ints.unsigned;
          default = 1;
          description = "Models resident at once (`--models-max`). Every one is split across the workers.";
        };
      };
    };

    den.schema.host.options.llm-cluster = mkOption {
      type = types.nullOr types.raw;
      default = null;
      example = lib.literalExpression "den.llm-clusters.home";
      description = "The llm-cluster this host is a member of. Also its `reach = \"llm-cluster\"` scope.";
    };

    den.policies.llm-cluster-roles =
      { host, ... }:
      let
        c = host.llm-cluster;
      in
      lib.optionals (c != null) [
        (den.lib.policy.include (if isHead host c then den.aspects.llm-head else den.aspects.llm-node))
      ];

    den.schema.host.includes = [ den.policies.llm-cluster-roles ];
  };
}
