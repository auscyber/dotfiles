{ den, ... }:
{
  # Members name it: `llm-cluster = den.llm-clusters.home;` on the host.
  den.llm-clusters.home = {
    head = den.hosts.x86_64-linux.secondpc;
    models."qwen2.5-1.5b" = {
      hf = "Qwen/Qwen2.5-1.5B-Instruct-GGUF:Q4_K_M";
      loadOnStartup = true;
    };
  };
}
