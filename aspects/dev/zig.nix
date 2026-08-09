{
  den,
  lib,
  ...
}:
{

  den.aspects.zig = {
    includes = [ den.aspects.lspmux ];
    lsp-servers = { pkgs, ... }: {
      zls = {
        package = pkgs.zls;
        exe = "zls";
        zed = "zls";
        extensionToLanguage.".zig" = "zig";
      };
    };

    homeManager = { pkgs, ... }: {
      home.packages = with pkgs; [ zig ];
    };

  };

}
