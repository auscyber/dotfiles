{
  nvfetcher.sources.eagle-nvim = {
    src.git = "https://github.com/soulis-1256/eagle.nvim.git";
    src.git_branch = "main";
    fetch.github = "soulis-1256/eagle.nvim";
  };

  den.aspects.packages.eagle-nvim = {
    overlays = { sources, ... }: {
      eagle-nvim = final: prev: {
        vimPlugins = prev.vimPlugins.extend (
          self: super: {
            eagle-nvim = final.vimUtils.buildVimPlugin {
              name = "eagle-nvim";
              inherit (sources.eagle-nvim) src version;
            };
          }
        );
      };
    };
  };
}
