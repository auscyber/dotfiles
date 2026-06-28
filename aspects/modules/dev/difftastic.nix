{ den, ... }:
{
  den.aspects.difftastic = {
    homeManager.programs.difftastic = {
      enable = true;
      git.enable = true;
      git.mode = "both";
      jujutsu.enable = true;
    };
    nvim.plugins = {
      difftastic.enable = true;

      jujutsu.settings.diff_preset = "difftastic";
    };
  };

}
