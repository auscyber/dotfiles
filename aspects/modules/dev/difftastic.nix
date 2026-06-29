{ den, ... }:
{
  den.aspects.difftastic = { user, ... }: {
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
