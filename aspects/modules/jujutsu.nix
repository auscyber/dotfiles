{ den, ... }:
{
  den.aspects.jujutsu = {
    homeManager = {
      programs.difftastic = {
        enable = true;
        git.enable = true;
        git.diffToolMode = true;
        jujutsu.enable = true;
      };
      programs.jujutsu = {

        enable = true;

        settings = {
          user = {
            name = "Ivy Pierlot";
            email = "ivyp@outlook.com.au";
          };
        };
      };
    };
  };
}
