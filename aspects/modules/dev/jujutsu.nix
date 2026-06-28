{ den, ... }:
{
  den.aspects.jujutsu = {
    includes = [ den.aspects.difftastic ];
    homeManager = {
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
    nvim.plugins.jujutsu = {
      enable = true;
      settings = {
        keymap = {
          "<leader>jd" = {
            cmd = "show_help";
          };
        };
      };
    };

  };
}
