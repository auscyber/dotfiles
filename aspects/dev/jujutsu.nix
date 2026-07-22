{ den, ... }: {
  # jujutsu starship stuff

  den.aspects.jujutsu = {
    includes = [ den.aspects.difftastic ];
    homeManager = {
      programs.jujutsu = {
        enable = true;

        settings = {
          "revset-aliases" = {
            "closest_bookmark(to)" = "heads(::to & bookmarks())";
          };

          "template-aliases" = {
            prompt = ''
              truncate_end(
                15,
                concat(
                  if(conflict, "="),
                  if(self.diff().files().filter(|f| f.status() == "removed"), "✘"),
                  if(self.diff().files().filter(|f| f.status() == "renamed"), "»"),
                  if(self.diff().files().filter(|f| f.status() == "modified"), "!"),
                  if(self.diff().files().filter(|f| f.status() == "added"), "?"),
                  if(!description, " "),
                  if(divergent, "⇕"),
                  if(immutable, " "),
                  if(description, concat(" ", description))
                ),
                ".."
              )
            '';
          };

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
