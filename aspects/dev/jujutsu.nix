{ den, ... }: {
  # jujutsu starship stuff

  den.aspects.jujutsu = {
    includes = [
      den.aspects.difftastic

      #      (den.lib.whenAspect den.aspects.claude ({
      #        homeManager =
      #          {
      #            config,
      #            pkgs,
      #            lib,
      #            ...
      #          }:
      #
      #          let
      #            jjAiEditor = pkgs.writeShellApplication {
      #              name = "jj-ai-editor";
      #              runtimeInputs = with pkgs; [
      #                jujutsu
      #                config.programs.claude-code.package
      #              ];
      #              text = ''
      #                file="$1"
      #                content=$(grep -v '^JJ:' "$file")
      #                if [ -z "$(echo "$content" | tr -d '[:space:]')" ]; then
      #                	diff=$(jj diff --git -r @)
      #                	summary=$(echo "Write a concise, imperative-mood commit message for this diff. Output only the message.
      #
      #                $diff" | claude --model claude-haiku-4-5-20251001)
      #                	{
      #                		echo "$summary"
      #                		echo
      #                		cat "$file"
      #                	} >"$file.tmp"
      #                	mv "$file.tmp" "$file"
      #                fi
      #                exec "''${VISUAL:-''${EDITOR:-nvim}}" "$file"
      #              '';
      #            };
      #          in
      #          {
      #            home.packages = [ jjAiEditor ];
      #
      #            programs.jujutsu = {
      #              enable = true;
      #              settings = {
      #                ui.editor = "${jjAiEditor}/bin/jj-ai-editor";
      #              };
      #            };
      #          };
      #      }))
    ];

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
