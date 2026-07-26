{ den, ... }: {
  # jujutsu starship stuff

  den.aspects.jujutsu = {
    includes = [
      den.aspects.difftastic

      (den.lib.whenAspect den.aspects.llama-cpp ({
        homeManager =
          {
            config,
            pkgs,
            lib,
            ...
          }:
          let
            jjAiEditor = pkgs.writeShellApplication {
              name = "jj-ai-editor";
              runtimeInputs = with pkgs; [
                jujutsu
                curl
                jq
              ];
              text = ''
                file="$1"
                content=$(grep -v '^JJ:' "$file")

                if [ -z "$(echo "$content" | tr -d '[:space:]')" ]; then
                    stat=$(jj diff -r @ --git)

                    prompt="Diff:
                    $stat

                    ---
                    Based on the diff above, write a conventional commit message with this structure:
                    1. A summary line: <type>(<scope>): <short imperative summary>
                    2. Below it, 2-3 distinct bullet points, each on its own line starting with a dash, describing separate specific changes. Do not repeat the same point twice.

                    Types: feat, fix, refactor, chore, docs, test.
                    Scope must be a single short word, not a file path.
                    Output only the message. Do not repeat or quote the diff."
                    response=$(curl -sf http://localhost:${builtins.toString config.programs.llama-cpp.port}/v1/chat/completions \
                      -H "Content-Type: application/json" \
                      -d "$(jq -n --arg prompt "$prompt" '{
                        messages: [{role:"user", content: $prompt}],
                        temperature: 0.3,
                        max_tokens: 150,
                        presence_penalty: 0.4,
                        stop: ["Diff:", "---"]
                      }')")
                    raw=$(echo "$response" | jq -r '.choices[0].message.content // empty')
                    summary=$(echo "$raw" | grep -E '^[a-z]+[[:space:]]*\(' -A 20 | sed '/^$/N;/^\n$/D')
                    if [ -z "$summary" ]; then
                      summary="# AI summary failed — raw response: $raw"
                    fi
                	{
                		echo "$summary"
                		echo
                		cat "$file"
                	} >"$file.tmp"
                	mv "$file.tmp" "$file"
                fi

                exec "''${VISUAL:-''${EDITOR:-nvim}}" "$file"
              '';
            };
          in
          {
            home.packages = [ jjAiEditor ];

            programs.jujutsu = {
              settings = {
                ui.editor = "${jjAiEditor}/bin/jj-ai-editor";
              };
            };
          };

      }))
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
