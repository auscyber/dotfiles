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
                difftastic
              ];
              text = ''
                file="$1"
                content=$(grep -v '^JJ:' "$file")

                if [ -z "$(echo "$content" | tr -d '[:space:]')" ]; then
                    stat=$(jj diff -r @ --git)
                    prompt="Diff:
                    $stat

                    ---
                    Write a commit message for the diff above. Output ONLY the message, nothing else — no preamble, no explanation, no markdown formatting (no asterisks, no bold, no headers).

                    Structure, exactly:
                    <type>(<scope>): <short imperative summary>
                    - <bullet describing one specific change>
                    - <bullet describing another specific change>

                    Rules:
                    - Exactly ONE type(scope) line, at the very top. Nothing else in the message has a type/scope prefix.
                    - 2-3 bullets below it, each a plain sentence, no bold, no nested sub-bullets.
                    - Types: feat, fix, refactor, chore, docs, test.
                    - Scope is a single short word, not a file path.
                    - Never repeat a bullet."

                      response=$(curl -sf http://localhost:${builtins.toString config.programs.llama-cpp.port}/v1/chat/completions \
                      -H "Content-Type: application/json" \
                      -d "$(jq -n --arg prompt "$prompt" '{
                        messages: [{role:"user", content: $prompt}],
                        temperature: 0.3,
                        max_tokens: 150,
                        presence_penalty: 0.6,
                        stop: ["Diff:", "---"]
                      }')")
                      raw=$(echo "$response" | jq -r '.choices[0].message.content // empty')
                      raw=$(echo "$raw" | sed -E 's/\*\*//g; /^```/d; s/^Here is.*://I')

                      # keep only the first type(scope) block, drop everything from the second header onward
                      summary=$(echo "$raw" | awk '
                        /^[a-zA-Z]+[[:space:]]*\(/ { n++; if (n > 1) exit }
                        { print }
                      ')

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
