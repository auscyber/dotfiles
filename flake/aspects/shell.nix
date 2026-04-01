{ den, ... }:
{
  # ── Shell aspect ─────────────────────────────────────────────────────────────
  # Fish + Zsh shells, git, direnv, nh, eza, fzf, zoxide, attic, starship, gpg.
  # Include in a user aspect to enable the full shell environment.
  den.aspects.shell = {
    homeManager =
      { config, lib, pkgs, ... }:
      let
        cfgShell = config.auscybernix.shell;
        cfgFish = config.auscybernix.shell.fish;
        cfgZsh = config.auscybernix.shell.zsh;
      in
      {
        options.auscybernix.shell = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable custom shell configuration.";
          };
          fish.enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable Fish shell configuration.";
          };
          zsh.enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable Zsh shell configuration.";
          };
        };

        config = lib.mkIf cfgShell.enable (
          lib.mkMerge [
            {
              age.secrets.attic_token = {
                rekeyFile = ../../secrets/attickey.age;
              };
              age.templates."attic-config" = {
                path = "${config.home.homeDirectory}/.config/attic/config.toml";
                dependencies = {
                  inherit (config.age.secrets) attic_token;
                };
                content =
                  { pkgs, placeholders, ... }:
                  ''
                    default-server = "central"
                    [servers.central]
                    endpoint = "https://cache.ivymect.in"
                    token = "${placeholders.attic_token}"
                  '';
              };
              home.packages = with pkgs; [ attic-client ];
              services.gpg-agent = {
                extraConfig = ''
                  allow-loopback-pinentry
                  default-cache-ttl 600
                  max-cache-ttl 7200
                  ttyname $GPG_TTY
                  enable-ssh-support
                  debug-level 2
                '';
                enableScDaemon = true;
              };
              home.file.".config/starship.toml".source = ../../.config/starship.toml;
              programs.gh = {
                enable = true;
                settings.git_protocol = "ssh";
              };
              programs.zoxide.enable = true;
              programs.fzf.enable = true;
              programs.nix-your-shell = {
                enable = true;
                nix-output-monitor.enable = true;
              };
              programs.nh.enable = true;
              programs.direnv = {
                enable = true;
                nix-direnv.enable = true;
              };
              programs.home-manager.enable = true;
              programs.gpg.enable = true;
              programs.git = {
                enable = true;
                settings.user = {
                  name = "Ivy Pierlot";
                  email = "ivyp@outlook.com.au";
                };
              };
              programs.eza = {
                enable = true;
                git = true;
                icons = "auto";
              };
            }

            # ── Fish shell ─────────────────────────────────────────────────
            (lib.mkIf cfgFish.enable {
              home.packages = with pkgs; [ nix-output-monitor bat starship ];
              home.sessionVariables.BROWSER = "zen";
              programs.fish = {
                enable = true;
                shellAliases = {
                  fzf = "fzf --reverse --height 40%";
                  vim = "nvim";
                  cat = "bat";
                  e = "vim";
                  ls = "eza --icons --git";
                  ll = "ls -la";
                  t = "tmux";
                  grep = "grep --color=auto";
                  hm = "home-manager ";
                };
                interactiveShellInit =
                  # fish
                  ''
                    function bind_bang
                        switch (commandline -t)[-1]
                            case "!"
                                commandline -t -- $history[1]
                                commandline -f repaint
                            case "*"
                                commandline -i !
                        end
                    end

                    function bind_dollar
                        switch (commandline -t)[-1]
                            case "!"
                                commandline -f backward-delete-char history-token-search-backward
                            case "*"
                                commandline -i '$'
                        end
                    end

                    function fish_user_key_bindings
                        bind ! bind_bang
                        bind '$' bind_dollar
                    end
                    set fish_greeting
                    fetch -s
                    starship init fish | source
                  '';
              };
            })

            # ── Zsh shell ──────────────────────────────────────────────────
            (lib.mkIf cfgZsh.enable {
              programs.eza.enable = true;
              home.packages = with pkgs; [ bat ];
              home.file."Music/Phoebe/lyricslist".source = ../../phoebelyrics/lyricslist;
              programs.zsh = {
                enable = true;
                enableCompletion = true;
                syntaxHighlighting.enable = true;
                history = {
                  size = 10000;
                  path = "${config.xdg.dataHome}/zsh/history";
                };
                sessionVariables = {
                  MANPATH = "\${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man";
                  EDITOR = "nvim";
                  editor = "$EDITOR";
                  BROWSER = "firefox";
                  GTK2_RC_FILES = "$HOME/.gtkrc-2.0";
                  _JAVA_AWT_WM_NONREPARENTING = 1;
                  WLR_NO_HARDWARE_CURSORS = 1;
                };
                plugins = builtins.map
                  (package: { name = package.pname; inherit (package) src; })
                  (with pkgs; [
                    zsh-autosuggestions
                    nix-zsh-completions
                    zsh-completions
                    zsh-nix-shell
                    zsh-syntax-highlighting
                  ]);
                shellAliases = {
                  ghc = "stack exec -- ghc";
                  fzf = "fzf --reverse --height 40%";
                  vim = "nvim";
                  cat = "bat";
                  e = "vim";
                  ls = "exa --icons --git";
                  ll = "ls -la";
                  t = "tmux";
                  grep = "grep --color=auto";
                  windows = "sudo grub-reboot 2 && sudo reboot";
                  hm = "home-manager --flake $NIXFLAKE#$FLAKENAME";
                };
                initExtra = ''
                  export PATH=$PATH:~/.cabal/bin:~/go/bin:~/.emacs.d/bin:~/.local/bin:~/.dotnet/tools:/usr/sbin:/snap/bin:$NPM_PACKAGES/bin:~/.luarocks/bin:/usr/local/go/bin:$DENO_INSTALL/bin:/opt/jdk8u292-b10:$IDRIS_PREFIX/bin
                  fetch -s
                  eval "$(starship init zsh)"
                '';
              };
            })
          ]
        );
      };
  };
}
