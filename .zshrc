
# }}} End configuration added by Zim install
autoload -U compinit promptinit
compinit
#promptinit; #prompt gentoo
export _JAVA_AWT_WM_NONREPARENTING=1

source ~/antigen.zsh
antigen use oh-my-zsh
#antigen bundle git@github.com:spwhitt/nix-zsh-completions.git
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen apply

export PATH=$PATH:~/.local/bin:~/.cargo/bin:~/.cabal/bin:~/go/bin:~/.emacs.d/bin
# Aliases 

function set_win_title(){
    echo -ne "\033]0; Alacritty: $(basename $PWD) \007"
}
precmd_functions+=(set_win_title)

function ghc_env(){
    nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; $1)"
}

if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

alias fzf="fzf --reverse --height 40%"
alias vim="nvim"
alias e="vim"
#alias sudo="doas"
alias t="tmux"
alias hm=home-manager
alias grep="grep --color=auto"
alias windows="sudo grub-reboot 2 --boot-directory=/efi && sudo reboot"
alias emacs="emacsclient -t "
eval "$(starship init zsh)"
export EDITOR=vim
export VISUAL=gvim
export BROWSER=chromium
