#!/usr/bin/zsh
#source ~/.bashrc
unset __HM_SESS_VARS_SOURCED
source ~/.nix-profile/etc/profile.d/hm-session-vars.sh
source ~/.nix-profile/etc/profile.d/nix.sh

#zmodload zsh/zprof
#source /etc/profile
NPM_PACKAGES="${HOME}/.npm-packages"
export DENO_INSTALL="/home/auscyber/.deno"

#export IDRIS2_CG=racket
#IDRIS_PREFIX=$HOME/.idris2
export PATH=$PATH:~/.cabal/bin:~/go/bin:~/.emacs.d/bin:/home/auscyber/.local/bin:~/.dotnet/tools:/usr/sbin:/snap/bin:$NPM_PACKAGES/bin:~/.luarocks/bin:/usr/local/go/bin:$DENO_INSTALL/bin:/opt/jdk8u292-b10:$IDRIS_PREFIX/bin

test -r /home/auscyber/.opam/opam-init/init.zsh && . /home/auscyber/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$IDRIS_PREFIX/lib
export RUSTC_WRAPPER=`which sccache`
fpath=(~/.zsh $fpath)
#promptinit; #prompt gentoo
export _JAVA_AWT_WM_NONREPARENTING=1
ZSH_CUSTOM=$HOME/.zsh-plugins
#ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#ffa0a0,bg=NONE,bold,underline"

## customisation
zstyle ':completion:*' file-sort size

## HISTORY
export HISTSIZE=1000000000
export HISTFILE=~/.zsh_history
setopt SHARE_HISTORY
setopt appendhistory

if ! command -v starship 2>&- 1>&-
then
    sh -c "$(curl -fsSL https://starship.rs/install.sh)"
fi

zinit_install () {
    if [[ ! -e ~/.zinit  ]] then
        mkdir ~/.zinit
        git clone https://github.com/zdharma/zinit.git ~/.zinit/bin
    fi
    source ~/.zinit/bin/zinit.zsh

    zinit load zsh-users/zsh-autosuggestions
    zinit load spwhitt/nix-zsh-completions
    zinit load zsh-users/zsh-completions
    zinit load zsh-users/zsh-syntax-highlighting
    zinit load chisui/zsh-nix-shell
}

antigen_install () {
    if [[ ! -e ~/antigen.zsh ]]; then
        curl -L git.io/antigen > ~/antigen.zsh
    fi
    source ~/antigen.zsh
#    antigen use oh-my-zsh
    antigen bundle zsh-users/zsh-autosuggestions
    antigen bundle spwhitt/nix-zsh-completions
    antigen bundle zsh-users/zsh-completions
    antigen bundle chisui/zsh-nix-shell
    antigen bundle zsh-users/zsh-syntax-highlighting
    antigen apply
}
antigen_install
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor regexp)
typeset -A ZSH_HIGHLIGHT_STYLES
#ZSH_HIGHLIGHT_STYLES[alias]=fg=#ffa0a0,bold
#ZSH_HIGHLIGHT_STYLES[builtin]=fg=#ffa0a0,bold
#ZSH_HIGHLIGHT_STYLES[function]=fg=#ffa0a0,bold
#ZSH_HIGHLIGHT_STYLES[command]=fg=#ffa0a0,bold

#zinit_install

autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit

eval "$(idris2 --bash-completion-script idris2)"
eval "$(stack --bash-completion-script stack)"
eval "$(dots completion --shell bash)"


function ghc_env(){
    nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; $1)"
}


if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

alias ghc="stack exec -- ghc"
#alias ghci="stack exec -- ghci"
alias fzf="fzf --reverse --height 40%"
alias vim="nvim"
alias e="vim"
#alias sudo="doas"

run () {
    nix run nixpkgs#$@
}
nix-i () {
    nix-env -i $@
}

alias ls="exa --icons"
alias t="tmux"
alias grep="grep --color=auto"
alias windows="sudo grub-reboot 2 && sudo reboot"
alias hm="home-manager --flake $NIXFLAKE#$FLAKENAME"
#alias emacs="emacsclient -t "
#export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"
export EDITOR="~/.nix-profile/bin/nvim"
export editor=$EDITOR
export BROWSER=firefox
source ~/.cargo/env
export GTK2_RC_FILES=$HOME/.gtkrc-2.0
eval "$(direnv hook zsh)"
#export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
fetch -s
eval "$(starship init zsh)"
