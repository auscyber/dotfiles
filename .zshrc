#!/usr/bin/zsh
#zmodload zpropf
#source ~/.bashrc
unset __HM_SESS_VARS_SOURCED


has () {
    command -v $1 2>&- 1>&-
}
eval_if_installed () {
    has $1 && eval "$($@)"
}

source_if_exists () {
    if [[ -e "$1" ]]
    then
        if [[ -n "$2" ]]
        then
            shift
            source $@
        else
            source "$1"
        fi
    fi
}

source_if_exists ~/.nix-profile/etc/profile.d/hm-session-vars.sh
source_if_exists ~/.config/op/plugins.sh
source_if_exists ~/.nix-profile/etc/profile.d/nix.sh
source_if_exists ~/.cargo/env


#zmodload zsh/zprof
#source /etc/profile
NPM_PACKAGES="${HOME}/.npm-packages"
export DENO_INSTALL="/home/auscyber/.deno"

#export IDRIS2_CG=racket
#IDRIS_PREFIX=$HOME/.idris2
export PATH=$PATH:~/.cabal/bin:~/go/bin:~/.emacs.d/bin:/home/auscyber/.local/bin:~/.dotnet/tools:/usr/sbin:/snap/bin:$NPM_PACKAGES/bin:~/.luarocks/bin:/usr/local/go/bin:$DENO_INSTALL/bin:/opt/jdk8u292-b10:$IDRIS_PREFIX/bin

test -r /home/auscyber/.opam/opam-init/init.zsh && . /home/auscyber/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$IDRIS_PREFIX/lib
if  has sccache ;
    then
        RUSTC_WRAPPER=`which sccache`
fi
fpath=(~/.zsh $fpath)
#promptinit; #prompt gentoo
export _JAVA_AWT_WM_NONREPARENTING=1
export WLR_NO_HARDWARE_CURSORS=1
#exec fish
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

eval_if_installed idris2 --bash-completion-script idris2
eval_if_installed stack --bash-completion-script stack
eval_if_installed dots completion --shell bash
eval_if_installed direnv hook zsh
eval_if_installed op completion zsh
#eval_if_installed lorri direnv


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

#run_if_installed exa () { alias ls="exa --icons --git"}
alias ng="nvim -c ':Neogit'"
alias ls="exa --icons --git"
alias ll="ls -la"
alias t="tmux"
alias grep="grep --color=auto"
alias windows="sudo grub-reboot 2 && sudo reboot"
alias hm="home-manager --flake $NIXFLAKE#$FLAKENAME"
#nvim () {
# nix run "$NIXFLAKE#nvim" -- $@
#}

#alias emacs="emacsclient -t "
#export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"
export EDITOR="nvim"
export editor=$EDITOR
export BROWSER=firefox
export GTK2_RC_FILES=$HOME/.gtkrc-2.0
#export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
fetch -s
eval "$(starship init zsh)"
