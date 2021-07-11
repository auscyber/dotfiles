#source /etc/profile
NPM_PACKAGES="${HOME}/.npm-packages"
export DENO_INSTALL="/home/auscyber/.deno"
export PATH=$PATH:~/.cabal/bin:~/go/bin:~/.emacs.d/bin:/home/auscyber/.local/bin:~/.dotnet/tools:/usr/sbin:/snap/bin:$NPM_PACKAGES/bin:~/.luarocks/bin:/usr/local/go/bin:${DENO_INSTALL}/bin:/opt/jdk8u292-b10
# Aliases 
fpath=(~/.zsh $fpath)
#promptinit; #prompt gentoo
export _JAVA_AWT_WM_NONREPARENTING=1
ZSH_CUSTOM=$HOME/.zsh-plugins
source ~/antigen.zsh

antigen use oh-my-zsh
antigen bundle git@github.com:spwhitt/nix-zsh-completions.git
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle chisui/zsh-nix-shell
antigen apply



function ghc_env(){
    nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; $1)"
}


if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

#alias ghc="stack exec -- ghc"
#alias ghci="stack exec -- ghci"
alias fzf="fzf --reverse --height 40%"
alias vim="nvim"
alias e="vim"
#alias sudo="doas"
alias t="tmux"
alias hm=home-manager
alias grep="grep --color=auto"
alias windows="sudo grub-reboot 2 && sudo reboot"
#alias emacs="emacsclient -t "
eval "$(starship init zsh)"
#export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"
export EDITOR=vim
export editor=$EDITOR
export BROWSER=firefox
source ~/.cargo/env
# test -r /home/auscyber/.opam/opam-init/init.zsh && . /home/auscyber/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fetch -s
export GTK2_RC_FILES=$HOME/.gtkrc-2.0
#alias luamake=/home/auscyber/lua-language-server/3rd/luamake/luamake
eval "$(direnv hook zsh)"
