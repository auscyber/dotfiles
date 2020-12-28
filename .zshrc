
# }}} End configuration added by Zim install
autoload -U compinit promptinit
compinit
promptinit; #prompt gentoo


source ~/antigen.zsh

antigen use oh-my-zsh
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


alias vim="nvim"
alias e="vim"
#alias sudo="doas"
alias t="tmux"
alias windows="sudo grub-reboot 2 --boot-directory=/efi && sudo reboot"
alias emacs="emacsclient -c -s /tmp/emacs1000/server"
eval "$(starship init zsh)"
export BROWSER=chromium
~/pfetch
