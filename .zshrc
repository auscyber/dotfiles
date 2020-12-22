autoload -U compinit promptinit
compinit
#promptinit; prompt gentoo


source ~/antigen.zsh


antigen use oh-my-zsh

antigen bundle git
#antigen bundle Aloxaf/fzf-tab
antigen bundle zsh-users/zsh-autosuggestions
antigen apply

export PATH=$PATH:~/.local/bin:~/.cargo/bin:~/.cabal/bin:~/go/bin:~/.emacs.d/bin
# Aliases 

function set_win_title(){
    echo -ne "\033]0; Alacritty: $(basename $PWD) \007"
}
precmd_functions+=(set_win_title)

#alias vim="nvim"
#alias e="vim"
#alias sudo="doas"
#alias t="tmux"
alias windows="sudo grub-reboot 2 --boot-directory=/efi && sudo reboot"
alias emacs="emacsclient -c -s /tmp/emacs1000/server"





eval "$(starship init zsh)"
export BROWSER=chromium
~/pfetch
[ -f "/home/auscyber/.ghcup/env" ] && source "/home/auscyber/.ghcup/env" # ghcup-env
