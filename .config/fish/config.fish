set fish_greeting
starship init fish | source
alias hm="home-manager"
alias vim="nvim"
alias e="vim"
alias t="tmux"
alias emacs="emacsclient -t"
export EDITOR="vim"
function ghc_env
    nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ $argv ])"
end
alias windows="sudo grub-reboot 2 --boot-directory=/efi && sudo reboot"
alias emacs="emacsclient -t "

