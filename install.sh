#!/bin/sh
set -e
#. /etc/os-release

rust_packages=(ripgrep starship)

install_haskell() {
    curl -sSL https://get.haskellstack.org/ | sh
}

install_nix () {
    curl -L https://nixos.org/nix/install | sh
}

install_rust() {
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
}


usage () {
    cat <<EOF
usage: $0 [package|options]

OPTIONS:
    -v: verbose
    -h: help

PACKAGES:
    rust: install rust
    haskell: install haskell stack
    nix: install nix
        --system [system]: install home-manager system

EOF
}


install_nix_packages () {
    cargo install $(rust_packages)
}

#cargo install starship --force
if [ -z "$1"]; then
    usage
fi
while [ -n "$1" ]; do
case $1 in
    -v)
        verbose=""
        ;;
    -h)
        usage
        exit 0
        ;;
    haskell)
        [[ -z "${verbose+x}" ]] || printf 'Installing Haskell Stack\n'
        install_haskell;;
    nix)
        printf 'Installing Nix\n'
        install_nix
        case $2 in
            --system)

            esac
        ;;
    rust)
        printf 'Installing Rust\n'
        install_rust;;
    *)
        usage
        ;;
esac
shift
done

